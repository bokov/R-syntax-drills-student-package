load_tutorial_client <- function() {
  tutorial <- system.file("tutorials", "drills", package = "drillr")
  env <- new.env(parent = globalenv())
  env$APP_CONFIG <- list(
    course_id = "course",
    queue_size = 10L,
    topic_priority = c("vector_creation", "vector_indexing"),
    webhook_url = "https://example.invalid",
    bank_version = "md5-current",
    runtime_support_hash = "md5-support",
    package_version = "0.1.0.9002",
    manifest_path = ""
  )
  sys.source(file.path(tutorial, "R", "question_manifest.R"), envir = env)
  sys.source(file.path(tutorial, "R", "assignment_storage.R"), envir = env)
  sys.source(file.path(tutorial, "R", "progress.R"), envir = env)
  sys.source(file.path(tutorial, "R", "logging.R"), envir = env)
  env
}

test_that("assignment requests opt into the bank handshake", {
  env <- load_tutorial_client()
  payload <- env$assignment_service_payload(
    "get_or_create_active_assignments",
    "student",
    env$APP_CONFIG
  )
  expect_identical(payload$bank_version, "md5-current")
  expect_identical(payload$runtime_support_hash, "md5-support")
  expect_identical(payload$package_version, "0.1.0.9002")

  legacy <- env$APP_CONFIG
  legacy$bank_version <- ""
  payload <- env$assignment_service_payload(
    "get_active_assignments",
    "student",
    legacy
  )
  expect_null(payload$bank_version)
  expect_null(payload$runtime_support_hash)
  expect_null(payload$package_version)
})

test_that("progress requests use the read-only assignment path", {
  env <- load_tutorial_client()
  payload <- env$progress_request_payload("student", env$APP_CONFIG)

  expect_identical(payload$request_type, "get_active_assignments")
  expect_true(payload$include_progress)
  expect_identical(
    payload$topic_priority,
    c("vector_creation", "vector_indexing")
  )
  expect_identical(payload$bank_version, "md5-current")
  expect_identical(payload$runtime_support_hash, "md5-support")
})

test_that("queued graded events migrate only across compatible banks", {
  env <- load_tutorial_client()
  manifest <- data.frame(
    item_label = "q1",
    event = "exercise_result",
    topic = "vectors",
    points = 1,
    starter_question = FALSE,
    question_hash = "same-hash",
    stringsAsFactors = FALSE
  )
  record <- list(
    payload = list(
      request_id = "stable-request",
      item_label = "q1",
      bank_version = "md5-old",
      runtime_support_hash = "md5-support",
      package_version = "old-package"
    ),
    question_hash = "same-hash"
  )

  migrated <- env$migrate_outbox_record(record, manifest, env$APP_CONFIG)
  expect_false(migrated$drop)
  expect_true(migrated$changed)
  expect_identical(migrated$payload$request_id, "stable-request")
  expect_identical(migrated$payload$bank_version, "md5-current")
  expect_identical(migrated$payload$runtime_support_hash, "md5-support")

  changed_question <- manifest
  changed_question$question_hash <- "new-hash"
  expect_true(
    env$migrate_outbox_record(record, changed_question, env$APP_CONFIG)$drop
  )

  changed_support <- record
  changed_support$payload$runtime_support_hash <- "md5-old-support"
  expect_true(
    env$migrate_outbox_record(changed_support, manifest, env$APP_CONFIG)$drop
  )

  legacy_support <- record
  legacy_support$payload$runtime_support_hash <- NULL
  expect_true(
    env$migrate_outbox_record(legacy_support, manifest, env$APP_CONFIG)$drop
  )

  removed <- manifest[FALSE, , drop = FALSE]
  expect_true(env$migrate_outbox_record(record, removed, env$APP_CONFIG)$drop)
})
