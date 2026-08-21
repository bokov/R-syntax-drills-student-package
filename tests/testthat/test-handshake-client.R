load_tutorial_client <- function() {
  tutorial <- system.file("tutorials", "drills", package = "drillr")
  env <- new.env(parent = globalenv())
  env$APP_CONFIG <- list(
    course_id = "course",
    queue_size = 10L,
    topic_priority = c("vectors"),
    webhook_url = "https://example.invalid",
    bank_version = "md5-current",
    package_version = "0.1.0.9001",
    manifest_path = ""
  )
  sys.source(file.path(tutorial, "R", "question_manifest.R"), envir = env)
  sys.source(file.path(tutorial, "R", "assignment_storage.R"), envir = env)
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
  expect_identical(payload$package_version, "0.1.0.9001")

  legacy <- env$APP_CONFIG
  legacy$bank_version <- ""
  payload <- env$assignment_service_payload(
    "get_active_assignments",
    "student",
    legacy
  )
  expect_null(payload$bank_version)
  expect_null(payload$package_version)
})

test_that("queued graded events migrate only when their question is unchanged", {
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
      package_version = "old-package"
    ),
    question_hash = "same-hash"
  )

  migrated <- env$migrate_outbox_record(record, manifest, env$APP_CONFIG)
  expect_false(migrated$drop)
  expect_true(migrated$changed)
  expect_identical(migrated$payload$request_id, "stable-request")
  expect_identical(migrated$payload$bank_version, "md5-current")

  changed <- manifest
  changed$question_hash <- "new-hash"
  expect_true(env$migrate_outbox_record(record, changed, env$APP_CONFIG)$drop)

  removed <- manifest[FALSE, , drop = FALSE]
  expect_true(env$migrate_outbox_record(record, removed, env$APP_CONFIG)$drop)
})
