#' Load tutorial client helpers into an isolated test environment
#'
#' Recreates the subset of the learnr runtime needed to test assignment payload
#' reconciliation and queued-item usability without launching a tutorial.
#'
#' @return An environment containing `question_manifest.R`,
#'   `assignment_storage.R`, and `logging.R` helpers plus a minimal `APP_CONFIG`.
#' @details Used by both tests in this file. It depends on the installed tutorial
#'   copies of those three helper files and has no callers outside this test file.
load_tutorial_client <- function() {
  tutorial <- system.file("tutorials", "drills", package = "drillr")
  env <- new.env(parent = globalenv())
  env$APP_CONFIG <- list(
    course_id = "course",
    queue_size = 10L,
    topic_priority = c("vectors"),
    webhook_url = "https://example.invalid",
    manifest_path = ""
  )
  sys.source(file.path(tutorial, "R", "question_manifest.R"), envir = env)
  sys.source(file.path(tutorial, "R", "assignment_storage.R"), envir = env)
  sys.source(file.path(tutorial, "R", "logging.R"), envir = env)
  env
}

test_that("assignment requests reconcile against available item labels", {
  env <- load_tutorial_client()
  manifest <- data.frame(
    item_label = c("q1", "q2", "note"),
    event = c("exercise_result", "exercise_result", "question_submission"),
    topic = c("vectors", "vectors", "vectors"),
    points = c(1, 1, 0),
    starter_question = c(FALSE, FALSE, FALSE),
    release = c(1L, 1L, 1L),
    stringsAsFactors = FALSE
  )

  payload <- env$assignment_service_payload(
    "get_or_create_active_assignments",
    "student",
    env$APP_CONFIG,
    manifest
  )

  expect_true(payload$reconcile_bank)
  expect_identical(payload$available_item_labels, c("q1", "q2"))
  expect_identical(payload$queue_size, 10L)
  expect_identical(payload$topic_priority, "vectors")
})

test_that("queued graded events are retained only while their item label is usable", {
  env <- load_tutorial_client()
  manifest <- data.frame(
    item_label = "q1",
    event = "exercise_result",
    topic = "vectors",
    points = 1,
    starter_question = FALSE,
    release = 1L,
    stringsAsFactors = FALSE
  )

  usable <- list(event = "exercise_result", item_label = "q1")
  removed <- list(event = "exercise_result", item_label = "q2")
  identity <- list(event = "identity_saved", item_label = "identity")

  expect_true(env$outbox_payload_is_usable(usable, manifest))
  expect_false(env$outbox_payload_is_usable(removed, manifest))
  expect_true(env$outbox_payload_is_usable(identity, manifest))
})
