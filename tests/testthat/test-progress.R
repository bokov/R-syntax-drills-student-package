load_progress_helpers <- function() {
  tutorial <- system.file("tutorials", "drills", package = "drillr")
  env <- new.env(parent = globalenv())
  sys.source(file.path(tutorial, "R", "progress.R"), envir = env)
  env
}

test_that("progress responses preserve scheduler summary fields", {
  env <- load_progress_helpers()
  body <- list(progress = list(
    list(
      topic = "vector_creation",
      observations = 18,
      recent_count = 10,
      recent_correct = 9,
      recent_accuracy = 0.9,
      estimated_recall = 0.94,
      mastered = TRUE
    ),
    list(
      topic = "dataframe_indexing",
      observations = 0,
      recent_count = 0,
      recent_correct = 0,
      recent_accuracy = NULL,
      estimated_recall = NULL,
      mastered = FALSE
    )
  ))

  rows <- env$progress_response_table(body)
  expect_identical(rows$topic, c("vector_creation", "dataframe_indexing"))
  expect_identical(rows$observations, c(18L, 0L))
  expect_equal(rows$recent_accuracy[[1]], 0.9)
  expect_true(is.na(rows$recent_accuracy[[2]]))
  expect_equal(rows$estimated_recall[[1]], 0.94)
  expect_true(is.na(rows$estimated_recall[[2]]))
  expect_identical(rows$mastered, c(TRUE, FALSE))
})

test_that("progress table is compact and student-facing", {
  env <- load_progress_helpers()
  rows <- data.frame(
    topic = c("vector_creation", "dataframe_indexing"),
    observations = c(18L, 0L),
    recent_count = c(10L, 0L),
    recent_correct = c(9L, 0L),
    recent_accuracy = c(0.9, NA_real_),
    estimated_recall = c(0.94, NA_real_),
    mastered = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  out <- env$format_progress_table(rows)
  expect_identical(
    names(out),
    c(
      "Topic",
      "Recent first-try accuracy",
      "Questions practiced",
      "Estimated recall"
    )
  )
  expect_identical(out$Topic, c("Vector Creation", "Data Frame Indexing"))
  expect_identical(out[["Recent first-try accuracy"]], c("9/10 (90%)", "—"))
  expect_identical(out[["Questions practiced"]], c(18L, 0L))
  expect_identical(out[["Estimated recall"]], c("94%", "—"))
})

test_that("missing progress data remains distinguishable from an empty summary", {
  env <- load_progress_helpers()
  expect_null(env$progress_response_table(list()))
  expect_identical(
    names(env$progress_response_table(list(progress = list()))),
    env$PROGRESS_COLUMNS
  )
})
