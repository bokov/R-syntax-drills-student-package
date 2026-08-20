test_that("production service defaults match the current drill service", {
  config <- drillr:::drillr_runtime_config()

  expect_identical(config$course_id, "R Syntax Drill")
  expect_identical(config$queue_size, 10L)
  expect_identical(
    config$topic_priority,
    c("vector_creation", "vector_indexing")
  )
  expect_match(config$webhook_url, "^https://script\\.google\\.com/macros/s/.+/exec$")
})

test_that("webhook URL can be overridden for development", {
  old <- options(drillr.webhook_url = "https://example.invalid/exec")
  on.exit(options(old), add = TRUE)

  expect_identical(
    drillr:::drillr_runtime_config()$webhook_url,
    "https://example.invalid/exec"
  )
})
