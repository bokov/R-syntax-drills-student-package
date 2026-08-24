test_that("webhook URL can be overridden for development", {
  old <- options(drillr.webhook_url = "https://example.invalid/exec")
  on.exit(options(old), add = TRUE)

  expect_identical(
    drillr:::drillr_runtime_config()$webhook_url,
    "https://example.invalid/exec"
  )
})
