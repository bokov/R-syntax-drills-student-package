test_that("the packaged tutorial is discoverable", {
  tutorials <- learnr::available_tutorials("drillr")
  expect_true("drills" %in% tutorials$name)
})

test_that("bundled runtime question pool and manifest ship as a usable pair", {
  tutorial <- system.file("tutorials", "drills", package = "drillr")
  expect_true(file.exists(file.path(tutorial, "runtime_question_pool.Rmd")))
  expect_true(file.exists(file.path(tutorial, "question_manifest.csv")))

  bank <- drillr:::drillr_bundled_bank()
  expect_gt(length(bank$mismatch$usable), 0)
})

test_that("stable tutorial shell selects its question pool dynamically", {
  tutorial <- system.file("tutorials", "drills", package = "drillr")
  rmd <- readLines(file.path(tutorial, "drills.Rmd"), warn = FALSE)
  expect_true(any(grepl("child=DRILLR_LAUNCH_BANK\\$pool_path", rmd)))
  expect_false(any(grepl("DRILLR_RENDERED_BANK", rmd, fixed = TRUE)))
  expect_true(any(grepl(
    "fallback_manifest <- DRILLR_LAUNCH_BANK$manifest",
    rmd,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "DRILLR_SESSION_BANK <- DRILLR_LAUNCH_BANK",
    rmd,
    fixed = TRUE
  )))
})

test_that("checker support comes from the runtime pool rather than a duplicate file", {
  tutorial <- system.file("tutorials", "drills", package = "drillr")
  expect_true(file.exists(file.path(tutorial, "R", "syntax_checkers.R")))

  shell <- paste(
    readLines(file.path(tutorial, "drills.Rmd"), warn = FALSE),
    collapse = "\n"
  )
  expect_true(grepl('source("R/syntax_checkers.R")', shell, fixed = TRUE))
})

test_that("tutorial exposes bank warnings and a lazy progress page", {
  tutorial <- system.file("tutorials", "drills", package = "drillr")
  shell <- paste(
    readLines(file.path(tutorial, "drills.Rmd"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(shell, 'uiOutput("bank_status")', fixed = TRUE)
  expect_match(shell, "register_progress_handlers()", fixed = TRUE)
  expect_match(shell, "tableOutput(\"progress_table\")", fixed = TRUE)
})
