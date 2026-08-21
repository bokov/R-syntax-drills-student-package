test_that("the packaged tutorial is discoverable", {
  tutorials <- learnr::available_tutorials("drillr")
  expect_true("drills" %in% tutorials$name)
})

test_that("frozen runtime question pool and manifest ship as a valid pair", {
  tutorial <- system.file("tutorials", "drills", package = "drillr")
  expect_true(file.exists(file.path(tutorial, "runtime_question_pool.Rmd")))
  expect_true(file.exists(file.path(tutorial, "question_manifest.csv")))

  bank <- drillr:::drillr_bundled_bank()
  expect_match(bank$bank_version, "^md5-[0-9a-f]{32}$")
})

test_that("stable tutorial shell selects its question pool dynamically", {
  tutorial <- system.file("tutorials", "drills", package = "drillr")
  rmd <- readLines(file.path(tutorial, "drills.Rmd"), warn = FALSE)
  expect_true(any(grepl("child=DRILLR_LAUNCH_BANK\\$pool_path", rmd)))
  expect_true(any(grepl("DRILLR_RENDERED_BANK <- DRILLR_LAUNCH_BANK", rmd, fixed = TRUE)))
})

test_that("checker support comes from the runtime pool rather than a duplicate file", {
  tutorial <- system.file("tutorials", "drills", package = "drillr")
  expect_true(file.exists(file.path(tutorial, "R", "syntax_checkers.R")))

  shell <- paste(
    readLines(file.path(tutorial, "drills.Rmd"), warn = FALSE),
    collapse = "\n"
  )
  expect_true(grepl('source("R/syntax_checkers.R")', shell, fixed = TRUE))

  pool <- paste(
    readLines(file.path(tutorial, "runtime_question_pool.Rmd"), warn = FALSE),
    collapse = "\n"
  )
})

test_that("tutorial exposes a lazy progress page", {
  tutorial <- system.file("tutorials", "drills", package = "drillr")
  shell <- paste(
    readLines(file.path(tutorial, "drills.Rmd"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(shell, "## Your progress", fixed = TRUE)
  expect_match(shell, "register_progress_handlers()", fixed = TRUE)
  expect_match(shell, "tableOutput(\"progress_table\")", fixed = TRUE)
  expect_match(shell, "Estimated recall is a scheduling estimate, not a grade.", fixed = TRUE)
})

test_that("student package does not ship canonical question-bank sources", {
  root <- system.file(package = "drillr")
  expect_false(dir.exists(file.path(root, "question-bank")))
})
