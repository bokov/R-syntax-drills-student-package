test_that("the packaged tutorial is discoverable", {
  tutorials <- learnr::available_tutorials("drillr")
  expect_true("drills" %in% tutorials$name)
})

test_that("runtime question pool and manifest ship together", {
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

test_that("student package does not ship canonical question-bank sources", {
  root <- system.file(package = "drillr")
  expect_false(dir.exists(file.path(root, "question-bank")))
})
