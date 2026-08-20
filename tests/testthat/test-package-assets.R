test_that("the packaged tutorial is discoverable", {
  tutorials <- learnr::available_tutorials("drillr")
  expect_true("drills" %in% tutorials$name)
})

test_that("runtime question pool and manifest ship together", {
  tutorial <- system.file("tutorials", "drills", package = "drillr")
  expect_true(file.exists(file.path(tutorial, "runtime_question_pool.Rmd")))
  expect_true(file.exists(file.path(tutorial, "question_manifest.csv")))
})

test_that("student package does not ship canonical question-bank sources", {
  root <- system.file(package = "drillr")
  expect_false(dir.exists(file.path(root, "question-bank")))
})
