test_that("saved identity round-trips and can be forgotten", {
  path <- tempfile(fileext = ".rds")
  old <- options(drillr.identity_path = path)
  on.exit(options(old), add = TRUE)

  expect_equal(
    drillr:::drillr_read_identity(),
    list(student_id = "", student_name = "")
  )

  drillr:::drillr_save_identity("abc123", "Ada Lovelace")
  expect_equal(
    drillr:::drillr_read_identity(),
    list(student_id = "abc123", student_name = "Ada Lovelace")
  )

  drillr:::drillr_forget_identity()
  expect_false(file.exists(path))
})
