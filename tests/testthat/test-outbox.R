test_that("outbox persists payloads and filters by student", {
  dir <- tempfile("outbox-")
  dir.create(dir)
  payload_a <- list(
    request_id = "req-a",
    course_id = "course",
    student_id = "student-a",
    event = "exercise_result"
  )
  payload_b <- list(
    request_id = "req-b",
    course_id = "course",
    student_id = "student-b",
    event = "exercise_result"
  )

  drillr:::drillr_outbox_enqueue(payload_a, "hash-a", dir)
  drillr:::drillr_outbox_enqueue(payload_b, "hash-b", dir)

  entries <- drillr:::drillr_outbox_entries("student-a", "course", dir)
  expect_length(entries, 1)
  expect_identical(entries[[1]]$payload$request_id, "req-a")
  expect_identical(entries[[1]]$question_hash, "hash-a")

  drillr:::drillr_outbox_remove(entries[[1]])
  expect_length(drillr:::drillr_outbox_entries("student-a", "course", dir), 0)
  expect_length(drillr:::drillr_outbox_entries("student-b", "course", dir), 1)
})

test_that("outbox replacement retains request id and queue time", {
  dir <- tempfile("outbox-")
  dir.create(dir)
  payload <- list(
    request_id = "stable-request",
    course_id = "course",
    student_id = "student",
    bank_version = "md5-old"
  )
  drillr:::drillr_outbox_enqueue(payload, "hash", dir)
  record <- drillr:::drillr_outbox_entries("student", "course", dir)[[1]]

  replacement <- payload
  replacement$bank_version <- "md5-new"
  drillr:::drillr_outbox_replace(record, replacement)
  updated <- drillr:::drillr_outbox_entries("student", "course", dir)[[1]]

  expect_identical(updated$payload$request_id, "stable-request")
  expect_identical(updated$payload$bank_version, "md5-new")
  expect_identical(updated$queued_at_utc, record$queued_at_utc)

  bad <- replacement
  bad$request_id <- "different-request"
  expect_error(
    drillr:::drillr_outbox_replace(updated, bad),
    "retain the original request_id"
  )
})
