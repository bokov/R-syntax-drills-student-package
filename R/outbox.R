# Logging outbox paths and persistence ----------------------------------------

#' Locate the local logging outbox directory
#'
#' Returns the per-user directory where Drillr temporarily stores logging
#' payloads that could not be delivered to the assignment service, optionally
#' creating that directory on demand.
#'
#' @param create If `TRUE`, create the directory recursively when it does not
#'   already exist.
#' @return A length-one character path to the logging outbox directory.
#' @details Called directly by `drillr_outbox_enqueue()` and
#'   `drillr_outbox_entries()` through their default arguments. It has no
#'   within-repo function dependencies.
drillr_outbox_dir <- function(create = TRUE) {
  path <- file.path(tools::R_user_dir("drillr", "data"), "outbox")
  if (isTRUE(create) && !dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

#' Convert a request ID to an outbox filename
#'
#' Sanitizes a logging request ID into a filesystem-safe RDS filename so each
#' queued request has a stable local storage location.
#'
#' @param request_id Non-empty request identifier from a logging payload.
#' @return A length-one character filename ending in `.rds`.
#' @details Called directly by `drillr_outbox_enqueue()`. It has no within-repo
#'   function dependencies.
drillr_outbox_filename <- function(request_id) {
  request_id <- as.character(request_id)[[1]]
  if (!nzchar(request_id)) stop("Outbox payload requires a request_id.")
  paste0(gsub("[^A-Za-z0-9._-]", "_", request_id), ".rds")
}

#' Atomically write an outbox record
#'
#' Saves a complete outbox record to a temporary RDS file in the destination
#' directory and renames it into place, avoiding a partially written target.
#'
#' @param record Outbox record to serialize, normally containing `payload` and
#'   `queued_at_utc` elements.
#' @param path Destination RDS path.
#' @return Invisibly, `path` after the record has been moved into place.
#' @details Called directly by `drillr_outbox_enqueue()` and
#'   `drillr_outbox_replace()`. It has no within-repo function dependencies.
drillr_outbox_write_record <- function(record, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("outbox-", tmpdir = dirname(path), fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(record, tmp)
  if (file.exists(path)) unlink(path)
  if (!file.rename(tmp, path)) stop("Could not save the Drillr logging outbox entry.")
  invisible(path)
}

# Outbox queue operations -----------------------------------------------------

#' Queue a logging payload for later delivery
#'
#' Wraps an undelivered payload with its queue timestamp and persists it under
#' a filename derived from the payload's request ID.
#'
#' @param payload Logging payload list containing a non-empty `request_id`.
#' @param dir Outbox directory in which to store the record.
#' @return Invisibly, the path returned by `drillr_outbox_write_record()`.
#' @details Called by tutorial helper `queue_log_payload()` in
#'   `inst/tutorials/drills/R/logging.R` whenever transport fails or an earlier
#'   queued request blocks delivery, and directly by `test-outbox.R`. Depends on
#'   `drillr_outbox_dir()`, `drillr_outbox_filename()`, and
#'   `drillr_outbox_write_record()`.
drillr_outbox_enqueue <- function(payload, dir = drillr_outbox_dir()) {
  if (is.null(payload$request_id) || !nzchar(as.character(payload$request_id))) {
    stop("Outbox payload requires a request_id.")
  }
  target <- file.path(dir, drillr_outbox_filename(payload$request_id))
  record <- list(
    payload = payload,
    queued_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  )
  drillr_outbox_write_record(record, target)
}

#' Read queued outbox records
#'
#' Loads usable RDS records from the outbox, optionally filters them by student
#' and course, and orders them by their original queue timestamps for retry.
#'
#' @param student_id Optional student identifier used to retain matching
#'   payloads only.
#' @param course_id Optional course identifier used to retain matching payloads
#'   only.
#' @param dir Outbox directory to inspect. The default does not create a missing
#'   directory.
#' @return A list of valid outbox records, each augmented with its source `path`,
#'   ordered by `queued_at_utc`; an empty list when none are available.
#' @details Called by tutorial helper `flush_log_outbox()` in
#'   `inst/tutorials/drills/R/logging.R` and directly by `test-outbox.R`.
#'   Depends on `drillr_outbox_dir()` and `%||%`.
drillr_outbox_entries <- function(
  student_id = NULL,
  course_id = NULL,
  dir = drillr_outbox_dir(create = FALSE)
) {
  if (!dir.exists(dir)) return(list())
  paths <- list.files(dir, pattern = "[.]rds$", full.names = TRUE)
  if (!length(paths)) return(list())

  records <- lapply(paths, function(path) {
    record <- tryCatch(readRDS(path), error = function(e) NULL)
    if (is.null(record) || is.null(record$payload)) return(NULL)
    record$path <- path
    record
  })
  records <- Filter(Negate(is.null), records)

  if (!is.null(student_id)) {
    key <- as.character(student_id)[[1]]
    records <- Filter(
      function(x) identical(as.character(x$payload$student_id), key),
      records
    )
  }
  if (!is.null(course_id)) {
    key <- as.character(course_id)[[1]]
    records <- Filter(
      function(x) identical(as.character(x$payload$course_id), key),
      records
    )
  }

  if (length(records)) {
    ord <- order(vapply(records, function(x) x$queued_at_utc %||% "", character(1)))
    records <- records[ord]
  }
  records
}

#' Remove a delivered or obsolete outbox record
#'
#' Deletes the local RDS file associated with an outbox record or explicit path
#' after the queued payload no longer needs to be retained.
#'
#' @param record Either an outbox record containing `path`, or a character path
#'   to an outbox file.
#' @return Invisibly, `TRUE`.
#' @details Called by tutorial helper `flush_log_outbox()` after a queued payload
#'   is successfully delivered or found obsolete, and directly by
#'   `test-outbox.R`. It has no within-repo function dependencies.
drillr_outbox_remove <- function(record) {
  path <- if (is.list(record)) record$path else as.character(record)[[1]]
  if (!is.null(path) && nzchar(path) && file.exists(path)) unlink(path)
  invisible(TRUE)
}

#' Replace the payload stored in an existing outbox record
#'
#' Rewrites a queued record while preserving its original request ID and queue
#' timestamp. This supports retry-safe mutation without making the replacement
#' look like a newly queued request.
#'
#' @param record Existing outbox record containing `path`, `payload`, and
#'   `queued_at_utc` elements.
#' @param payload Replacement payload. Its `request_id` must be identical to the
#'   original payload's request ID.
#' @return Invisibly, the record path returned by
#'   `drillr_outbox_write_record()`.
#' @details No production code currently calls this function. It is exercised
#'   directly by `tests/testthat/test-outbox.R`. Depends on `%||%` and
#'   `drillr_outbox_write_record()`.
drillr_outbox_replace <- function(record, payload) {
  path <- record$path
  if (is.null(path) || !nzchar(path)) stop("Outbox record has no path.")
  original_id <- as.character(record$payload$request_id %||% "")
  replacement_id <- as.character(payload$request_id %||% "")
  if (!identical(original_id, replacement_id)) {
    stop("Outbox retries must retain the original request_id.")
  }
  replacement <- list(
    payload = payload,
    queued_at_utc = as.character(record$queued_at_utc %||% "")
  )
  drillr_outbox_write_record(replacement, path)
}

# Utility helpers -------------------------------------------------------------

#' Substitute a fallback for a null or empty value
#'
#' Provides the package-internal null-coalescing operation used while reading
#' optional outbox fields. Equivalent definitions also appear in other package
#' source files, so the final namespace binding is intentionally interchangeable
#' with those copies.
#'
#' @param x Value to return unless it is `NULL` or length zero.
#' @param y Fallback value.
#' @return `y` when `x` is `NULL` or empty; otherwise `x`.
#' @details Used directly by `drillr_outbox_entries()` and
#'   `drillr_outbox_replace()` in this file.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
