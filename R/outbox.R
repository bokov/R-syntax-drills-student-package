drillr_outbox_dir <- function(create = TRUE) {
  path <- file.path(tools::R_user_dir("drillr", "data"), "outbox")
  if (isTRUE(create) && !dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

drillr_outbox_filename <- function(request_id) {
  request_id <- as.character(request_id)[[1]]
  if (!nzchar(request_id)) stop("Outbox payload requires a request_id.")
  paste0(gsub("[^A-Za-z0-9._-]", "_", request_id), ".rds")
}

drillr_outbox_write_record <- function(record, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("outbox-", tmpdir = dirname(path), fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(record, tmp)
  if (file.exists(path)) unlink(path)
  if (!file.rename(tmp, path)) stop("Could not save the Drillr logging outbox entry.")
  invisible(path)
}

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

drillr_outbox_remove <- function(record) {
  path <- if (is.list(record)) record$path else as.character(record)[[1]]
  if (!is.null(path) && nzchar(path) && file.exists(path)) unlink(path)
  invisible(TRUE)
}

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

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
