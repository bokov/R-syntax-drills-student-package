drillr_identity_path <- function() {
  override <- getOption("drillr.identity_path", NULL)
  if (!is.null(override) && nzchar(override)) return(override)

  file.path(tools::R_user_dir("drillr", "config"), "identity.rds")
}

drillr_empty_identity <- function() {
  list(student_id = "", student_name = "")
}

drillr_read_identity <- function() {
  path <- drillr_identity_path()
  if (!file.exists(path)) return(drillr_empty_identity())

  value <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(value)) return(drillr_empty_identity())

  student_id <- trimws(as.character(value$student_id %||% "")[[1]])
  student_name <- trimws(as.character(value$student_name %||% "")[[1]])

  list(student_id = student_id, student_name = student_name)
}

drillr_save_identity <- function(student_id, student_name = "") {
  student_id <- trimws(as.character(student_id)[[1]])
  student_name <- trimws(as.character(student_name %||% "")[[1]])

  if (!grepl("^[A-Za-z0-9._@-]{2,100}$", student_id)) {
    stop("student_id has an invalid format.")
  }

  path <- drillr_identity_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(
    list(student_id = student_id, student_name = student_name),
    path,
    version = 2
  )
  invisible(path)
}

drillr_forget_identity <- function() {
  path <- drillr_identity_path()
  if (file.exists(path)) unlink(path)
  invisible(path)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
