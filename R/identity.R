# Identity persistence --------------------------------------------------------

#' Locate the saved student identity file
#'
#' Returns the path where Drillr stores the most recently saved student ID and
#' optional name. An explicit `drillr.identity_path` option overrides the normal
#' per-user configuration path, primarily so tests can isolate their state.
#'
#' @return A length-one character path to the identity RDS file.
#' @details Called directly by `drillr_read_identity()`,
#'   `drillr_save_identity()`, and `drillr_forget_identity()`. It has no
#'   within-repo function dependencies.
drillr_identity_path <- function() {
  override <- getOption("drillr.identity_path", NULL)
  if (!is.null(override) && nzchar(override)) return(override)

  file.path(tools::R_user_dir("drillr", "config"), "identity.rds")
}

#' Create a blank student identity
#'
#' Supplies the canonical empty identity used when no saved identity exists or
#' when a saved RDS file cannot be read as the expected list.
#'
#' @return A list with empty-string `student_id` and `student_name` elements.
#' @details Called directly by `drillr_read_identity()`. It has no within-repo
#'   function dependencies.
drillr_empty_identity <- function() {
  list(student_id = "", student_name = "")
}

#' Read the saved student identity
#'
#' Loads the persisted identity used to pre-fill the tutorial's identity form,
#' returning a blank identity when no usable saved value is available.
#'
#' @return A list with normalized `student_id` and `student_name` character
#'   values.
#' @details Called directly from the `identity-ui` chunk in
#'   `inst/tutorials/drills/drills.Rmd` and by `test-identity.R`. Depends on
#'   `drillr_identity_path()`, `drillr_empty_identity()`, and `%||%`.
drillr_read_identity <- function() {
  path <- drillr_identity_path()
  if (!file.exists(path)) return(drillr_empty_identity())

  value <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(value)) return(drillr_empty_identity())

  student_id <- trimws(as.character(value$student_id %||% "")[[1]])
  student_name <- trimws(as.character(value$student_name %||% "")[[1]])

  list(student_id = student_id, student_name = student_name)
}

#' Save a student identity for future tutorial sessions
#'
#' Validates and normalizes the supplied identity, then persists it as an RDS
#' file so later Drillr sessions on the same computer can pre-fill the form.
#'
#' @param student_id Student identifier. After coercion and trimming it must be
#'   2--100 characters consisting only of letters, digits, `.`, `_`, `@`, or
#'   `-`.
#' @param student_name Optional student name to save with the identifier.
#' @return Invisibly, the path of the saved identity RDS file.
#' @details Called directly from the `save_identity` observer in
#'   `inst/tutorials/drills/drills.Rmd` and by `test-identity.R`. Depends on
#'   `drillr_identity_path()` and `%||%`.
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

#' Forget the saved student identity
#'
#' Deletes the persisted identity file, if present, so a later tutorial session
#' no longer pre-fills the previously saved student information.
#'
#' @return Invisibly, the identity-file path whether or not a file existed.
#' @details Called directly from the `forget_identity` observer in
#'   `inst/tutorials/drills/drills.Rmd` and by `test-identity.R`. Depends on
#'   `drillr_identity_path()`.
drillr_forget_identity <- function() {
  path <- drillr_identity_path()
  if (file.exists(path)) unlink(path)
  invisible(path)
}

# Utility helpers -------------------------------------------------------------

#' Substitute a fallback for a null or empty value
#'
#' Provides a small package-internal null-coalescing operation used while
#' normalizing optional identity fields. Equivalent definitions also appear in
#' other package source files, so the final namespace binding is intentionally
#' interchangeable with those copies.
#'
#' @param x Value to return unless it is `NULL` or length zero.
#' @param y Fallback value.
#' @return `y` when `x` is `NULL` or empty; otherwise `x`.
#' @details Used directly by `drillr_read_identity()` and
#'   `drillr_save_identity()` in this file.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
