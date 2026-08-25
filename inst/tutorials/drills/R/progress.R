PROGRESS_COLUMNS <- c(
  "topic",
  "observations",
  "recent_count",
  "recent_correct",
  "recent_accuracy",
  "estimated_recall",
  "mastered"
)

# Progress response normalization --------------------------------------------

#' Create an empty progress table
#'
#' Supplies the canonical zero-row scheduler-summary schema used to initialize
#' session state and represent a valid progress response with no topic rows.
#'
#' @return A zero-row data frame with the columns in `PROGRESS_COLUMNS` and
#'   stable column types.
#' @details Called by `progress_response_table()` and directly from
#'   `inst/tutorials/drills/drills.Rmd` when progress state is initialized or
#'   reset. It has no within-repo function dependencies.
empty_progress_table <- function() {
  data.frame(
    topic = character(),
    observations = integer(),
    recent_count = integer(),
    recent_correct = integer(),
    recent_accuracy = numeric(),
    estimated_recall = numeric(),
    mastered = logical(),
    stringsAsFactors = FALSE
  )
}

#' Extract one scalar value from a progress field
#'
#' Normalizes possibly empty service fields before they are inserted into the
#' rectangular progress table.
#'
#' @param x Response field value.
#' @param default Value returned for `NULL` or length-zero input.
#' @return The first element of `x`, or `default`.
#' @details Called only by `progress_response_table()`. It has no within-repo
#'   function dependencies.
progress_scalar <- function(x, default = NA) {
  if (is.null(x) || length(x) == 0) return(default)
  x[[1]]
}

#' Convert a progress-service response to a data frame
#'
#' Distinguishes an unavailable `progress` field from an intentionally empty
#' progress list, then flattens returned topic summaries into the stable local
#' schema used by the tutorial UI.
#'
#' @param body Parsed assignment-service response body.
#' @return `NULL` when the response has no `progress` field; otherwise a progress
#'   data frame, possibly with zero rows.
#' @details Called by `fetch_student_progress()` and directly by
#'   `tests/testthat/test-progress.R`. Depends on `progress_scalar()`,
#'   `empty_progress_table()`, and the `PROGRESS_COLUMNS` constant.
progress_response_table <- function(body) {
  rows <- body$progress
  if (is.null(rows)) return(NULL)
  if (!length(rows)) return(empty_progress_table())

  out <- do.call(rbind, lapply(rows, function(row) {
    data.frame(
      topic = as.character(progress_scalar(row$topic, "")),
      observations = suppressWarnings(as.integer(progress_scalar(row$observations, 0L))),
      recent_count = suppressWarnings(as.integer(progress_scalar(row$recent_count, 0L))),
      recent_correct = suppressWarnings(as.integer(progress_scalar(row$recent_correct, 0L))),
      recent_accuracy = suppressWarnings(as.numeric(progress_scalar(row$recent_accuracy, NA_real_))),
      estimated_recall = suppressWarnings(as.numeric(progress_scalar(row$estimated_recall, NA_real_))),
      mastered = isTRUE(progress_scalar(row$mastered, FALSE)),
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL

  missing <- setdiff(PROGRESS_COLUMNS, names(out))
  if (length(missing)) {
    stop("Progress response is missing required field(s): ", paste(missing, collapse = ", "), ".")
  }
  out
}

# Progress requests -----------------------------------------------------------

#' Build a progress request payload
#'
#' Starts from a read-only active-assignment request and adds the fields that ask
#' the service for scheduler progress using the configured topic ordering.
#'
#' @param student_id Student identifier whose progress should be requested.
#' @param config Runtime configuration list.
#' @param manifest Current reconciled question manifest.
#' @return A request payload list with `include_progress = TRUE` and topic
#'   priority metadata.
#' @details Called only by `fetch_student_progress()`. Depends on
#'   `read_question_manifest()` through its default,
#'   `assignment_service_payload()`, and `assignment_config()`.
progress_request_payload <- function(
  student_id,
  config = APP_CONFIG,
  manifest = read_question_manifest(config$manifest_path)
) {
  payload <- assignment_service_payload(
    "get_active_assignments",
    student_id = student_id,
    config = config,
    manifest = manifest
  )
  settings <- assignment_config(config)
  payload$include_progress <- TRUE
  payload$topic_priority <- unname(settings$topic_priority)
  payload
}

#' Fetch a student's scheduler progress summary
#'
#' Sends the progress request through the assignment service and converts its
#' topic rows into the local progress schema, preserving the service timestamp
#' that describes when the scheduler estimate was calculated.
#'
#' @param student_id Student identifier whose progress should be fetched.
#' @param config Runtime configuration list.
#' @param manifest Current reconciled question manifest.
#' @return A list with progress `rows` and character `as_of_utc` timestamp.
#' @details Called only by `refresh_session_progress()`. Depends on
#'   `read_question_manifest()` through its default,
#'   `progress_request_payload()`, `post_assignment_service()`,
#'   `progress_response_table()`, and `%||%`.
fetch_student_progress <- function(
  student_id,
  config = APP_CONFIG,
  manifest = read_question_manifest(config$manifest_path)
) {
  payload <- progress_request_payload(student_id, config, manifest)
  body <- post_assignment_service(payload, config = config)

  rows <- progress_response_table(body)
  if (is.null(rows)) {
    stop("Progress reporting is not available from the grading service yet.")
  }

  list(
    rows = rows,
    as_of_utc = as.character(body$progress_as_of_utc %||% "")
  )
}

# Progress display ------------------------------------------------------------

#' Convert a topic key to a student-facing label
#'
#' Replaces internal separators and the `dataframe` token, then title-cases the
#' result for display in the progress table.
#'
#' @param topic Internal topic identifier.
#' @return A length-one character display label.
#' @details Called only by `format_progress_table()`. It has no within-repo
#'   function dependencies.
progress_topic_label <- function(topic) {
  label <- gsub("_", " ", as.character(topic), fixed = TRUE)
  label <- gsub("dataframe", "data frame", label, fixed = TRUE)
  tools::toTitleCase(label)
}

#' Format progress rows for the student UI
#'
#' Converts scheduler fields to compact display strings for recent accuracy and
#' estimated recall while retaining the number of practiced questions.
#'
#' @param rows Progress data frame in the `PROGRESS_COLUMNS` schema.
#' @return A student-facing data frame with Topic, Recent first-try accuracy,
#'   Questions practiced, and Estimated recall columns.
#' @details Called from the `progress_table` renderer in
#'   `inst/tutorials/drills/drills.Rmd` and directly by `test-progress.R`.
#'   Depends on `progress_topic_label()`.
format_progress_table <- function(rows) {
  if (!nrow(rows)) {
    return(data.frame(
      Topic = character(),
      `Recent first-try accuracy` = character(),
      `Questions practiced` = integer(),
      `Estimated recall` = character(),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }

  recent <- ifelse(
    rows$recent_count > 0 & is.finite(rows$recent_accuracy),
    sprintf(
      "%d/%d (%.0f%%)",
      rows$recent_correct,
      rows$recent_count,
      100 * rows$recent_accuracy
    ),
    "—"
  )
  recall <- ifelse(
    rows$observations > 0 & is.finite(rows$estimated_recall),
    sprintf("%.0f%%", 100 * rows$estimated_recall),
    "—"
  )

  data.frame(
    Topic = vapply(rows$topic, progress_topic_label, character(1)),
    `Recent first-try accuracy` = recent,
    `Questions practiced` = rows$observations,
    `Estimated recall` = recall,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# Session progress state ------------------------------------------------------

#' Update progress-related Shiny session state
#'
#' Writes optional progress rows plus the service timestamp and status message
#' into the reactive values consumed by the progress UI.
#'
#' @param session Current Shiny session.
#' @param ok Logical status value (`TRUE`, `FALSE`, or `NA`).
#' @param message Student-facing status message.
#' @param rows Optional progress data frame; when `NULL`, existing rows are
#'   retained.
#' @param as_of_utc Scheduler-estimate timestamp to store.
#' @return Invisibly, `NULL`.
#' @details Called by `refresh_session_progress()` and directly from the
#'   `save_identity` and `forget_identity` observers in `drills.Rmd`. Depends on
#'   `%||%`.
set_progress_state <- function(session, ok = NA, message = "", rows = NULL, as_of_utc = "") {
  if (!is.null(rows)) session$userData$progress_rows(rows)
  session$userData$progress_as_of_utc(as.character(as_of_utc %||% ""))
  session$userData$progress_status(list(ok = ok, message = message))
  invisible(NULL)
}

#' Refresh progress for the current tutorial session
#'
#' Validates that progress reactives and a saved session identity exist, resolves
#' the session-specific runtime configuration and manifest, fetches the latest
#' scheduler summary, and updates the UI state for success or failure.
#'
#' @param session Current Shiny session.
#' @return Invisibly, the fetched progress result on success or `NULL` when the
#'   refresh cannot proceed or fails.
#' @details Called by `register_progress_handlers()` and directly from the
#'   `refresh_progress` observer in `drills.Rmd`. Depends on `%||%`,
#'   `read_question_manifest()`, `fetch_student_progress()`, and
#'   `set_progress_state()`.
refresh_session_progress <- function(session) {
  if (
    is.null(session$userData$progress_status) ||
    is.null(session$userData$progress_rows) ||
    is.null(session$userData$progress_as_of_utc)
  ) {
    return(invisible(NULL))
  }

  identity <- tryCatch(
    shiny::isolate(session$userData$identity()),
    error = function(e) NULL
  )
  if (is.null(identity) || !nzchar(as.character(identity$student_id %||% ""))) {
    set_progress_state(
      session,
      ok = NA,
      message = "Load your student ID on Before you begin to see progress."
    )
    return(invisible(NULL))
  }

  config <- session$userData$runtime_config %||% APP_CONFIG
  manifest <- session$userData$question_manifest %||%
    read_question_manifest(config$manifest_path)
  result <- tryCatch(
    fetch_student_progress(identity$student_id, config, manifest),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    set_progress_state(
      session,
      ok = FALSE,
      message = paste("Could not load progress:", conditionMessage(result))
    )
    return(invisible(NULL))
  }

  set_progress_state(
    session,
    ok = TRUE,
    message = "Progress updated.",
    rows = result$rows,
    as_of_utc = result$as_of_utc
  )
  invisible(result)
}

#' Register automatic progress refresh handling
#'
#' Registers the learnr `section_viewed` handler that refreshes scheduler
#' progress when the student opens the Your progress section.
#'
#' @return Invisibly, `TRUE` after the handler is registered.
#' @details Called directly from the `logging-start` server-start chunk in
#'   `inst/tutorials/drills/drills.Rmd`. Depends on `%||%` and
#'   `refresh_session_progress()`.
register_progress_handlers <- function() {
  learnr::event_register_handler("section_viewed", function(session, event, data) {
    if (identical(as.character(data$sectionId %||% ""), "section-your-progress")) {
      refresh_session_progress(session)
    }
  })
  invisible(TRUE)
}

# Utility helpers -------------------------------------------------------------

#' Substitute a fallback for a null or empty value
#'
#' Provides the tutorial-runtime null-coalescing operation used throughout
#' progress state and response handling. Equivalent definitions appear in other
#' sourced tutorial helper files and are intentionally interchangeable.
#'
#' @param x Value to return unless it is `NULL` or length zero.
#' @param y Fallback value.
#' @return `y` when `x` is `NULL` or empty; otherwise `x`.
#' @details Used by `fetch_student_progress()`, `set_progress_state()`,
#'   `refresh_session_progress()`, and `register_progress_handlers()`.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
