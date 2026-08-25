# Question manifest access ----------------------------------------------------

#' Read the tutorial question manifest
#'
#' Loads the question metadata used by assignment selection, logging, and
#' progress requests. When the requested file is absent, returns an empty table
#' with the expected schema so downstream helpers can handle the absence
#' consistently.
#'
#' @param path Path to the question-manifest CSV.
#' @return A data frame with `item_label`, `event`, `topic`, `points`,
#'   `starter_question`, and `release` columns, or the columns present in the
#'   loaded manifest.
#' @details Used in default arguments and fallbacks by
#'   `initialize_student_assignments()`, `flush_log_outbox()`,
#'   `post_log_event()`, `register_logging_handlers()`,
#'   `progress_request_payload()`, `fetch_student_progress()`, and
#'   `refresh_session_progress()`. It has no within-repo function dependencies.
read_question_manifest <- function(path = "question_manifest.csv") {
  if (!file.exists(path)) {
    return(data.frame(
      item_label = character(),
      event = character(),
      topic = character(),
      points = numeric(),
      starter_question = logical(),
      release = integer(),
      stringsAsFactors = FALSE
    ))
  }

  read.csv(path, stringsAsFactors = FALSE, na.strings = "")
}

#' Return scored exercise labels from a manifest
#'
#' Extracts the unique item labels that represent positive-point
#' `exercise_result` rows. These labels define the questions available for
#' assignment reconciliation and queued-response validation.
#'
#' @param manifest Question-manifest data frame, or `NULL`.
#' @return A character vector of unique scored item labels, possibly empty.
#' @details Called directly by `assignment_service_payload()`,
#'   `outbox_payload_is_usable()`, and `build_log_payload()`. It has no
#'   within-repo function dependencies.
scored_manifest_labels <- function(manifest) {
  if (is.null(manifest) || !nrow(manifest)) return(character())
  unique(as.character(manifest$item_label[
    manifest$event == "exercise_result" & manifest$points > 0
  ]))
}

#' Look up the topic for a question label
#'
#' Resolves an exact manifest item label or, for generated child labels, the
#' unique manifest parent separated by `-`, `_`, or `.`. Logging uses the result
#' to attach curriculum topics to graded events.
#'
#' @param item_label Question or generated child label to resolve.
#' @param manifest Question-manifest data frame.
#' @param default Value returned when the label is missing, ambiguous, or not
#'   represented in the manifest.
#' @return The matching topic value, or `default`.
#' @details Called directly by `build_log_payload()` in `logging.R`. It has no
#'   within-repo function dependencies.
question_topic <- function(item_label, manifest, default = "unassigned") {
  if (is.null(item_label) || is.na(item_label) || !nzchar(item_label)) {
    return(default)
  }
  if (!nrow(manifest)) return(default)

  exact <- which(manifest$item_label == item_label)
  if (length(exact) == 1) return(manifest$topic[[exact]])

  parent <- which(vapply(
    manifest$item_label,
    function(label) {
      any(vapply(
        c("-", "_", "."),
        function(separator) startsWith(item_label, paste0(label, separator)),
        logical(1)
      ))
    },
    logical(1)
  ))
  if (length(parent) == 1) return(manifest$topic[[parent]])

  default
}
