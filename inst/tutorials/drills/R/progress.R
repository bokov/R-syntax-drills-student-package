PROGRESS_COLUMNS <- c(
  "topic",
  "observations",
  "recent_count",
  "recent_correct",
  "recent_accuracy",
  "estimated_recall",
  "mastered"
)

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

progress_scalar <- function(x, default = NA) {
  if (is.null(x) || length(x) == 0) return(default)
  x[[1]]
}

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

progress_topic_label <- function(topic) {
  label <- gsub("_", " ", as.character(topic), fixed = TRUE)
  label <- gsub("dataframe", "data frame", label, fixed = TRUE)
  tools::toTitleCase(label)
}

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

set_progress_state <- function(session, ok = NA, message = "", rows = NULL, as_of_utc = "") {
  if (!is.null(rows)) session$userData$progress_rows(rows)
  session$userData$progress_as_of_utc(as.character(as_of_utc %||% ""))
  session$userData$progress_status(list(ok = ok, message = message))
  invisible(NULL)
}

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

register_progress_handlers <- function() {
  learnr::event_register_handler("section_viewed", function(session, event, data) {
    if (identical(as.character(data$sectionId %||% ""), "section-your-progress")) {
      refresh_session_progress(session)
    }
  })
  invisible(TRUE)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
