ASSIGNMENT_COLUMNS <- c(
  "assignment_id",
  "course_id",
  "week_id",
  "student_id",
  "item_label",
  "topic",
  "points",
  "assigned_at_utc",
  "assignment_reason",
  "assignment_status",
  "retired_at_utc",
  "retired_reason",
  "retired_request_id"
)

# Assignment request configuration -------------------------------------------

#' Validate assignment queue configuration
#'
#' Normalizes the configured queue size and ordered topic curriculum before
#' those values are sent to the assignment service.
#'
#' @param config Runtime configuration list, normally `APP_CONFIG`.
#' @return A list containing integer `queue_size` and character
#'   `topic_priority`.
#' @details Called by `assignment_service_payload()` for create requests,
#'   `build_log_payload()` for graded logging events, and
#'   `progress_request_payload()` for progress requests. It has no within-repo
#'   function dependencies.
assignment_config <- function(config = APP_CONFIG) {
  if (is.null(config$queue_size) || length(config$queue_size) != 1) {
    stop("APP_CONFIG$queue_size must be one positive integer.")
  }

  queue_size <- suppressWarnings(as.numeric(config$queue_size))
  if (
    is.na(queue_size) || !is.finite(queue_size) || queue_size < 1 ||
    queue_size != floor(queue_size) || queue_size > 500
  ) {
    stop("APP_CONFIG$queue_size must be an integer from 1 through 500.")
  }

  topic_priority <- trimws(as.character(config$topic_priority))
  topic_priority <- topic_priority[nzchar(topic_priority)]
  if (!length(topic_priority)) {
    stop("APP_CONFIG$topic_priority must contain the ordered topic curriculum.")
  }
  if (anyDuplicated(topic_priority)) {
    stop("APP_CONFIG$topic_priority must not contain duplicates.")
  }

  list(queue_size = as.integer(queue_size), topic_priority = topic_priority)
}

#' Generate a request ID for assignment-service calls
#'
#' Combines a caller-supplied prefix, UTC timestamp, and random suffix to create
#' a request identifier used for service-side idempotency and tracing.
#'
#' @param prefix Character prefix describing the request family.
#' @return A length-one character request ID.
#' @details Called directly by `assignment_service_payload()`. It has no
#'   within-repo function dependencies.
make_service_request_id <- function(prefix = "assignment") {
  paste0(
    prefix, "-",
    format(Sys.time(), "%Y%m%d%H%M%OS6", tz = "UTC"), "-",
    paste(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")
  )
}

#' Build an assignment-service request payload
#'
#' Validates the request type and student ID, adds course and bank-reconciliation
#' metadata, and for create requests adds queue size and topic priority.
#'
#' @param request_type Either `"get_active_assignments"` or
#'   `"get_or_create_active_assignments"`.
#' @param student_id Student identifier sent to the assignment service.
#' @param config Runtime configuration list, normally `APP_CONFIG`.
#' @param manifest Optional question manifest used to advertise currently usable
#'   scored item labels.
#' @return A list ready to encode as the assignment-service JSON request body.
#' @details Called by `initialize_student_assignments()`,
#'   `progress_request_payload()`, and `test-manifest-client.R`. Depends on
#'   `make_service_request_id()`, `scored_manifest_labels()`, and, for create
#'   requests, `assignment_config()`.
assignment_service_payload <- function(
  request_type,
  student_id,
  config = APP_CONFIG,
  manifest = NULL
) {
  if (!request_type %in% c("get_active_assignments", "get_or_create_active_assignments")) {
    stop("Unsupported assignment request_type: ", request_type, ".")
  }

  student_id <- trimws(as.character(student_id)[[1]])
  if (!grepl("^[A-Za-z0-9._@-]{2,100}$", student_id)) {
    stop("student_id has an invalid format.")
  }

  payload <- list(
    schema_version = "1",
    request_type = request_type,
    request_id = make_service_request_id(),
    course_id = config$course_id,
    student_id = student_id,
    reconcile_bank = TRUE
  )

  available <- scored_manifest_labels(manifest)
  if (length(available)) payload$available_item_labels <- unname(available)

  if (identical(request_type, "get_or_create_active_assignments")) {
    settings <- assignment_config(config)
    payload$queue_size <- settings$queue_size
    payload$topic_priority <- unname(settings$topic_priority)
  }

  payload
}

# Assignment-service transport -----------------------------------------------

#' Convert an assignment-service error body to an R condition
#'
#' Preserves the service's error code and response body while providing the
#' standard condition fields expected by `stop()` and downstream error handling.
#'
#' @param body Parsed response body returned by the assignment service.
#' @param fallback Message used when the body contains no error text.
#' @return A condition object with class `drillr_service_error` plus standard
#'   `error` and `condition` classes.
#' @details Called directly by `post_assignment_service()`. Depends on `%||%`.
service_error_condition <- function(body, fallback = "The assignment service returned ok=false.") {
  code <- as.character(body$code %||% "")
  message <- as.character(body$error %||% fallback)
  structure(
    list(message = message, call = NULL, code = code, body = body),
    class = c("drillr_service_error", "error", "condition")
  )
}

#' Send a request to the assignment service
#'
#' Posts a JSON payload to the configured webhook, parses the response, and
#' raises a structured service error when the endpoint returns `ok = false`.
#'
#' @param payload Request list to send as JSON.
#' @param config Runtime configuration list containing `webhook_url`.
#' @param timeout_sec HTTP request timeout in seconds.
#' @return The parsed successful response body.
#' @details Called by `initialize_student_assignments()` and
#'   `fetch_student_progress()`. Depends on `service_error_condition()`.
post_assignment_service <- function(payload, config = APP_CONFIG, timeout_sec = 30) {
  if (!nzchar(config$webhook_url)) {
    stop("This Drillr build does not contain an assignment-service URL.")
  }

  response <- httr2::request(config$webhook_url) |>
    httr2::req_body_json(payload, auto_unbox = TRUE, null = "null") |>
    httr2::req_timeout(timeout_sec) |>
    httr2::req_perform()

  body <- httr2::resp_body_json(response, simplifyVector = FALSE)
  if (!isTRUE(body$ok)) stop(service_error_condition(body))
  body
}

# Assignment response normalization ------------------------------------------

#' Extract one scalar value from a service field
#'
#' Normalizes possibly empty response fields before they are placed into a
#' rectangular assignment table.
#'
#' @param x Response field value.
#' @param default Value returned for `NULL` or length-zero input.
#' @return The first element of `x`, or `default`.
#' @details Called only by `assignment_response_table()`. It has no within-repo
#'   function dependencies.
assignment_scalar <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0) return(default)
  x[[1]]
}

#' Create an empty assignment table
#'
#' Supplies the canonical zero-row schema used when the service returns no
#' active assignments.
#'
#' @return A zero-row data frame containing every column in
#'   `ASSIGNMENT_COLUMNS` with stable types.
#' @details Called directly by `assignment_response_table()`. It has no
#'   within-repo function dependencies.
empty_assignment_table <- function() {
  data.frame(
    assignment_id = character(),
    course_id = character(),
    week_id = character(),
    student_id = character(),
    item_label = character(),
    topic = character(),
    points = numeric(),
    assigned_at_utc = character(),
    assignment_reason = character(),
    assignment_status = character(),
    retired_at_utc = character(),
    retired_reason = character(),
    retired_request_id = character(),
    stringsAsFactors = FALSE
  )
}

#' Convert service assignment rows to a data frame
#'
#' Flattens the assignment objects returned by the service into the stable local
#' assignment schema and orders them by assignment time.
#'
#' @param body Parsed service response body containing an `assignments` list.
#' @return A data frame of assignment rows, or the canonical empty assignment
#'   table when the response contains no assignments.
#' @details Called by `initialize_student_assignments()` and
#'   `apply_log_response()`. Depends on `assignment_scalar()` and
#'   `empty_assignment_table()`.
assignment_response_table <- function(body) {
  rows <- body$assignments
  if (is.null(rows) || !length(rows)) return(empty_assignment_table())

  out <- do.call(rbind, lapply(rows, function(row) {
    data.frame(
      assignment_id = as.character(assignment_scalar(row$assignment_id)),
      course_id = as.character(assignment_scalar(row$course_id)),
      week_id = as.character(assignment_scalar(row$week_id)),
      student_id = as.character(assignment_scalar(row$student_id)),
      item_label = as.character(assignment_scalar(row$item_label)),
      topic = as.character(assignment_scalar(row$topic)),
      points = suppressWarnings(as.numeric(assignment_scalar(row$points, NA_real_))),
      assigned_at_utc = as.character(assignment_scalar(row$assigned_at_utc)),
      assignment_reason = as.character(assignment_scalar(row$assignment_reason)),
      assignment_status = as.character(assignment_scalar(row$assignment_status)),
      retired_at_utc = as.character(assignment_scalar(row$retired_at_utc, "")),
      retired_reason = as.character(assignment_scalar(row$retired_reason, "")),
      retired_request_id = as.character(assignment_scalar(row$retired_request_id, "")),
      stringsAsFactors = FALSE
    )
  }))

  rownames(out) <- NULL
  out[order(out$assigned_at_utc), , drop = FALSE]
}

#' Validate active assignments against the local manifest
#'
#' Checks assignment schema, uniqueness, required metadata, and active status;
#' restricts rows to the locally usable question-bank intersection; and verifies
#' topic and point metadata against the manifest.
#'
#' @param assignments Assignment data frame returned by
#'   `assignment_response_table()`.
#' @param manifest Current reconciled question-manifest data frame.
#' @return The validated assignment data frame, possibly with server rows outside
#'   the locally usable bank removed.
#' @details Called by `initialize_student_assignments()` and
#'   `apply_log_response()`. It uses `ASSIGNMENT_COLUMNS` but has no within-repo
#'   function dependencies.
validate_persisted_assignments <- function(assignments, manifest) {
  missing_assignment <- setdiff(ASSIGNMENT_COLUMNS, names(assignments))
  if (length(missing_assignment)) {
    stop(
      "Assignment rows are missing required column(s): ",
      paste(missing_assignment, collapse = ", "), "."
    )
  }

  manifest_required <- c("item_label", "topic", "points")
  missing_manifest <- setdiff(manifest_required, names(manifest))
  if (length(missing_manifest)) {
    stop(
      "Question manifest is missing required column(s): ",
      paste(missing_manifest, collapse = ", "), "."
    )
  }

  if (anyDuplicated(assignments$item_label)) {
    stop("Active assignment rows contain duplicate item_label values.")
  }
  if (anyDuplicated(assignments$assignment_id)) {
    stop("Active assignment rows contain duplicate assignment_id values.")
  }
  if (
    anyNA(assignments$assignment_id) || any(!nzchar(assignments$assignment_id)) ||
    anyNA(assignments$item_label) || any(!nzchar(assignments$item_label)) ||
    anyNA(assignments$topic) || any(!nzchar(assignments$topic)) ||
    anyNA(assignments$points)
  ) {
    stop("Active assignment rows contain missing required metadata.")
  }
  if (nrow(assignments) && any(assignments$assignment_status != "active")) {
    stop("The assignment service returned a non-active row in the active queue.")
  }

  # If manifest and Rmd temporarily disagree, the launch-time reconciliation has
  # already restricted manifest to item_label values present in both. Ignore any
  # server assignment outside that usable intersection rather than failing the app.
  assignments <- assignments[
    assignments$item_label %in% manifest$item_label,
    ,
    drop = FALSE
  ]
  if (!nrow(assignments)) return(assignments)

  expected <- manifest[
    match(assignments$item_label, manifest$item_label),
    ,
    drop = FALSE
  ]
  if (
    any(assignments$topic != expected$topic) ||
    !isTRUE(all.equal(
      as.numeric(assignments$points),
      as.numeric(expected$points),
      check.attributes = FALSE
    ))
  ) {
    stop("The server assignment metadata do not match this Drillr question manifest.")
  }

  assignments
}

# Assignment lifecycle --------------------------------------------------------

#' Load or create the student's active assignment queue
#'
#' Requests the student's active queue from the service, converts and validates
#' the returned assignments against the current manifest, and carries retired
#' assignment metadata alongside the resulting table as an attribute.
#'
#' @param student_id Student identifier whose queue should be initialized.
#' @param manifest Current reconciled question manifest.
#' @param config Runtime configuration list.
#' @return A validated assignment data frame with optional
#'   `retired_assignments` attribute.
#' @details Called directly by the `save_identity` observer in
#'   `inst/tutorials/drills/drills.Rmd`. Depends on
#'   `read_question_manifest()` through its default,
#'   `assignment_service_payload()`, `post_assignment_service()`,
#'   `assignment_response_table()`, `validate_persisted_assignments()`, and
#'   `%||%`.
initialize_student_assignments <- function(
  student_id,
  manifest = read_question_manifest(),
  config = APP_CONFIG
) {
  payload <- assignment_service_payload(
    "get_or_create_active_assignments",
    student_id = student_id,
    config = config,
    manifest = manifest
  )

  body <- post_assignment_service(payload, config = config)
  assignments <- assignment_response_table(body)
  assignments <- validate_persisted_assignments(assignments, manifest)
  attr(assignments, "retired_assignments") <- body$retired_assignments %||% list()
  assignments
}

#' Map question labels to assignment IDs
#'
#' Converts a validated assignment table into the named vector stored in Shiny
#' session state and used to associate logged attempts with their assignments.
#'
#' @param assignments Assignment data frame containing `item_label` and
#'   `assignment_id` columns.
#' @return A character vector of assignment IDs named by item label; an empty
#'   named character vector when there are no assignments.
#' @details Called directly by `set_active_assignment_player()` in `logging.R`.
#'   It has no within-repo function dependencies.
assignment_id_map <- function(assignments) {
  if (!nrow(assignments)) return(setNames(character(), character()))
  if (anyDuplicated(assignments$item_label)) {
    stop("Cannot build assignment ID map from duplicate item_label values.")
  }
  stats::setNames(as.character(assignments$assignment_id), assignments$item_label)
}

# Utility helpers -------------------------------------------------------------

#' Substitute a fallback for a null or empty value
#'
#' Provides the tutorial-runtime null-coalescing operation used when service
#' response fields are optional. Equivalent definitions appear in other sourced
#' tutorial helper files and are intentionally interchangeable.
#'
#' @param x Value to return unless it is `NULL` or length zero.
#' @param y Fallback value.
#' @return `y` when `x` is `NULL` or empty; otherwise `x`.
#' @details Used directly by `service_error_condition()` and
#'   `initialize_student_assignments()` in this file.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
