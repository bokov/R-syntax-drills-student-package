# Logging value and identity helpers -----------------------------------------

#' Extract one scalar value for a logging payload
#'
#' Normalizes event fields that may be absent, vector-valued, or HTML before
#' they are serialized into the logging request.
#'
#' @param x Event field value.
#' @param default Value returned for `NULL` or length-zero input.
#' @return The first element of `x`, `x` converted to character for HTML values,
#'   or `default` for missing input.
#' @details Called only by `build_log_payload()`. It has no within-repo function
#'   dependencies.
log_scalar <- function(x, default = NA) {
  if (is.null(x) || length(x) == 0) return(default)
  if (inherits(x, "html")) return(as.character(x))
  x[[1]]
}

#' Generate a request ID for a logged event
#'
#' Combines a UTC timestamp with a random suffix to give each event a distinct
#' request identifier for service-side deduplication and tracing.
#'
#' @return A length-one character request ID.
#' @details Called only by `build_log_payload()`. It has no within-repo function
#'   dependencies.
make_request_id <- function() {
  paste0(
    format(Sys.time(), "%Y%m%d%H%M%OS6", tz = "UTC"), "-",
    paste(sample(c(letters, LETTERS, 0:9), 20, replace = TRUE), collapse = "")
  )
}

#' Read the current session identity
#'
#' Isolates the reactive identity stored on the Shiny session and supplies an
#' NA-valued placeholder when the reactive is unavailable or unset.
#'
#' @param session Current Shiny session.
#' @return A list containing `student_id` and `student_name`.
#' @details Called by `build_log_payload()` and `post_log_event()`. It has no
#'   within-repo function dependencies.
current_identity <- function(session) {
  identity <- tryCatch(
    shiny::isolate(session$userData$identity()),
    error = function(e) NULL
  )
  if (is.null(identity)) {
    identity <- list(student_id = NA_character_, student_name = NA_character_)
  }
  identity
}

#' Look up the assignment ID for a logged question
#'
#' Reads the session's reactive item-label-to-assignment-ID map and returns the
#' matching assignment identifier when one is available.
#'
#' @param session Current Shiny session.
#' @param item_label Question label being logged.
#' @return The assignment ID as character, or `NA_character_` when the label is
#'   missing or not in the active assignment map.
#' @details Called only by `build_log_payload()`. It has no within-repo function
#'   dependencies.
current_assignment_id <- function(session, item_label) {
  if (is.null(item_label) || is.na(item_label) || !nzchar(item_label)) {
    return(NA_character_)
  }

  ids <- tryCatch(
    shiny::isolate(session$userData$assignment_ids()),
    error = function(e) NULL
  )
  if (is.null(ids) || !length(ids) || !item_label %in% names(ids)) {
    return(NA_character_)
  }

  as.character(ids[[item_label]])
}

# Session logging state -------------------------------------------------------

#' Update the session logging status message
#'
#' Writes the logging health state consumed by the tutorial's identity-status
#' UI while tolerating sessions where that reactive is unavailable.
#'
#' @param session Current Shiny session.
#' @param ok Logical status value (`TRUE`, `FALSE`, or `NA`).
#' @param message Student-facing status message.
#' @return Invisibly, `NULL`.
#' @details Called by `post_log_event()` and directly from the `save_identity`
#'   observer in `drills.Rmd` when retired/dropped-item notices are appended. It
#'   has no within-repo function dependencies.
set_logging_status <- function(session, ok, message) {
  tryCatch(
    session$userData$logging_status(list(ok = ok, message = message)),
    error = function(e) invisible(NULL)
  )
  invisible(NULL)
}

#' Activate an assignment queue in the session and browser player
#'
#' Stores the assignment-ID map in Shiny session state and sends the active item
#' labels to the browser-side assignment player that shows and hides exercises.
#'
#' @param session Current Shiny session.
#' @param assignments Validated active assignment data frame.
#' @return Invisibly, `assignments`.
#' @details Called by `apply_log_response()` after correct graded responses and
#'   directly by the `save_identity` observer in `drills.Rmd`. Depends on
#'   `assignment_id_map()`.
set_active_assignment_player <- function(session, assignments) {
  session$userData$assignment_ids(assignment_id_map(assignments))
  session$sendCustomMessage(
    "assignment:set",
    list(item_labels = unname(as.character(assignments$item_label)))
  )
  invisible(assignments)
}

# Logging-service transport --------------------------------------------------

#' Send a logging payload to the assignment service
#'
#' Posts a logging event as JSON and converts both service-level failures and
#' transport errors into a consistent result list rather than throwing them to
#' the tutorial event handler.
#'
#' @param payload Logging payload list.
#' @param config Runtime configuration containing `webhook_url`.
#' @param timeout_sec HTTP request timeout in seconds.
#' @return A result list containing at least `ok`, `transport_ok`, and `code`,
#'   plus `body` on success or `message` on failure.
#' @details Called by `flush_log_outbox()` and `post_log_event()`. Depends on
#'   `%||%` for optional service error fields.
log_service_request <- function(payload, config = APP_CONFIG, timeout_sec = 30) {
  if (!nzchar(config$webhook_url)) {
    return(list(
      ok = FALSE,
      transport_ok = FALSE,
      code = "",
      message = "This Drillr build does not contain an assignment-service URL."
    ))
  }

  tryCatch({
    response <- httr2::request(config$webhook_url) |>
      httr2::req_body_json(payload, auto_unbox = TRUE, null = "null") |>
      httr2::req_timeout(timeout_sec) |>
      httr2::req_perform()

    body <- httr2::resp_body_json(response, simplifyVector = FALSE)
    if (!isTRUE(body$ok)) {
      return(list(
        ok = FALSE,
        transport_ok = TRUE,
        code = as.character(body$code %||% ""),
        message = as.character(body$error %||% "The logging endpoint returned ok=false."),
        body = body
      ))
    }

    list(ok = TRUE, transport_ok = TRUE, code = "", body = body)
  }, error = function(e) {
    list(
      ok = FALSE,
      transport_ok = FALSE,
      code = "",
      message = conditionMessage(e)
    )
  })
}

#' Apply assignment changes returned by a logged event
#'
#' For a correct graded response, converts and validates any replacement
#' assignment queue returned by the service and activates it in both session and
#' browser state. Other events leave assignments unchanged.
#'
#' @param session Current Shiny session.
#' @param payload Logging payload that produced the response.
#' @param body Parsed successful service response body.
#' @param manifest Current reconciled question manifest.
#' @return Invisibly, `body`.
#' @details Called by `flush_log_outbox()` for successful retries and by
#'   `post_log_event()` for live events. Depends on `assignment_response_table()`,
#'   `validate_persisted_assignments()`, and `set_active_assignment_player()`.
apply_log_response <- function(session, payload, body, manifest) {
  graded_event <- payload$event %in% c("exercise_result", "question_submission")
  if (
    graded_event && isTRUE(as.logical(payload$correct)) &&
    !is.null(body$assignments)
  ) {
    assignments <- assignment_response_table(body)
    assignments <- validate_persisted_assignments(assignments, manifest)
    set_active_assignment_player(session, assignments)
  }
  invisible(body)
}

# Local outbox integration ----------------------------------------------------

#' Queue an undelivered logging payload
#'
#' Bridges tutorial-runtime logging code to the package-level persistent outbox
#' used to retry responses after transport failures.
#'
#' @param payload Logging payload to persist.
#' @return The value returned by `drillr_outbox_enqueue()`.
#' @details Called only by `post_log_event()`. Depends on package-internal
#'   `drillr:::drillr_outbox_enqueue()`.
queue_log_payload <- function(payload) {
  drillr:::drillr_outbox_enqueue(payload)
}

#' Check whether a queued payload still refers to a usable question
#'
#' Allows non-graded events to retry unconditionally, while requiring graded
#' events to reference a scored item label that remains in the reconciled
#' manifest.
#'
#' @param payload Queued logging payload.
#' @param manifest Current reconciled question manifest.
#' @return `TRUE` when the payload can still be sent; otherwise `FALSE`.
#' @details Called by `flush_log_outbox()` and directly by
#'   `tests/testthat/test-manifest-client.R`. Depends on `%||%` and
#'   `scored_manifest_labels()`.
outbox_payload_is_usable <- function(payload, manifest) {
  event <- as.character(payload$event %||% "")
  if (!event %in% c("exercise_result", "question_submission")) return(TRUE)
  label <- as.character(payload$item_label %||% "")
  nzchar(label) && label %in% scored_manifest_labels(manifest)
}

#' Retry queued logging payloads for a student
#'
#' Reads matching local outbox records in queue order, drops graded events whose
#' questions are no longer usable, sends remaining records until a failure
#' blocks progress, applies successful assignment updates, and removes delivered
#' records.
#'
#' @param session Current Shiny session.
#' @param student_id Student identifier whose queued records should be retried.
#' @param config Runtime configuration list.
#' @param manifest Current reconciled question manifest.
#' @return A summary list with sent/dropped counts plus transport/blocking status,
#'   service code, and message.
#' @details Called by `post_log_event()` before sending a new identified event
#'   and directly by the `save_identity` observer in `drills.Rmd`. Depends on
#'   `read_question_manifest()` through its default,
#'   `drillr:::drillr_outbox_entries()`, `outbox_payload_is_usable()`,
#'   `drillr:::drillr_outbox_remove()`, `log_service_request()`, and
#'   `apply_log_response()`.
flush_log_outbox <- function(
  session,
  student_id,
  config = APP_CONFIG,
  manifest = read_question_manifest(config$manifest_path)
) {
  records <- drillr:::drillr_outbox_entries(
    student_id = student_id,
    course_id = config$course_id
  )
  summary <- list(
    sent = 0L,
    dropped = 0L,
    transport_failed = FALSE,
    blocked = FALSE,
    code = "",
    message = ""
  )
  if (!length(records)) return(summary)

  for (record in records) {
    if (!outbox_payload_is_usable(record$payload, manifest)) {
      drillr:::drillr_outbox_remove(record)
      summary$dropped <- summary$dropped + 1L
      next
    }

    result <- log_service_request(record$payload, config)
    if (!isTRUE(result$transport_ok)) {
      summary$transport_failed <- TRUE
      summary$message <- result$message
      break
    }
    if (!isTRUE(result$ok)) {
      summary$blocked <- TRUE
      summary$code <- result$code
      summary$message <- result$message
      break
    }

    apply_log_response(session, record$payload, result$body, manifest)
    drillr:::drillr_outbox_remove(record)
    summary$sent <- summary$sent + 1L
  }

  summary
}

# Logging payload construction -----------------------------------------------

#' Build a complete logging event payload
#'
#' Combines learnr event data with session identity, assignment IDs, course
#' configuration, question topic metadata, and graded-event reconciliation
#' fields into the JSON-ready structure sent to the assignment service.
#'
#' @param session Current Shiny session.
#' @param event Learnr event name.
#' @param data Event data list supplied by learnr.
#' @param config Runtime configuration list.
#' @param manifest Current reconciled question manifest.
#' @return A logging payload list.
#' @details Called only by `post_log_event()`. Depends on `current_identity()`,
#'   `log_scalar()`, `current_assignment_id()`, `make_request_id()`,
#'   `question_topic()`, `assignment_config()`, and `scored_manifest_labels()`.
build_log_payload <- function(session, event, data, config, manifest) {
  identity <- current_identity(session)
  item_label <- log_scalar(data$label, NA_character_)
  assignment_id <- log_scalar(
    data$assignment_id,
    current_assignment_id(session, item_label)
  )
  graded_event <- event %in% c("exercise_result", "question_submission")

  payload <- list(
    schema_version = "1",
    request_id = make_request_id(),
    client_timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    course_id = config$course_id,
    session_token = session$token,
    student_id = log_scalar(identity$student_id, NA_character_),
    student_name = log_scalar(identity$student_name, NA_character_),
    event = event,
    item_label = item_label,
    topic = if (graded_event) question_topic(item_label, manifest) else NA_character_,
    assignment_id = assignment_id,
    attempt_id = log_scalar(data$id, NA_character_),
    submitted_code = log_scalar(data$code, NA_character_),
    correct = if (!is.null(data$feedback$correct)) {
      isTRUE(data$feedback$correct)
    } else {
      log_scalar(data$correct, NA)
    },
    answer = if (!is.null(data$answer)) paste(data$answer, collapse = " | ") else NA_character_,
    checked = log_scalar(data$checked, NA),
    restore = log_scalar(data$restore, NA),
    time_elapsed_sec = log_scalar(data$time_elapsed, NA_real_),
    timeout_exceeded = log_scalar(data$timeout_exceeded, NA),
    error_message = log_scalar(data$error_message, NA_character_)
  )

  if (graded_event) {
    settings <- assignment_config(config)
    payload$queue_size <- settings$queue_size
    payload$topic_priority <- unname(settings$topic_priority)
    payload$reconcile_bank <- TRUE
    payload$available_item_labels <- unname(scored_manifest_labels(manifest))
  }

  payload
}

# Event delivery --------------------------------------------------------------

#' Deliver one learnr logging event with local retry protection
#'
#' Builds the event payload, flushes older queued responses first for identified
#' students, queues the new payload when transport is unavailable, applies
#' successful assignment updates, and keeps the tutorial's logging-status UI in
#' sync with the outcome.
#'
#' @param session Current Shiny session.
#' @param event Learnr event name.
#' @param data Event data list supplied by learnr.
#' @param config Runtime configuration list.
#' @param manifest Current reconciled question manifest.
#' @return Invisibly, a result list describing success, queueing, message, and
#'   duplicate status when available.
#' @details Called directly by the `identity_saved` path in `drills.Rmd` and by
#'   both event callbacks registered in `register_logging_handlers()`. Depends on
#'   `read_question_manifest()` through its default, `current_identity()`,
#'   `build_log_payload()`, `flush_log_outbox()`, `queue_log_payload()`,
#'   `set_logging_status()`, `log_service_request()`, and `apply_log_response()`.
post_log_event <- function(
  session,
  event,
  data = list(),
  config = APP_CONFIG,
  manifest = read_question_manifest(config$manifest_path)
) {
  identity <- current_identity(session)
  payload <- build_log_payload(session, event, data, config, manifest)

  if (!is.na(identity$student_id) && nzchar(identity$student_id)) {
    flush <- flush_log_outbox(
      session,
      student_id = identity$student_id,
      config = config,
      manifest = manifest
    )
    if (isTRUE(flush$transport_failed) || isTRUE(flush$blocked)) {
      queue_log_payload(payload)
      msg <- if (nzchar(flush$message)) {
        flush$message
      } else {
        "This response was saved locally and will retry automatically."
      }
      if (isTRUE(flush$transport_failed)) {
        msg <- "Connection problem: this response was saved locally and will retry automatically."
      }
      set_logging_status(session, FALSE, msg)
      return(invisible(list(ok = FALSE, queued = TRUE, message = msg)))
    }
  }

  result <- log_service_request(payload, config)
  if (!isTRUE(result$transport_ok)) {
    queue_log_payload(payload)
    msg <- "Connection problem: this response was saved locally and will retry automatically."
    set_logging_status(session, FALSE, msg)
    return(invisible(list(ok = FALSE, queued = TRUE, message = msg)))
  }

  if (!isTRUE(result$ok)) {
    set_logging_status(session, FALSE, result$message)
    return(invisible(list(ok = FALSE, queued = FALSE, message = result$message)))
  }

  apply_log_response(session, payload, result$body, manifest)
  msg <- "Responses are being recorded."
  set_logging_status(session, TRUE, msg)
  invisible(list(
    ok = TRUE,
    message = msg,
    duplicate = isTRUE(result$body$duplicate)
  ))
}

# Handler registration --------------------------------------------------------

#' Resolve session-specific logging configuration
#'
#' Prefers the runtime config and reconciled manifest stored on the current
#' Shiny session, falling back to the values captured when event handlers were
#' registered.
#'
#' @param session Current Shiny session.
#' @param fallback_config Configuration captured at handler registration.
#' @param fallback_manifest Manifest captured at handler registration.
#' @return A list containing resolved `config` and `manifest`.
#' @details Called only by callbacks created in `register_logging_handlers()`.
#'   Depends on `%||%`.
session_logging_context <- function(session, fallback_config, fallback_manifest) {
  config <- session$userData$runtime_config %||% fallback_config
  manifest <- session$userData$question_manifest %||% fallback_manifest
  list(config = config, manifest = manifest)
}

#' Register learnr logging event handlers
#'
#' Registers callbacks for exercise results and question submissions. Each
#' callback resolves the current session-specific bank/config context before
#' passing the event to the common logging pipeline.
#'
#' @param config Fallback runtime configuration captured at registration.
#' @param manifest Fallback reconciled manifest captured at registration.
#' @return Invisibly, `TRUE` after both handlers are registered.
#' @details Called directly from the `logging-start` server-start chunk in
#'   `inst/tutorials/drills/drills.Rmd`. Depends on `read_question_manifest()`
#'   through its default, `session_logging_context()`, and `post_log_event()`.
register_logging_handlers <- function(
  config = APP_CONFIG,
  manifest = read_question_manifest(config$manifest_path)
) {
  learnr::event_register_handler("exercise_result", function(session, event, data) {
    context <- session_logging_context(session, config, manifest)
    post_log_event(session, event, data, context$config, context$manifest)
  })

  learnr::event_register_handler("question_submission", function(session, event, data) {
    context <- session_logging_context(session, config, manifest)
    post_log_event(session, event, data, context$config, context$manifest)
  })

  invisible(TRUE)
}

# Utility helpers -------------------------------------------------------------

#' Substitute a fallback for a null or empty value
#'
#' Provides the tutorial-runtime null-coalescing operation used for optional
#' service fields and session state. Equivalent definitions appear in other
#' sourced tutorial helper files and are intentionally interchangeable.
#'
#' @param x Value to return unless it is `NULL` or length zero.
#' @param y Fallback value.
#' @return `y` when `x` is `NULL` or empty; otherwise `x`.
#' @details Used by `log_service_request()`, `outbox_payload_is_usable()`, and
#'   `session_logging_context()` in this file.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
