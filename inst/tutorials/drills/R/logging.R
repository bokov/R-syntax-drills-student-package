log_scalar <- function(x, default = NA) {
  if (is.null(x) || length(x) == 0) return(default)
  if (inherits(x, "html")) return(as.character(x))
  x[[1]]
}

make_request_id <- function() {
  paste0(
    format(Sys.time(), "%Y%m%d%H%M%OS6", tz = "UTC"), "-",
    paste(sample(c(letters, LETTERS, 0:9), 20, replace = TRUE), collapse = "")
  )
}

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

set_logging_status <- function(session, ok, message) {
  tryCatch(
    session$userData$logging_status(list(ok = ok, message = message)),
    error = function(e) invisible(NULL)
  )
  invisible(NULL)
}

set_active_assignment_player <- function(session, assignments) {
  session$userData$assignment_ids(assignment_id_map(assignments))
  session$sendCustomMessage(
    "assignment:set",
    list(item_labels = unname(as.character(assignments$item_label)))
  )
  invisible(assignments)
}

log_service_request <- function(payload, config = APP_CONFIG, timeout_sec = 30) {
  if (!nzchar(config$webhook_url)) {
    return(list(
      ok = FALSE,
      transport_ok = FALSE,
      code = "",
      message = "This drillr build does not contain an assignment-service URL."
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

outbox_record_hash <- function(payload, manifest) {
  question_hash_for_item(
    as.character(payload$item_label %||% ""),
    manifest,
    default = ""
  )
}

queue_log_payload <- function(payload, manifest) {
  drillr:::drillr_outbox_enqueue(
    payload,
    question_hash = outbox_record_hash(payload, manifest)
  )
}

migrate_outbox_record <- function(record, manifest, config = APP_CONFIG) {
  payload <- record$payload
  queued_version <- as.character(payload$bank_version %||% "")
  current_version <- as.character(config$bank_version %||% "")

  if (!nzchar(queued_version) || !nzchar(current_version) || identical(queued_version, current_version)) {
    if (nzchar(current_version) && !is.null(payload$package_version)) {
      payload$package_version <- config$package_version
    }
    return(list(drop = FALSE, changed = FALSE, payload = payload))
  }

  item_label <- as.character(payload$item_label %||% "")
  current_hash <- question_hash_for_item(item_label, manifest, default = "")
  queued_hash <- as.character(record$question_hash %||% "")
  if (!nzchar(current_hash) || !nzchar(queued_hash) || !identical(current_hash, queued_hash)) {
    return(list(drop = TRUE, changed = FALSE, payload = payload))
  }

  payload$bank_version <- current_version
  payload$package_version <- config$package_version
  list(drop = FALSE, changed = TRUE, payload = payload)
}

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
    migrated <- migrate_outbox_record(record, manifest, config)
    if (isTRUE(migrated$drop)) {
      drillr:::drillr_outbox_remove(record)
      summary$dropped <- summary$dropped + 1L
      next
    }
    if (isTRUE(migrated$changed)) {
      drillr:::drillr_outbox_replace(
        record,
        migrated$payload,
        question_hash = record$question_hash
      )
      record$payload <- migrated$payload
    }

    result <- log_service_request(migrated$payload, config)
    if (!isTRUE(result$transport_ok)) {
      summary$transport_failed <- TRUE
      summary$message <- result$message
      break
    }
    if (!isTRUE(result$ok)) {
      summary$blocked <- TRUE
      summary$code <- result$code
      summary$message <- result$message
      if (identical(result$code, "bank_update_required")) {
        update <- drillr:::drillr_update_for_service(result$body)
        summary$message <- update$message
      }
      break
    }

    apply_log_response(session, migrated$payload, result$body, manifest)
    drillr:::drillr_outbox_remove(record)
    summary$sent <- summary$sent + 1L
  }

  summary
}

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
    payload <- add_bank_handshake_fields(payload, config)
  }

  payload
}

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
      queue_log_payload(payload, manifest)
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
    queue_log_payload(payload, manifest)
    msg <- "Connection problem: this response was saved locally and will retry automatically."
    set_logging_status(session, FALSE, msg)
    return(invisible(list(ok = FALSE, queued = TRUE, message = msg)))
  }

  if (!isTRUE(result$ok)) {
    queueable <- result$code %in% c("bank_update_required", "bank_metadata_unavailable")
    if (isTRUE(queueable)) queue_log_payload(payload, manifest)

    msg <- result$message
    if (identical(result$code, "bank_update_required")) {
      update <- drillr:::drillr_update_for_service(result$body)
      msg <- paste(update$message, "This response was saved for a safe retry.")
    } else if (identical(result$code, "bank_metadata_unavailable")) {
      msg <- paste(
        "The grading service has not finished publishing its drill-bank metadata.",
        "This response was saved locally and will retry automatically."
      )
    }
    set_logging_status(session, FALSE, msg)
    return(invisible(list(ok = FALSE, queued = queueable, message = msg)))
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

session_logging_context <- function(session, fallback_config, fallback_manifest) {
  config <- session$userData$runtime_config %||% fallback_config
  manifest <- session$userData$question_manifest %||% fallback_manifest
  list(config = config, manifest = manifest)
}

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

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
