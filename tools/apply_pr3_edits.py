from pathlib import Path

path = Path("inst/tutorials/drills/drills.Rmd")
text = path.read_text(encoding="utf-8")


def replace_once(old, new, label):
    global text
    if old in text:
        text = text.replace(old, new, 1)
        return
    if new in text:
        return
    raise RuntimeError(f"Could not locate {label}")


replace_once(
    'source("R/question_manifest.R")\nsource("R/assignment_storage.R")\nsource("R/syntax_checkers.R")\nsource("R/logging.R")',
    'source("R/question_manifest.R")\nsource("R/assignment_storage.R")\nsource("R/progress.R")\nsource("R/logging.R")',
    "runtime helper sources",
)

replace_once(
    'fallback_config$manifest_path <- DRILLR_LAUNCH_BANK$manifest_path\n'
    'fallback_config$bank_version <- DRILLR_LAUNCH_BANK$bank_version\n'
    'fallback_manifest <- read_question_manifest(DRILLR_LAUNCH_BANK$manifest_path)\n'
    'register_logging_handlers(fallback_config, fallback_manifest)',
    'fallback_config$manifest_path <- DRILLR_LAUNCH_BANK$manifest_path\n'
    'fallback_config$bank_version <- DRILLR_LAUNCH_BANK$bank_version\n'
    'fallback_config$runtime_support_hash <- DRILLR_LAUNCH_BANK$runtime_support_hash\n'
    'fallback_manifest <- read_question_manifest(DRILLR_LAUNCH_BANK$manifest_path)\n'
    'register_logging_handlers(fallback_config, fallback_manifest)\n'
    'register_progress_handlers()',
    "server-start handlers",
)

replace_once(
    'SESSION_CONFIG$manifest_path <- DRILLR_SESSION_BANK$manifest_path\n'
    'SESSION_CONFIG$bank_version <- DRILLR_SESSION_BANK$bank_version\n'
    'SESSION_CONFIG$package_version <- as.character(utils::packageVersion("drillr"))',
    'SESSION_CONFIG$manifest_path <- DRILLR_SESSION_BANK$manifest_path\n'
    'SESSION_CONFIG$bank_version <- DRILLR_SESSION_BANK$bank_version\n'
    'SESSION_CONFIG$runtime_support_hash <- DRILLR_SESSION_BANK$runtime_support_hash\n'
    'SESSION_CONFIG$package_version <- as.character(utils::packageVersion("drillr"))',
    "session bank metadata",
)

replace_once(
    'session$userData$logging_status <- shiny::reactiveVal(\n'
    '  list(\n'
    '    ok = NA,\n'
    '    message = if (DRILLR_STALE_RENDER) {\n'
    '      drillr:::drillr_invalidate_prerendered_html()\n'
    '      "New drill content is ready. Close and reopen Drillr before loading your drills."\n'
    '    } else {\n'
    '      "Load your student ID to begin."\n'
    '    }\n'
    '  )\n'
    ')',
    'session$userData$logging_status <- shiny::reactiveVal(\n'
    '  list(\n'
    '    ok = NA,\n'
    '    message = if (DRILLR_STALE_RENDER) {\n'
    '      drillr:::drillr_invalidate_prerendered_html()\n'
    '      "New drill content is ready. Close and reopen Drillr before loading your drills."\n'
    '    } else {\n'
    '      "Load your student ID to begin."\n'
    '    }\n'
    '  )\n'
    ')\n'
    'session$userData$progress_rows <- shiny::reactiveVal(empty_progress_table())\n'
    'session$userData$progress_as_of_utc <- shiny::reactiveVal("")\n'
    'session$userData$progress_status <- shiny::reactiveVal(list(\n'
    '  ok = NA,\n'
    '  message = "Load your student ID, then open Your progress."\n'
    '))',
    "progress reactive state",
)

replace_once(
    '  session$userData$identity(list(\n'
    '    student_id = student_id,\n'
    '    student_name = student_name\n'
    '  ))\n'
    '  drillr:::drillr_save_identity(student_id, student_name)',
    '  session$userData$identity(list(\n'
    '    student_id = student_id,\n'
    '    student_name = student_name\n'
    '  ))\n'
    '  set_progress_state(\n'
    '    session,\n'
    '    ok = NA,\n'
    '    message = "Open Your progress to load your current summary.",\n'
    '    rows = empty_progress_table(),\n'
    '    as_of_utc = ""\n'
    '  )\n'
    '  drillr:::drillr_save_identity(student_id, student_name)',
    "identity progress reset",
)

replace_once(
    '"A locally saved response was not retried because that question changed or was discontinued."',
    '"A locally saved response was not retried because that question or its grading rules changed or were discontinued."',
    "outbox notice",
)

replace_once(
    '  drillr:::drillr_forget_identity()\n'
    '  session$userData$identity(NULL)\n'
    '  clear_assignment_player()\n'
    '  shiny::updateTextInput(session, "student_id", value = "")',
    '  drillr:::drillr_forget_identity()\n'
    '  session$userData$identity(NULL)\n'
    '  clear_assignment_player()\n'
    '  set_progress_state(\n'
    '    session,\n'
    '    ok = NA,\n'
    '    message = "Load your student ID, then open Your progress.",\n'
    '    rows = empty_progress_table(),\n'
    '    as_of_utc = ""\n'
    '  )\n'
    '  shiny::updateTextInput(session, "student_id", value = "")',
    "forget progress reset",
)

replace_once(
    'output$identity_status <- shiny::renderUI({',
    'shiny::observeEvent(input$refresh_progress, {\n'
    '  refresh_session_progress(session)\n'
    '})\n\n'
    'output$identity_status <- shiny::renderUI({',
    "manual progress refresh",
)

replace_once(
    '  shiny::tags$div(class = cls, status$message)\n})\n```',
    '  shiny::tags$div(class = cls, status$message)\n})\n\n'
    'output$progress_status <- shiny::renderUI({\n'
    '  status <- session$userData$progress_status()\n'
    '  cls <- if (isTRUE(status$ok)) {\n'
    '    "alert alert-success"\n'
    '  } else if (identical(status$ok, FALSE)) {\n'
    '    "alert alert-danger"\n'
    '  } else {\n'
    '    "alert alert-info"\n'
    '  }\n'
    '  shiny::tags$div(class = cls, status$message)\n'
    '})\n\n'
    'output$progress_table <- shiny::renderTable({\n'
    '  format_progress_table(session$userData$progress_rows())\n'
    '}, striped = TRUE, bordered = FALSE, spacing = "s")\n\n'
    'output$progress_as_of <- shiny::renderText({\n'
    '  as_of <- session$userData$progress_as_of_utc()\n'
    '  if (!nzchar(as_of)) return("")\n'
    '  paste("Scheduler estimate as of", as_of)\n'
    '})\n```',
    "progress outputs",
)

replace_once(
    '```{r assignment-player-script, echo=FALSE}\n'
    'shiny::includeScript("www/assignment-player.js")\n'
    '```\n',
    '```{r assignment-player-script, echo=FALSE}\n'
    'shiny::includeScript("www/assignment-player.js")\n'
    '```\n\n'
    '## Your progress\n\n'
    'This table summarizes the same learning history Drillr uses to choose your questions. Estimated recall is a scheduling estimate, not a grade.\n\n'
    '```{r progress-ui, echo=FALSE}\n'
    'shiny::tagList(\n'
    '  shiny::uiOutput("progress_status"),\n'
    '  shiny::tableOutput("progress_table"),\n'
    '  shiny::textOutput("progress_as_of"),\n'
    '  shiny::br(),\n'
    '  shiny::actionButton("refresh_progress", "Refresh progress")\n'
    ')\n'
    '```\n',
    "progress page",
)

path.write_text(text, encoding="utf-8")
