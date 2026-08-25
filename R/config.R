.drillr_default_webhook_url <- paste0(
  "https://script.google.com/macros/s/",
  "AKfycbzpC-6D-CrclfMA8PZByTqw9jxOCZYbthx7NB0Y42LfeJc8uDIWj2P2FQd31PXN1Lz3xw/exec"
)

# The central service currently expects curriculum configuration from the
# client. Keep these defaults synchronized with the production authoring config.
# Options/environment variables exist for development and testing; students do
# not need to configure the package for normal use.

#' Build the Drillr runtime configuration
#'
#' Collects package options and the webhook environment-variable fallback into
#' the configuration list consumed by the student tutorial. This keeps runtime
#' defaults in one place while allowing development and tests to override them.
#'
#' @return A named list containing the course ID, queue size, topic priority,
#'   webhook URL, manifest path, and update notice expected by the tutorial
#'   runtime.
#'
#' @section Within-repo dependencies:
#' Uses `.drillr_default_webhook_url` as the final webhook URL fallback. It does
#' not call any within-repo functions.
#'
#' @section Direct callers and output consumers:
#' The packaged tutorial runtime calls this helper to obtain its client
#' configuration. Tests in `tests/testthat/test-config.R` also exercise its
#' output.
drillr_runtime_config <- function() {
  webhook_url <- getOption("drillr.webhook_url", NULL)
  if (is.null(webhook_url)) {
    webhook_url <- Sys.getenv(
      "DRILLR_WEBHOOK_URL",
      unset = .drillr_default_webhook_url
    )
  }

  list(
    course_id = getOption("drillr.course_id", "R Syntax Drill"),
    queue_size = as.integer(getOption("drillr.queue_size", 10L)),
    topic_priority = as.character(getOption(
      "drillr.topic_priority",
      c("vector_creation", "vector_indexing", "syntax_vocabulary")
    )),
    webhook_url = webhook_url,
    manifest_path = "",
    update_notice = ""
  )
}
