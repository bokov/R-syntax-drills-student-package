.drillr_default_webhook_url <- paste0(
  "https://script.google.com/macros/s/",
  "AKfycbzpC-6D-CrclfMA8PZByTqw9jxOCZYbthx7NB0Y42LfeJc8uDIWj2P2FQd31PXN1Lz3xw/exec"
)

# The central service currently expects curriculum configuration from the
# client. Keep these defaults synchronized with the production authoring config.
# Options/environment variables exist for development and testing; students do
# not need to configure the package for normal use.
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
      c("vector_creation", "vector_indexing")
    )),
    webhook_url = webhook_url,
    bank_version = "",
    package_version = as.character(utils::packageVersion("drillr")),
    manifest_path = "",
    update_notice = ""
  )
}
