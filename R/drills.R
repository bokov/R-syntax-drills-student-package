#' Run the Drillr syntax drills
#'
#' Launches the stable packaged learnr shell using the newest locally available
#' validated drill bank. When the grading service later reports that the bank is
#' outdated, Drillr downloads the required pool and manifest to the user's cache
#' and uses them on the next launch.
#'
#' @param as_rstudio_job Passed to [learnr::run_tutorial()]. The default lets
#'   learnr choose the normal RStudio behavior.
#' @param clean Passed to [learnr::run_tutorial()]. Set to `TRUE` to force the
#'   tutorial to be re-rendered before launch. Drillr also forces a clean render
#'   whenever cached question-bank content is active.
#' @return Invisibly, the value returned by [learnr::run_tutorial()].
#' @export
#'
#' @examples
#' \dontrun{
#' drills()
#' }
drills <- function(as_rstudio_job = NULL, clean = FALSE) {
  bank <- drillr_runtime_bank()
  drillr_activate_bank(bank)

  learnr::run_tutorial(
    name = "drills",
    package = "drillr",
    clean = isTRUE(clean) || identical(bank$source, "cache"),
    as_rstudio_job = as_rstudio_job
  )
}
