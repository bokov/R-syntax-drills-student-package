#' Run the Drillr syntax drills
#'
#' Checks the published question manifest on GitHub before launch. If the
#' manifest differs from the local copy, Drillr downloads the matching runtime
#' question pool and re-renders the tutorial once. Otherwise it reuses the
#' existing local drill files and rendered tutorial.
#'
#' @param as_rstudio_job Passed to [learnr::run_tutorial()]. The default lets
#'   learnr choose the normal RStudio behavior.
#' @param clean If `TRUE`, force a fresh download of both the manifest and
#'   runtime question pool and force the tutorial to be re-rendered.
#' @return Invisibly, the value returned by [learnr::run_tutorial()].
#' @export
#'
#' @examples
#' \dontrun{
#' drills()
#' }
drills <- function(as_rstudio_job = NULL, clean = FALSE) {
  bank <- drillr_runtime_bank(force = isTRUE(clean), prelaunch = TRUE)

  learnr::run_tutorial(
    name = "drills",
    package = "drillr",
    clean = isTRUE(clean) || isTRUE(bank$updated),
    as_rstudio_job = as_rstudio_job
  )
}
