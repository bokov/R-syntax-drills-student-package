#' Run the Drillr syntax drills
#'
#' Checks the published question manifest on GitHub before launch. If the
#' manifest differs from the local copy, Drillr downloads the matching runtime
#' question pool and re-renders the tutorial once. Otherwise it reuses the
#' existing local drill files and rendered tutorial.
#'
#' This is the package's student-facing entry point. It prepares the active
#' runtime question bank before handing control to `learnr` so the tutorial sees
#' a mutually consistent manifest and question pool.
#'
#' @param as_rstudio_job Passed to [learnr::run_tutorial()]. The default lets
#'   learnr choose the normal RStudio behavior.
#' @param clean If `TRUE`, force a fresh download of both the manifest and
#'   runtime question pool and force the tutorial to be re-rendered.
#' @return Invisibly, the value returned by [learnr::run_tutorial()].
#'
#' @section Within-repo dependencies:
#' Calls `drillr_runtime_bank()` to select, refresh, validate, and activate the
#' question bank used by the tutorial. It relies on the returned bank's
#' `updated` field to decide whether `learnr` must render a clean tutorial.
#'
#' @section Direct callers and output consumers:
#' This exported function is intended to be called directly by students or
#' instructors. No other package function directly calls it. Its return value is
#' simply the return value from [learnr::run_tutorial()] and is not consumed by
#' another within-repo function.
#'
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
