#' Run the Drillr syntax drills
#'
#' Launches the bundled learnr tutorial using the student's locally installed
#' copy of the drill bank while assignment selection and attempt history remain
#' centralized in the Drillr assignment service.
#'
#' @param as_rstudio_job Passed to [learnr::run_tutorial()]. The default lets
#'   learnr choose the normal RStudio behavior.
#' @param clean Passed to [learnr::run_tutorial()]. Set to `TRUE` to force the
#'   tutorial to be re-rendered before launch.
#' @return Invisibly, the value returned by [learnr::run_tutorial()].
#' @export
#'
#' @examples
#' \dontrun{
#' drills()
#' }
drills <- function(as_rstudio_job = NULL, clean = FALSE) {
  learnr::run_tutorial(
    name = "drills",
    package = "drillr",
    clean = clean,
    as_rstudio_job = as_rstudio_job
  )
}
