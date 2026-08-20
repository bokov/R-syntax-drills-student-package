\name{drills}
\alias{drills}
\title{Run the Drillr syntax drills}
\usage{
drills(as_rstudio_job = NULL, clean = FALSE)
}
\arguments{
\item{as_rstudio_job}{Passed to \code{learnr::run_tutorial()}. The default lets learnr choose the normal RStudio behavior.}
\item{clean}{Passed to \code{learnr::run_tutorial()}. Set to \code{TRUE} to force the tutorial to be re-rendered before launch. Drillr also forces a clean render whenever cached question-bank content is active.}
}
\value{
Invisibly, the value returned by \code{learnr::run_tutorial()}.
}
\description{
Launches the stable packaged learnr shell using the newest locally available validated drill bank. When the grading service later reports that the bank is outdated, Drillr downloads the required pool and manifest to the user's cache and uses them on the next launch.
}
\examples{
\dontrun{
drills()
}
}
