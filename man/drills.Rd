\name{drills}
\alias{drills}
\title{Run the Drillr syntax drills}
\usage{
drills(as_rstudio_job = NULL, clean = FALSE)
}
\arguments{
\item{as_rstudio_job}{Passed to \code{learnr::run_tutorial()}. The default lets learnr choose the normal RStudio behavior.}
\item{clean}{Passed to \code{learnr::run_tutorial()}. Set to \code{TRUE} to force the tutorial to be re-rendered before launch.}
}
\value{
Invisibly, the value returned by \code{learnr::run_tutorial()}.
}
\description{
Launches the bundled learnr tutorial using the student's locally installed copy of the drill bank while assignment selection and attempt history remain centralized in the Drillr assignment service.
}
\examples{
\dontrun{
drills()
}
}
