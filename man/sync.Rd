% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sync.R
\name{sync}
\alias{sync}
\alias{sync.scheduler}
\title{Sync scheduler's schedule to current time}
\usage{
sync(scheduler)

\method{sync}{scheduler}(scheduler)
}
\arguments{
\item{scheduler}{A \code{\link{scheduler}} object.}
}
\value{
Returns TRUE when successful.
}
\description{
Syncs scheduler's schedule to current time
}
\section{Methods (by class)}{
\itemize{
\item \code{scheduler}: Sync scheduler's schedule to current time.
}}

\examples{
sync(scheduler(counter()))
}
\seealso{
\code{\link{Sys.time}}
}