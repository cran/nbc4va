% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nbc4va_internal.R
\name{internalGetCSMFAcc}
\alias{internalGetCSMFAcc}
\title{Calculate CSMF accuracy}
\usage{
internalGetCSMFAcc(pred, obs)
}
\arguments{
\item{pred}{Chracter vector of predicted causes for each case.}

\item{obs}{Character vector of observed causes for each case.}
}
\value{
csmfa Numeric value of the overall CSMF accuracy (see \href{https://rrwen.github.io/nbc4va/methods}{Methods documentation}).
}
\description{
Calculates the overall CSMF accuracy given any number of predicted
cases and any number of observed cases.
}
\examples{
library(nbc4va)
pred <- c("HIV", "Stroke", "HIV", "Stroke")
obs <- c("HIV", "HIV", "Stroke", "Stroke")
csmfa <- nbc4va::internalGetCSMFAcc(pred, obs)

}
\seealso{
Other internal functions: 
\code{\link{internalGetCSMFMaxError}()},
\code{\link{internalGetCauseMetrics}()},
\code{\link{internalGetMetrics}()},
\code{\link{internalNBC}()}
}
\concept{internal functions}
\keyword{internal}
