% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nbc4va_wrapper.R
\name{topCOD.nbc}
\alias{topCOD.nbc}
\title{Cause of death predictions from a NBC model}
\usage{
topCOD.nbc(object)
}
\arguments{
\item{object}{The result \code{\link{nbc}} object.}
}
\value{
out A dataframe of the top CODs:
\itemize{
  \item Columns: ID, COD
  \item ID (vectorof char): The ids for each testing case
  \item COD (vectorof char): The top prediction for each testing case
}
}
\description{
Obtains the top causes of deaths for each testing case from a result \code{\link{nbc}} object.
}
\examples{
library(nbc4va)
data(nbc4vaData)

# Run naive bayes classifier on random train and test data
train <- nbc4vaData[1:50, ]
test <- nbc4vaData[51:100, ]
results <- nbc(train, test)

# Obtain the top cause of death predictions for the test data
topPreds <- topCOD.nbc(results)

}
\seealso{
Other wrapper functions: 
\code{\link{csmf.nbc}()}
}
\concept{wrapper functions}
