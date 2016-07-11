# Richard Wen (rwenite@gmail.com)
# Code for wrapper functions in the nbc4va package.


#' Calculate predicted CSMFs from a NBC model
#'
#' Obtains the predicted Cause Specific Mortality Fraction (CSMF) from a result \code{\link{nbc}} object.
#'
#' @inheritParams summary.nbc
#' @return out A numeric vector of the predicted CSMFs in which the names are the corresponding causes.
#'
#' @examples
#' library(nbc4va)
#' data(nbc4vaData)
#'
#' # Run naive bayes classifier on random train and test data
#' train <- nbc4vaData[1:50, ]
#' test <- nbc4vaData[51:100, ]
#' results <- nbc(train, test)
#'
#' # Obtain the predicted CSMFs
#' predCSMF <- csmf.nbc(results)
#'
#' @family wrapper functions
#' @export
csmf.nbc <- function(object) {
  brief <- suppressWarnings(summary(object))
  out <- brief$metrics.causes$CSMFpredicted
  names(out) <- brief$metrics.causes$Cause
  return(out)
}


#' Cause of death predictions from a NBC model
#'
#' Obtains the top causes of deaths for each testing case from a result \code{\link{nbc}} object.
#'
#' @inheritParams summary.nbc
#' @return out A dataframe of the top CODs:
#' \itemize{
#'   \item Columns: ID, COD
#'   \item ID (vectorof char): The ids for each testing case
#'   \item COD (vectorof char): The top prediction for each testing case
#' }
#'
#' @examples
#' library(nbc4va)
#' data(nbc4vaData)
#'
#' # Run naive bayes classifier on random train and test data
#' train <- nbc4vaData[1:50, ]
#' test <- nbc4vaData[51:100, ]
#' results <- nbc(train, test)
#'
#' # Obtain the top cause of death predictions for the test data
#' topPreds <- topCOD.nbc(results)
#'
#' @family wrapper functions
#' @export
topCOD.nbc <- function(object) {
  out <- object$pred[, c("CaseID", "Prediction1")]
  names(out) <- c("ID", "COD")
  return(out)
}

