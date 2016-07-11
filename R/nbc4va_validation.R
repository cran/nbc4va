# Richard Wen (rwenite@gmail.com)
# Code for validation functions in the nbc4va package.


#' Check arguments for nbc()
#'
#' Performs checks to ensure that the arguments passed to \code{\link{internalNBC}} are correct.
#' This function will also auto-clean when appropriate, and display
#' warning messages of the cleaning tasks.
#'
#' @details The following checks are applied to \emph{train} and \emph{test} to ensure they:
#' \itemize{
#'   \item are a dataframe
#'   \item have required number of rows and columns
#'   \item have required data types for each column
#'   \item have required symptom values
#'   \item are in the same format
#'   \item have unique ids
#' }
#'
#' @param train Dataframe of verbal autopsy train data (See \code{\link{nbc4vaHelpData}}).
#' \itemize{
#'   \item Columns (in order): ID, Cause, Symptom-1 to Symptom-n..
#'   \item ID (vectorof char): unique case identifiers
#'   \item Cause (vectorof char): observed causes for each case
#'   \item Symptom-n.. (vectorsof (1 OR 0)): 1 for presence, 0 for absence, other values are treated as unknown
#'   \item Unknown symptoms are imputed randomly from distributions of 1s and 0s per symptom column; if no 1s or 0s exist then the column is removed
#' }
#' Example:
#' \tabular{ccccc}{
#'   ID \tab Cause \tab S1 \tab S2 \tab S3 \cr
#'   "a1" \tab "HIV" \tab 1 \tab 0 \tab 0\cr
#'   "b2" \tab "Stroke" \tab 0 \tab 0 \tab 1\cr
#'   "c3" \tab "HIV" \tab 1 \tab 1 \tab 0
#' }
#' @param test Dataframe of verbal autopsy test data in the same format as \emph{train} except if causes are not known:
#' \itemize{
#'   \item The 2nd column (Cause) can be omitted if \emph{known} is FALSE
#' }
#' @param known TRUE to indicate that the \emph{test} causes are available in the 2nd column and FALSE to indicate that they are not known
#' @param assume TRUE to set all symptoms not equal to 1 as 0 and FALSE to raise error if symptoms are not 0 or 1. This takes priority over \emph{unknown}.
#' @param unknown A single integer value which determines if a symptom is unknown as to if is present or absent.
#' \itemize{
#'   \item The unknown values are substituted according to the proportion of the 1s and 0s per column
#'   \item Setting this to NULL will ignore this substitution
#'   \item All other values that are not the unknown value or 1 will be set to 0 after the substition
#' }
#' @return out A list object containing the checked inputs:
#' \itemize{
#'   \item $train: dataframe of id, cause and symptoms
#'   \item $test: dataframe of id, cause and symptoms in the same format as \emph{train}
#'   \item $known: TRUE if the \emph{test} causes are known or FALSE if not
#' }
#'
#' @examples
#' library(nbc4va)
#' data(nbc4vaData)
#'
#' # Check train and test inputs, error if it does not pass check
#' train <- nbc4vaData[1:50, ]
#' test <- nbc4vaData[51:100, ]
#' checked <- nbc4va:::internalCheckNBC(train, test)
#' train <- checked$train
#' test <- checked$test
#'
#' @importFrom methods is
#' @family validation functions
#' @keywords internal
internalCheckNBC <- function(train, test, known=TRUE, assume=FALSE, unknown=99) {

  # (Dataframe_Check) Check if dataframes are correctly formatted
  # ----------------------------------------------------------------

  # (Check_train_DF) Check if train is in correct dataframe format
  if (!is.data.frame(train)) {
    stop("Input train data is not a dataframe.")
  }
  if (ncol(train) < 3) {
    stop("Input train data must have at least 3 columns (ID, Cause, Symptom).")
  }
  if (nrow(train) < 1) {
    stop("Input train data must have at least 1 row or sample.")
  }
  if (!is.character(train[, 1])) {
    train[, 1] <- as.character(train[, 1])
    warning(paste("Column 1 (", names(train)[1], ") of train has been converted to type (vectorof char).", sep=""))
  }
  if (!is.character(train[, 2])) {
    train[, 2] <- as.character(train[, 2])
    warning(paste("Column 2 (", names(train)[2], ") of train has been converted to type (vectorof char).", sep=""))
  }

  # (Check_test_DF) Check if test is in correct dataframe format
  if (!is.data.frame(test)) {
    stop("Input test data is not a dataframe.")
  }
  if (ncol(test) < 2) {
    stop("Input test data must have at least 2 columns (ID, Symptom).")
  }
  if (nrow(test) < 1) {
    stop("Input test data must have at least 1 row or sample.")
  }
  if (!known) {  # if not obs causes for test data, create dummy cols
    dummyCol <- ncol(test) + 1
    test[, dummyCol] <- "_unknown"
    test <- test[, c(1, dummyCol, 3:dummyCol-1)]
  }
  if (!is.character(test[, 1])) {
    test[, 1] <- as.character(test[, 1])
    warning(paste("Column 1 (", names(test)[1], ") of test has been converted to type (vectorof char).", sep=""))
  }
  if (!is.character(test[, 2])) {
    test[, 2] <- as.character(test[, 2])
    warning(paste("Column 2 (", names(test)[2], ") of test has been converted to type (vectorof char).", sep=""))
  }

  # (Set_99) Set values that are not 1s and 0s to 99
  if (!is.null(unknown)) {

    # (Set_99_Train) Set 99s for training set
    trainUK <- train[, 3:ncol(train)]
    if (is.vector(trainUK)) {
      trainUK <- data.frame(x=trainUK)
      names(trainUK) <- names(train)[3]
    }
    for (s in 1:ncol(trainUK)) {
      trainUK[, s] <- sapply(trainUK[, s], function(si) if (si %in% c(0, 1)) si else unknown)
    }
    train[, 3:ncol(train)] <- trainUK
    testUK <- test[, 3:ncol(test)]

    # (Set_99_Test) Set 99s for testing set
    if (is.vector(testUK)) {
      testUK <- data.frame(x=testUK)
      names(testUK) <- names(test)[3]
    }
    for (s in 1:ncol(testUK)) {
      testUK[, s] <- sapply(testUK[, s], function(si) if (si %in% c(0, 1)) si else unknown)
    }
    test[, 3:ncol(test)] <- testUK
  }

  # (Symps_Check) Check the symptoms columns
  # ----------------------------------------------------------------

  # (Symps_Train) Check train symptoms are correct
  idx <- 3:ncol(train)
  for (isymp in idx) {
    if (!is.numeric(train[, isymp])) {
      if (is.factor(train[, isymp])) {
        train[, isymp] <- as.numeric(levels(train[, isymp]))[train[, isymp]]
      } else {
        train[, isymp] <- as.numeric(train[, isymp])
      }
      warning(paste("Column ", isymp, " (", names(train)[isymp], ") of train has been converted to type (vectorof numeric).", sep=""))
    }
    if (assume) {
      train[, isymp][train[, isymp] != 1] <- 0
    }
    if (!all(train[, isymp] %in% c(0, 1)) && is.null(unknown)) {
      stop(paste("Column ", isymp, " (", names(train)[isymp], ") of train does not contain all 0 or 1 values.", sep=""))
    }
  }

  # (Symps_Test) Check test symptoms are correct
  idx <- 3:ncol(test)
  for (isymp in idx) {
    if (!is.numeric(test[, isymp])) {
      if (is.factor(test[, isymp])) {
        test[, isymp] <- as.numeric(levels(test[, isymp]))[test[, isymp]]
      } else {
        test[, isymp] <- as.numeric(test[, isymp])
      }
      warning(paste("Column ", isymp, " (", names(train)[isymp], ") of test has been converted to type (vectorof numeric).", sep=""))
    }
    if (assume) {
      test[, isymp][test[, isymp] != 1] <- 0
    }
    if (!all(test[, isymp] %in% c(0, 1)) && is.null(unknown)) {
      warning(paste("Column ", isymp, " (", names(train)[isymp], ") of test does not contain all 0 or 1 values.", sep=""))
    }
  }

  # (Sub_Unknowns) Substitute unknowns for known distributions
  if (!is.null(unknown)) {

    # (Sub_Train) Substitute unknowns for training set
    trainSub <- internalSubAsRest(train, unknown, cols=3:ncol(train), ignore=NULL, removal=TRUE)
    train <- trainSub$dataset
    if (!is.null(trainSub$removed)) {
      test <- test[, -trainSub$removed]
    }

    # (Sub_Test) Substitute unknowns for testing set
    testSub <- internalSubAsRest(test, unknown, cols=3:ncol(test), ignore=NULL, removal=TRUE)
    test <- testSub$dataset
    if (!is.null(testSub$removed)) {
      train <- train[, -testSub$removed]
    }
  }

  # (Finalize) Check dimensions and return auto-cleaned inputs
  # ----------------------------------------------------------------

  # (Dim_Check) Check dimensions consistency
  if (ncol(train) != ncol(test)) {
    stop("The columns of the input train and test data do not match.")
  }

  # (Uniq_Check) Check that ids are unique
  trainUniq <- !duplicated(train[, 1])
  testUniq <- !duplicated(test[, 1])
  if (!all(trainUniq)) {
    stop(paste("The train data IDs are not unique (duplicates found:", paste(names(trainUniq[trainUniq==FALSE]), collapse=", "), ")", sep=""))
  }
  if (!all(testUniq)) {
    stop(paste("The test data IDs are not unique (duplicates found:", paste(names(testUniq[testUniq==FALSE]), collapse=", "), ")", sep=""))
  }

  # (Return) Return the checked input arguments
  out <- list(train=train,
              test=test,
              known=known,
              assume=assume)
  return(out)
}


#' Check arguments for summary.nbc()
#'
#' Performs checks to ensure that the arguments passed to \code{\link{summary.nbc}} are correct.
#' This function will perform automatic data type conversions, and display warnings when appropriate.
#'
#' @details The following checks are applied:
#' \itemize{
#'   \item \emph{object} is of class "nbc"
#'   \item \emph{top} is a numeric value
#'   \item \emph{id} is NULL or a character, and exists in the test data
#'   \item \emph{csmfa.obs} is NULL or a character or vector of characters
#' }
#'
#' @inheritParams internalGetCauseMetrics
#' @inheritParams internalGetMetrics
#' @param object The result \code{\link{nbc}} object.
#' @param top A number that produces top causes depending on \emph{id}:
#' \itemize{
#'   \item If (\emph{id} is char): provide the \emph{top} causes of the case by probability
#'   \item If (\emph{id} is NULL): provide the \emph{top} causes by predicted Cause Specific Mortality Fractions (CSMF)
#' }
#' @param id A character representing a case id in the test data.
#' @param ... Additional arguments to be passed if applicable
#' @return out A list object containing the checked inputs:
#' \itemize{
#'   \item $object: an \code{\link{nbc}} object
#'   \item $top: numeric value
#'   \item $id: NULL or character value
#'   \item $csmfa.obs: NULL or character vector
#' }
#'
#' @examples
#' library(nbc4va)
#' data(nbc4vaData)
#'
#' # Create an nbc
#' train <- nbc4vaData[1:50, ]
#' test <- nbc4vaData[51:100, ]
#' results <- nbc(train, test)
#'
#' # Check the inputs before passing on to summary
#' checked <- nbc4va:::internalCheckNBCSummary(results, 5, "g85")
#' results <- checked$object
#' top <- checked$top
#' id <- checked$id
#' csmfa.obs <- checked$csmfa.obs
#'
#' @importFrom methods is
#' @family validation functions
#' @keywords internal
internalCheckNBCSummary <- function(object, top=5, id=NULL, csmfa.obs=NULL, ...) {
  if (!is(object, "nbc")) {
    stop("The object must be of type \"nbc\".")
  }
  if (!is.numeric(top)) {
    top <- as.numeric(top)
    if (is.na(top)) {
      stop("Argument top must be numeric.")
    }
    warning("Argument top has been converted to numeric.")
  }
  if (!is.character(id) && !is.null(id)) {
    id <- as.character(id)
    if (is.na(id)) {
      stop("Argument id must be a character.")
    }
    warning("Argument id has been converted to character.")
  }
  if (!is.null(id) && !id %in% object$test.ids){
    stop(paste(id, "does not exist in the test data."))
  }
  if (!is.character(csmfa.obs) && !is.null(csmfa.obs)) {
    csmfa.obs <- as.character(csmfa.obs)
    if (is.na(csmfa.obs)) {
      stop("Argument csmfa.obs must be of character type.")
    }
    warning("Argument csmfa.obs has been converted to character.")
  }
  out <- list(object=object,
              top=top,
              id=id,
              csmfa.obs=csmfa.obs)
  return(out)
}

