# Richard Wen (rwenite@gmail.com)
# Code for extra functions in the nbc4va package.


#' Translate open verbal autopsy arguments to train a NBC model
#'
#' A wrapper function for creating an nbc object with the parameters specified by
#' the \href{https://cran.r-project.org/package=openVA}{\pkg{openVA}} package.
#'
#' @param symps.train Dataframe of verbal autopsy train data.
#' \itemize{
#'   \item Columns (in order): ID, Cause, Symptom-1 to Symptom-n..
#'   \item ID (vectorof char): case identifiers
#'   \item Cause (vectorof char): observed causes for each case
#'   \item Symptom-n.. (vectorsof char): "Y" for presence, "" for absence, "." for missing
#' }
#' Example:
#' \tabular{ccccc}{
#'   ID \tab Cause \tab S1 \tab S2 \tab S3 \cr
#'   "a1" \tab "HIV" \tab "Y" \tab "" \tab "."\cr
#'   "b2" \tab "Stroke" \tab "." \tab "" \tab "Y"\cr
#'   "c3" \tab "HIV" \tab "Y" \tab "Y" \tab "."
#' }
#' @param symps.test Dataframe of verbal autopsy test data in the same format as \emph{symps.train}.
#' \itemize{
#'   \item If (\emph{causes.train} is (vectorof char)): \emph{symps.test} is assumed to not have a cause column
#' }
#' @param causes.train The train vector or column for the causes of death to use.
#' \itemize{
#'   \item If (vectorof char): cause of death values with number of values equal to nrow(\emph{symps.train});
#'         it is assumed that \emph{symps.test} has no causes of death column
#'   \item If (char): name of cause of death column from \emph{symps.train}
#' }
#' @param causes.table Character list of unique causes to learn.
#' \itemize{
#'   \item If (NULL): set to unique causes of death in \emph{symps.train}
#' }
#' @param ... Additional arguments to be passed to avoid errors if necessary.
#' @return nbc An \code{\link{nbc}} object with the following modifications:
#' \itemize{
#'   \item $id (vectorof char): set to test data ids
#'   \item $prob (matrixof numeric): set to a matrix of likelihood for each cause of death for the test cases
#'   \item $CSMF (vectorof char): set to the predicted CSMFs with names for the corresponding causes
#' }
#'
#'
#' @references
#' \itemize{
#'   \item Li Z, McCormick T, Clark S. openVA: Automated Method for Verbal Autopsy [Internet]. 2016. [cited 2016 Apr 29]. Available from: \url{https://cran.r-project.org/package=openVA}
#' }
#'
#' @examples
#' \dontrun{
#' library(openVA)  # install.packages("openVA")
#' library(nbc4va)
#'
#' # Obtain some openVA formatted data
#' data(RandomVA3) # cols: deathId, cause, symptoms..
#' train <- RandomVA3[1:100, ]
#' test <- RandomVA3[101:200, ]
#'
#' # Run naive bayes classifier on openVA data
#' results <- ova2nbc(train, test, "cause")
#'
#' # Obtain the probabilities and predictions
#' prob <- results$prob.causes
#' pred <- results$pred.causes
#' }
#'
#' @family extra functions
#' @include nbc4va_main.R
#' @export
ova2nbc <- function(symps.train, symps.test, causes.train, causes.table=NULL, ...) {

  # (Setup_OVA) Ensure ova args meet reqs
  # ----------------------------------------------------------------
  # Modified code from Richard Li (lizehang@gmail.com)

  # (Default_Causes) Set causes.train depending on its data type
  if (class(causes.train) == "character" && length(causes.train) == 1) {  # If char
    colindex <- match(causes.train, colnames(symps.train))  # COD col
    if (is.na(colindex)) {
      stop("Cannot find the cause-of-death column in train data.")
    }
    causes.train <- symps.train[, colindex]
    testSympsStart <- 3  # test causes assumed known

  } else if (class(causes.train) == "character" && length(causes.train) == nrow(symps.train)) {  # If (vectorof char)

    # (Add_TrainCauses) Add causes col to the train data in pos 2
    symps.train[ncol(symps.train) + 1] <- causes.train
    cols <- names(symps.train)
    symps.train <- symps.train[, c(cols[1], cols[length(cols)], cols[2:length(cols) - 1])]
    testSympsCol <- 2  # test causes assumed not known

  } else {
    stop("The argument [causes.train] is not a character or a character vector with the same length as [symps.train].")
  }

  # (Default_CODList) Set default of causes.table if NULL
  if (is.null(causes.table)) {
    causes.table <- unique(causes.train)
  }

  # (Check_TrainTest) Check that train/test data hve the same cols
  joint <- intersect(colnames(symps.train), colnames(symps.test))
  symps.test <- symps.test[, joint]
  symps.train <- symps.train[, joint]

  # (Setup_NBC) Convert OVA args to nbc args
  # ----------------------------------------------------------------

  # (Setup_OrigInput) Setup original inputs for nbc code
  train <- symps.train
  test <- symps.test

  # (Set_TrainSympCodes) Set the train symptom codes to numeric values
  # "Y" will be 1, and all other values will be 0
  trainSymps <- train[, 3:ncol(train)]
  trainSymps <- sapply(trainSymps, tolower)
  trainSymps[trainSymps == "y"] <- 1
  trainSymps[trainSymps != "1"] <- 0
  trainSymps <- sapply(trainSymps, as.numeric)
  train[, 3:ncol(train)] <- trainSymps

  # (Set_TestSympCodes) Set the test symptom codes to numeric values
  # "Y" will be 1, and all other values will be 0
  testSymps <- test[, testSympsStart:ncol(test)]
  testSymps <- sapply(testSymps, tolower)
  testSymps[testSymps == "y"] <- 1
  testSymps[testSymps != "1"] <- 0
  testSymps <- sapply(testSymps, as.numeric)
  test[, testSympsStart:ncol(test)] <- testSymps

  # (Return) Return a modified nbc object
  out <- nbc(train, test)
  out$id <- out$test.ids
  prob <- as.matrix(out$prob[, -1])
  row.names(prob) <- out$prob[, 1]
  out$prob <- prob
  out$CSMF <- csmf.nbc(out)
  return (out)
}

