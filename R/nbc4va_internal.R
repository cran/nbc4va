# Richard Wen
# rrwen.dev@gmail.com
# Code for internal functions in the nbc4va package.


#' NBC algorithm source code
#'
#' Performs Naive Bayes Classification given train and test (validation) datasets, as well as
#' additional information for the train and test data.
#'
#' @details This function was built on code provided by \href{http://bmcmedicine.biomedcentral.com/articles/10.1186/s12916-015-0521-2}{Miasnikof \emph{et al} (2015)}.
#' Edits to the code included the following improvements:
#' \itemize{
#'   \item Causes can be character type
#'   \item Matrix operations for speed
#'   \item Removal of order dependence for causes
#'   \item Refactoring of variable names for clarity
#'   \item Included list structure of model data and details
#'   \item Argument validation
#' }
#'
#' @inheritParams internalCheckNBC
#' @return out The result list object containing:
#' \itemize{
#'   \item $prob.causes (vectorof double): the probabilities for each test case prediction by case id
#'   \item $pred.causes (vectorof char): the predictions for each \emph{test} case by case id
#'   \item Additional values:
#'   \itemize{
#'     \item * indicates that the value is only available if \emph{test} causes are known
#'     \item $train (dataframe): the input \emph{train} data
#'     \item $train.ids (vectorof char): the ids of the \emph{train} data
#'     \item $train.causes (vectorof char): the causes of the \emph{train} data by case id
#'     \item $train.samples (double): the number of input \emph{train} samples
#'     \item $test (dataframe): the input \emph{test} data
#'     \item $test.ids (vectorof char): the ids of the \emph{test} data
#'     \item $test.causes* (vectorof char): the causes of the \emph{test} data by case id
#'     \item $test.samples (double): the number of input \emph{test} samples
#'     \item $test.known (logical): whether the \emph{test} causes are known
#'     \item $symptoms (vectorof char): all unique symptoms in order
#'     \item $causes (vectorof char): all possible unique causes of death
#'     \item $causes.train (vectorof char): all unique causes of death in the \emph{train} data
#'     \item $causes.test* (vectorof char): all unique causes of death in the \emph{test} data
#'     \item $causes.pred (vectorof char): all unique causes of death in the predicted cases
#'     \item $causes.obs* (vectorof char): all unique causes of death in the observed cases
#'     \item $pred (dataframe): a table of predictions for each \emph{test} case, sorted by probability
#'       \itemize{
#'         \item Columns (in order): CaseID, TrueCause, Prediction-1 to Prediction-n..
#'         \item CaseID (vectorof char): case identifiers
#'         \item TrueCause* (vectorof char): the observed causes of death
#'         \item Prediction-n.. (vectorsof char): the predicted causes of death,
#'         where Prediction1 is the most probable cause, and Prediction-n is the least probable cause
#'      }
#'     Example:
#'     \tabular{cccc}{
#'       CaseID \tab Prediction1 \tab Prediction2 \cr
#'       "a1" \tab "HIV" \tab "Stroke" \cr
#'       "b2" \tab "Stroke" \tab "HIV" \cr
#'       "c3" \tab "HIV" \tab "Stroke"
#'     }
#'     \item $obs* (dataframe): a table of observed causes matching \emph{$pred} for each \emph{test} case
#'       \itemize{
#'         \item Columns (in order): CaseID, TrueCause
#'         \item CaseID (vectorof char): case identifiers
#'         \item TrueCause (vectorof char): the actual cause of death if applicable
#'      }
#'     Example:
#'     \tabular{cccc}{
#'       CaseID \tab TrueCause \cr
#'       "a1" \tab "HIV" \cr
#'       "b2" \tab "Stroke" \cr
#'       "c3" \tab "HIV"
#'     }
#'     \item $obs.causes* (vectorof char): all observed causes of death by case id
#'     \item $prob (dataframe): a table of probabilities of each cause for each test case
#'       \itemize{
#'        \item Columns (in order): CaseID, Cause-1 to Cause-n..
#'        \item CaseID (vectorof char): case identifiers
#'        \item Cause-n.. (vectorsof double): probabilies for each cause of death
#'       }
#'     Example:
#'     \tabular{ccc}{
#'       CaseID \tab HIV \tab Stroke \cr
#'       "a1" \tab 0.5 \tab 0.5 \cr
#'       "b2" \tab 0.3 \tab 0.7 \cr
#'       "c3" \tab 0.9 \tab 0.1
#'     }
#'   }
#' }
#'
#' @author Pierre Miasnikof (Original), Vasily Giannakeas (Original), Richard Wen (Edits) <\email{wenr@@smh.ca}>
#' @references
#' \itemize{
#'   \item Miasnikof P, Giannakeas V, Gomes M, Aleksandrowicz L, Shestopaloff AY, Alam D, Tollman S, Samarikhalaj, Jha P. Naive Bayes classifiers for verbal autopsies: comparison to physician-based classification for 21,000 child and adult deaths. BMC Medicine. 2015;13:286. doi:10.1186/s12916-015-0521-2.
#' }
#'
#' @examples
#' library(nbc4va)
#' data(nbc4vaData)
#'
#' # Create naive bayes classifier on random train and test data
#' # Set "known" to indicate whether or not "test" causes are known
#' train <- nbc4vaData[1:50, ]
#' test <- nbc4vaData[51:100, ]
#' results <- nbc4va:::internalNBC(train, test, known=TRUE)
#'
#' # Obtain the probabilities and predictions
#' prob <- results$prob.causes
#' pred <- results$pred.causes
#'
#' @importFrom methods is
#' @include nbc4va_validation.R
#' @family internal functions
#' @keywords internal
internalNBC <- function(train, test, known=TRUE){

  # (Prepare) Prepare variables required for NBC ----

  # (Check_Inputs) Check that inputs are correct
  checked <- internalCheckNBC(train, test, known=known)
  train <- checked$train
  test <- checked$test
  known <- checked$known

  # (Train) Train data variables
  trainIDs <- train[, 1]
  trainCauses <- train[, 2]
  trainSymptoms <- as.matrix(train[, -c(1, 2)])
  trainSamples <- nrow(train)

  # (Test) Test data variables
  testIDs <- test[, 1]
  testCauses <- test[, 2]
  testSymptoms <- as.matrix(test[, -c(1, 2)])
  testSamples <- nrow(test)

  # (Cause_Symptoms) Cause and symptom variables
  causes <- unique(c(trainCauses, testCauses))
  causesTotal <- length(causes)
  causesTrain <- unique(trainCauses)
  causesTest <- unique(testCauses)
  causesNonTrain <- causes[!causes %in% causesTrain]
  symptoms <- names(train)[3:ncol(train)]

  # (Run_Algorithm) Obtain NBC predictions and probabilities ----

  # (Train_Symptom_Counts) Sum symptoms per train cause, zero sums for causes not in train
  trainSSZero <- matrix(0, length(causesNonTrain), length(symptoms), dimnames=list(causesNonTrain, symptoms))
  trainSS <- rbind(rowsum(trainSymptoms, trainCauses), trainSSZero)  # train symptom sums

  # (Train_Cause_Counts) Train cause counts, zero count for causes not in train
  trainCC <- table(factor(trainCauses, levels=causes))[row.names(trainSS)]

  # (Train_Prob) Compute probabilities of each train symptom per cause
  # Probsympj = sum(sympj) / Nj
  trainProb <- sapply(1:nrow(trainSS), function(i, ss, cc) ss[i, ] / cc[i], ss=trainSS, cc=trainCC)
  trainProb[is.na(trainProb)] <- 0  # zero div to 0
  trainProbNot <-  1 - trainProb  # non-occuring train symptoms prob

  # (Test_Flip) Flip symptom positives and negatives
  testSymptomsFlip <- testSymptoms
  testSymptomsFlip[testSymptomsFlip == 1] <- -1
  testSymptomsFlip[testSymptomsFlip == 0] <- 1
  testSymptomsFlip[testSymptomsFlip == -1] <- 0

  # (Pred) Initialize prediction dataframe for test data
  pred <- matrix(NA, testSamples, causesTotal)
  colnames(pred) <- paste0("Prediction", 1:causesTotal)
  row.names(pred) <- testIDs

  # (Prob) Initialize probability dataframe for test dat
  prob <- pred
  colnames(prob) <- row.names(trainSS)

  # (Pred_Prob) Calculate predictions and probabilities
  for (case in 1:testSamples) {
    testProb <- testSymptoms[case, ] * trainProb + testSymptomsFlip[case, ] * trainProbNot
    caseProb <- apply(testProb, 2, prod) * (trainCC / trainSamples)
    prob[case, ] <- caseProb
    pred[case, ] <- names(sort(caseProb, decreasing=TRUE))
  }

  # (Format_Output) List to provide details of nbc algorithm ----

  # (Data_Conversion) Convert datatypes and structures to match output specifications
  trainIDs <- as.character(trainIDs)
  testIDs <- as.character(testIDs)
  trainCauses <- as.character(trainCauses)
  testCauses <- as.character(testCauses)
  pred <- data.frame(CaseID=testIDs, TrueCause=testCauses, pred, row.names=NULL, stringsAsFactors = FALSE)
  pred[, 2:ncol(pred)] <- sapply(pred[, 2:ncol(pred)], as.character)
  prob <- data.frame(CaseID=testIDs, prob, row.names=NULL, stringsAsFactors = FALSE)
  prob[, 2:ncol(prob)] <- sapply(prob[, 2:ncol(prob)], as.numeric)

  # (Format_Causes) Include case ids in cause vectors
  names(trainCauses) <- trainIDs  # train causes vector with ids as names
  names(testCauses) <- testIDs  # test causes vector with ids as names

  # (Format_ProbVect) Probabitilies of predictions for test data
  probv <- apply(prob[, 2:ncol(prob)], 1, max)
  names(probv) <- prob[, 1]

  # (Format_PredVect) Vector of predictions for test data
  predv <- pred[, 3]
  names(predv) <- pred[, 1]

  # (Format_Obs) Vector of observed causes for test data
  obsv <- pred[, 2]
  names(obsv) <- pred[, 1]

  # (Data_List) Create a list structure of algorithm details
  out <- list(train=train,
              train.ids=trainIDs,
              train.causes=trainCauses,
              train.samples=trainSamples,
              test=test,
              test.ids=testIDs,
              test.causes=testCauses,
              test.samples=testSamples,
              test.known=known,
              symptoms=symptoms,
              causes=causes,
              causes.train=causesTrain,
              causes.test=causesTest,
              causes.pred=unique(predv),
              causes.obs=unique(obsv),
              pred=pred[, -2],
              pred.causes=predv,
              obs=pred[, c(1, 2)],
              obs.causes=obsv,
              prob=prob,
              prob.causes=probv)

  # (Remove_Logic) Remove Irrelevant test Items if Causes are not Known
  known <- out$test.known
  if (!known) {
    out$causes <- unique(train[, 2])
    out$causes.test <- NULL
    out$test <- test[, -2]
    out$test.causes <- NULL
    out$obs <- NULL
    out$obs.causes <- NULL
    out$causes.obs <- NULL
  }
  return(out)
}


#' Calculate performance metrics table per cause
#'
#' A table providing performance metrics per unique cause based on input predicted and observed cases.
#'
#' @details This code is built on the original performance metrics code provided by Dr. Mireille Gomes.
#'
#' @param pred Chracter vector of predicted causes for each case.
#' @param obs Character vector of observed causes for each case.
#' @param causes Character vector of all possible causes including ones that are not in the \emph{pred} or \emph{obs}.
#' @return out Dataframe of a performance metrics per cause (see \href{https://rrwen.github.io/nbc4va/methods}{Methods documentation}):
#' \itemize{
#'   \item Columns: Cause, TruePositives, TrueNegatives, FalsePositives, FalseNegatives, PredictedFrequency, ObservedFrequency, Sensitivity, CSMFpredicted, CSMFobserved
#'   \item Cause (vectorof char): The unique causes from both the \emph{obs} and \emph{pred} inputs
#'   \item Sensitivity (vectorof double): the sensitivity for a cause
#'   \item CSMFpredicted (vectorof double): the cause specific mortality fraction for a cause given the predicted deaths
#'   \item CSMFobserved (vectorof double): the cause specific mortality fraction for a cause given the observed deaths
#'   \item TruePositives (vectorof double): The total number of true positives per cause
#'   \item TrueNegatives (vectorof double): The total number of true negatives per cause
#'   \item FalsePositives (vectorof double): The total number of false positives per cause
#'   \item FalseNegatives (vectorof double): The total number of false negatives per cause
#'   \item PredictedFrequency (vectorof double): The occurence of a cause in the \emph{pred} input
#'   \item ObservedFrequency (vectorof double): The occurence of a cause in the \emph{obs} input
#' }
#' Example:
#' \tabular{cccc}{
#'   Cause \tab Sensitivity \tab Metric-n.. \cr
#'   HIV \tab 0.5 \tab #.. \cr
#'   Stroke \tab 0.5 \tab #..
#' }
#'
#' @examples
#' library(nbc4va)
#' pred <- c("HIV", "Stroke", "HIV", "Stroke")
#' obs <- c("HIV", "HIV", "Stroke", "Stroke")
#' cmetrics <- nbc4va:::internalGetCauseMetrics(pred, obs)
#'
#' @family internal functions
#' @keywords internal
internalGetCauseMetrics <- function(pred, obs, causes=unique(c(pred, obs))) {

  # (Unmatched_Error) Observed and predicted do not match up
  if (length(pred) != length(obs)) {
    stop("The lengths of the predicted and observed data are not equal.")
  }

  # (TrueFalse_PosNeg) Calculate t/f pos/neg as in Miaskinof et al (2015) ----

  # (Conf_Matrix) Create a confusion matrix of t/f pos/neg per cause
  metrics <- c("TruePositives", "TrueNegatives", "FalsePositives", "FalseNegatives")
  mx <- matrix(0, length(causes), length(metrics))
  row.names(mx) <- causes
  colnames(mx) <- metrics

  # (Calc_PosNeg) Fill confusion matrix of t/f pos/neg per cause
  for (idx in 1:length(obs)) {

    # (PredObs_Cause) Individual predicted and observed causes
    predCause <- pred[[idx]]
    obsCause <- obs[[idx]]

    # (PosNeg_Count) Count t/f pos/neg per cause based on conditions
    if (predCause == obsCause) {
      mx[predCause, "TruePositives"] <- mx[predCause, "TruePositives"] + 1
      notPredCause <- causes[causes != obsCause]
      mx[notPredCause, "TrueNegatives"] <- mx[notPredCause, "TrueNegatives"] + 1
    } else {
      mx[predCause, "FalsePositives"] <- mx[predCause, "FalsePositives"] + 1
      mx[obsCause, "FalseNegatives"] <- mx[obsCause, "FalseNegatives"] + 1
      neitherCause <- causes[!causes %in% c(predCause, obsCause)]
      mx[neitherCause, "TrueNegatives"] <- mx[neitherCause, "TrueNegatives"] + 1
    }
  }

  # (Calculate_Metrics) Calculate performance metrics per cause ----

  # (Frequencies) Include frequencies in table
  out <- data.frame(Cause=row.names(mx), mx, stringsAsFactors=FALSE)
  row.names(out) <- NULL
  out$PredictedFrequency <- as.numeric(table(factor(pred, levels=causes)))
  out$ObservedFrequency <- as.numeric(table(factor(obs, levels=causes)))

  # (Variables) Variables, cause j
  tpj <- out$TruePositives
  tnj <- out$TrueNegatives
  fpj <- out$FalsePositives
  fnj <- out$FalseNegatives
  njP <- out$PredictedFrequency
  njO <- out$ObservedFrequency
  nP <- length(pred)
  nO <- length(obs)
  N <- length(causes)  # num of causes

  # (Sens) Sensitivity
  out$Sensitivity <- tpj / (tpj + tnj)

  # (CSMF) CSMF predicted and observed
  out$CSMFpredicted <- njP / nP
  out$CSMFobserved <- njO / nO

  # (Return) Resulting cause dataframe, reorder sens and csmfs to beginning
  first <- c(1, (ncol(out) - 2):ncol(out))
  last <- 2:(ncol(out) - 3)
  out <- out[, c(first, last)]
  return(out)
}


#' Calculate CSMF maximum error
#'
#' Calculates the CSMF maximum error given a set of observed cases.
#'
#' @inheritParams internalGetCauseMetrics
#' @return csmfMaxError Numeric value of the CSMF maximum error (see \href{https://rrwen.github.io/nbc4va/methods}{Methods documentation}).
#'
#' @examples
#' library(nbc4va)
#' obs <- c("HIV", "HIV", "Stroke", "Stroke")
#' maxerror <- nbc4va:::internalGetCSMFMaxError(obs)
#'
#' @family internal functions
#' @keywords internal
internalGetCSMFMaxError <- function(obs) {
  causes <- unique(obs)
  CSMFtruej <- table(factor(obs, levels=causes)) / length(obs)
  return(2 * (1 - min(CSMFtruej)))
}


#' Calculate CSMF accuracy
#'
#' Calculates the overall CSMF accuracy given any number of predicted
#' cases and any number of observed cases.
#'
#' @inheritParams internalGetCauseMetrics
#' @return csmfa Numeric value of the overall CSMF accuracy (see \href{https://rrwen.github.io/nbc4va/methods}{Methods documentation}).
#'
#' @examples
#' library(nbc4va)
#' pred <- c("HIV", "Stroke", "HIV", "Stroke")
#' obs <- c("HIV", "HIV", "Stroke", "Stroke")
#' csmfa <- nbc4va:::internalGetCSMFAcc(pred, obs)
#'
#' @family internal functions
#' @keywords internal
internalGetCSMFAcc <- function(pred, obs) {

  # (CSMF_TruePred) Calculate CSMF by cause for observed and predicted cases
  causes <- unique(c(obs, pred))
  CSMFtruej <- table(factor(obs, levels=causes)) / length(obs)
  CSMFpredj <- table(factor(pred, levels=causes)) / length(pred)

  # (CSMF_Acc) Calculate CSMF Accuracy
  CSMFMaxError <- internalGetCSMFMaxError(obs)
  csmfa <- 1 - (sum(abs(CSMFtruej - CSMFpredj)) / CSMFMaxError)
  return(csmfa)
}


#' Calculate overall performance metrics
#'
#' A vector providing overall performance metrics based on input predicted and observed cases.
#'
#' @details Developer Note: Depends on the \code{\link{internalGetCSMFAcc}} function to get the CSMF Accuracy.
#'
#' @inheritParams internalGetCauseMetrics
#' @param csmfa.obs A character vector of the true causes for calculating the CSMF accuracy.
#' @param causeMetrics Dataframe of a performance metrics per cause (see \code{\link{internalGetCauseMetrics}}):
#' \itemize{
#'   \item Columns: Cause, TruePositives, TrueNegatives, FalsePositives, FalseNegatives, PredictedFrequency, ObservedFrequency, Sensitivity, CSMFpredicted, CSMFobserved
#'   \item Cause (vectorof char): The unique causes from both the \emph{obs} and \emph{pred} inputs
#'   \item TruePositives (vectorof double): The total number of true positives per cause
#'   \item TrueNegatives (vectorof double): The total number of true negatives per cause
#'   \item FalsePositives (vectorof double): The total number of false positives per cause
#'   \item FalseNegatives (vectorof double): The total number of false negatives per cause
#'   \item PredictedFrequency (vectorof double): The occurence of a cause in the \emph{pred} input
#'   \item ObservedFrequency (vectorof double): The occurence of a cause in the \emph{obs} input
#'   \item Sensitivity (vectorof double): the sensitivity for a cause
#'   \item CSMFpredicted (vectorof double): the cause specific mortality fraction for a cause given the predicted deaths
#'   \item CSMFobserved (vectorof double): the cause specific mortality fraction for a cause given the observed deaths
#' }
#' @return metrics Named numeric vector of performance metrics (see \href{https://rrwen.github.io/nbc4va/methods}{Methods documentation}):
#' \itemize{
#'   \item Names: TruePositives, TrueNegatives, FalsePositives, FalseNegatives, Accuracy, Sensitivity, Specificity, PCCC, CSMFMaxError, CSMFaccuracy
#'   \item TruePositives (double): total number of true positives
#'   \item TrueNegatives (double): total number of true negatives
#'   \item FalsePositives (double): total number of false positives
#'   \item FalseNegatives (double): total number of false negatives
#'   \item Sensitivity (double): the overall sensitivity
#'   \item PCCC (double): the partial chance corrected concordance
#'   \item CSMFMaxError (double): the maximum Cause Specific Mortality Fraction Error
#'   \item CSMFaccuracy (double): the Cause Specific Mortaliy Fraction accuracy
#' }
#'
#' @examples
#' library(nbc4va)
#' pred <- c("HIV", "Stroke", "HIV", "Stroke")
#' obs <- c("HIV", "HIV", "Stroke", "Stroke")
#' metrics <- nbc4va:::internalGetMetrics(pred, obs)
#'
#' @importFrom methods is
#' @family internal functions
#' @keywords internal
internalGetMetrics <- function(pred, obs, causes=unique(c(pred, obs)), csmfa.obs=NULL, causeMetrics=internalGetCauseMetrics(pred, obs, causes)) {

  # (Variables) Summary variables
  tp <- sum(causeMetrics$TruePositives)
  fn <- sum(causeMetrics$FalseNegatives)
  fp <- sum(causeMetrics$FalsePositives)
  tn <- sum(causeMetrics$TrueNegatives)
  njP <- causeMetrics$PredictedFrequency
  njO <- causeMetrics$ObservedFrequency
  nP <- length(pred)
  nO <- length(obs)
  N <- length(causeMetrics$Cause)

  # (Sens_Spec) Overall Sensitivity and Specificity
  Sensitivity <- tp / (tp + fn)

  # (PCCC) Partial Chance Corrected Concordance
  k <- 1
  C <- tp / nO
  PCCC <- (C - (k / N)) / (1 - (k / N))

  # (CSMF) Maximum CSMF Error and CSMF Accuracy
  if (is.null(csmfa.obs)) {
    csmfa.obs <- obs
  }
  CSMFMaxError <- internalGetCSMFMaxError(csmfa.obs)
  CSMFaccuracy <- internalGetCSMFAcc(pred, csmfa.obs)

  # (Return) Return the vector of metrics
  metrics <- c(tp,
               tn,
               fp,
               fn,
               Sensitivity,
               PCCC,
               CSMFMaxError,
               CSMFaccuracy)
  names(metrics) <- c("TruePositives",
                      "TrueNegatives",
                      "FalsePositives",
                      "FalseNegatives",
                      "Sensitivity",
                      "PCCC",
                      "CSMFMaxError",
                      "CSMFaccuracy")
  return(metrics)
}

