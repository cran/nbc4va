# Richard Wen
# rrwen.dev@gmail.com
# Main functions code for nbc4va package.


#' Train a NBC model
#'
#' Performs supervised Naive Bayes Classification on verbal autopsy data.
#'
#' @inheritParams internalCheckNBC
#' @return out The result nbc list object containing:
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
#' @references
#' \itemize{
#'   \item Miasnikof P, Giannakeas V, Gomes M, Aleksandrowicz L, Shestopaloff AY, Alam D, Tollman S, Samarikhalaj, Jha P. Naive Bayes classifiers for verbal autopsies: comparison to physician-based classification for 21,000 child and adult deaths. BMC Medicine. 2015;13:286. doi:10.1186/s12916-015-0521-2.
#' }
#'
#' @examples
#' library(nbc4va)
#' data(nbc4vaData)
#'
#' # Run naive bayes classifier on random train and test data
#' # Set "known" to indicate whether or not "test" causes are known
#' train <- nbc4vaData[1:50, ]
#' test <- nbc4vaData[51:100, ]
#' results <- nbc(train, test, known=TRUE)
#'
#' # Obtain the probabilities and predictions
#' prob <- results$prob.causes
#' pred <- results$pred.causes
#'
#' @family main functions
#' @include nbc4va_internal.R
#' @export
nbc <- function(train, test, known=TRUE) {
  out <- internalNBC(train, test, known=known)
  class(out) <- "nbc"
  return(out)
}


#' Summarize a NBC model with metrics
#'
#' Summarizes the results from a \code{\link{nbc}} object. The summary
#' can be either for a particular case or for the entirety of cases.
#'
#' @details See \href{https://rrwen.github.io/nbc4va/methods}{Methods documentation} for details on calculations and metrics.
#'
#' @inheritParams internalGetMetrics
#' @inheritParams internalCheckNBCSummary
#' @return out A summary object built from a \code{\link{nbc}} object with modifications/additions:
#' \itemize{
#'   \item If (\emph{id} is char):
#'     \itemize{
#'       \item Additions to a \code{\link{nbc}} object:
#'         \itemize{
#'           \item $id (char): the case \emph{id} chosen by the user
#'           \item $top (numeric): the input number of \emph{top} causes for \emph{id}
#'           \item $top.prob (vectorof double): the \emph{top} probabilities for \emph{id}
#'         }
#'       \item The following are modified from a nbc object to be \emph{id} specific:\cr $test, $test.ids, $test.causes, $obs.causes, $prob, $prob.causes, $pred, $pred.causes
#'   }
#'   \item If (\emph{id} is NULL):
#'   \itemize{
#'     \item Additions to the \code{\link{nbc}} object:
#'     \itemize{
#'       \item * indicates that the item is only available if \emph{test} causes are known
#'       \item ** indicates that the item ignores * if \emph{csmfa.obs} is given
#'       \item $top.csmf.pred (vectorof double): the \emph{top} predicted CSMFs by cause
#'       \item $top.csmf.obs* (vectorof double): the \emph{top} observed CSMFs by cause
#'       \item $metrics.all** (vectorof double): a numeric vector of overall metrics.
#'         \itemize{
#'           \item Names: TruePositives, TrueNegatives, FalsePositives, FalseNegatives, Accuracy, Sensitivity, PCCC, CSMFMaxError, CSMFaccuracy
#'           \item TruePositives* (double): total number of true positives
#'           \item TrueNegatives* (double): total number of true negatives
#'           \item FalsePositives* (double): total number of false positives
#'           \item FalseNegatives* (double): total number of false negatives
#'           \item Sensitivity* (double): the overall sensitivity
#'           \item PCCC* (double): the partial chance corrected concordance
#'           \item CSMFMaxError** (double): the maximum Cause Specific Mortality Fraction Error
#'           \item CSMFaccuracy** (double): the Cause Specific Mortaliy Fraction accuracy
#'         }
#'       \item $metrics.causes (dataframe): a perfomance table of metrics by cause.
#'         \itemize{
#'           \item Columns: Cause, Sensitivity, CSMFpredicted, CSMFobserved
#'           \item Cause (vectorof char): The unique causes from both the \emph{obs} and \emph{pred} inputs
#'           \item Sensitivity* (vectorof double): the sensitivity for a cause
#'           \item CSMFpredicted (vectorof double): the cause specific mortality fraction for a cause given the predicted deaths
#'           \item CSMFobserved* (vectorof double): the cause specific mortality fraction for a cause given the observed deaths
#'           \item TruePositives (vectorof double): The total number of true positives per cause
#'           \item TrueNegatives (vectorof double): The total number of true negatives per cause
#'           \item FalsePositives (vectorof double): The total number of false positives per cause
#'           \item FalseNegatives (vectorof double): The total number of false negatives per cause
#'           \item PredictedFrequency (vectorof double): The occurence of a cause in the \emph{pred} input
#'           \item ObservedFrequency (vectorof double): The occurence of a cause in the \emph{obs} input
#'           \item Example:
#'             \tabular{cccc}{
#'               Cause \tab Sensitivity \tab Metric-n.. \cr
#'               HIV \tab 0.5 \tab #.. \cr
#'               Stroke \tab 0.5 \tab #..
#'             }
#'         }
#'     }
#'   }
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
#' # Obtain a summary for the results
#' brief <- summary(results, top=2)  # top 2 causes by CSMF for all test data
#' briefID <- summary(results, id="v48")  # top 5 causes by probability for case "v48"
#'
#' @importFrom methods hasArg is
#' @family main functions
#' @include nbc4va_internal.R
#' @export
summary.nbc <- function(object, top=5, id=NULL, csmfa.obs=NULL, ...) {

  # (Check_Inputs) Check the inputs passed to summary.nbc
  summaryChecked <- internalCheckNBCSummary(object, top, id, csmfa.obs)
  object <- summaryChecked$object
  top <- summaryChecked$top
  id <- summaryChecked$id
  csmfa.obs <- summaryChecked$csmfa.obs

  # (Out_S3) Create summary S3 object
  out <- object
  if (top > length(object$causes.pred)) {
    warning(paste("Argument top (", top, ") has been set to the number of available unique predicted causes (", length(object$causes.pred), ") as it exceeds this value.", sep=""))
    top <- length(object$causes.pred)
  }
  out$top <- top
  out$id <- id

  # (Summary) Calculate prob or CSMFs depending if [id] is given
  if (!is.null(id)){

    # (Cause_ByProb) Obtain top causes by probability and retain relevant info to id
    out$test <- object$test[object$test == id, ]
    out$test.ids <- id
    if (object$test.known) {
      out$test.causes <- out$test[, 2]
    }
    out$obs.causes <- object$obs.causes[id]
    out$prob.causes <- object$prob.causes[id]
    out$prob <- object$prob[object$prob[, 1] == id, ]
    out$top.prob <- sort(unlist(out$prob[, 2:ncol(out$prob)]), decreasing=TRUE)[1:top]
    out$pred.causes <- object$pred.causes[id]
    out$pred <- object$pred[object$pred == id, ]
    out$obs <- object$obs[object$obs == id, ]
  } else {

    # (No_ID) Obtain overall and cause metrics if no id is given
    if (out$test.known) {
      out$metrics.causes <- internalGetCauseMetrics(object$pred.causes, object$obs.causes, object$causes)
      out$metrics.all <- internalGetMetrics(object$pred.causes, object$obs.causes, object$causes, csmfa.obs, out$metrics.causes)
    } else {
      predCSMF <- table(factor(object$pred.causes, levels=object$causes)) / length(object$pred.causes)
      out$metrics.causes <- data.frame(Cause=names(predCSMF), CSMFpredicted=as.numeric(predCSMF))
      if (!is.null(csmfa.obs)) {
        metrics <- c(internalGetCSMFMaxError(csmfa.obs), internalGetCSMFAcc(object$pred.causes, csmfa.obs))
        names(metrics) <- c("CSMFMaxError", "CSMFaccuracy")
        out$metrics.all <- metrics
      }
    }

    # (Top_Pred_CSMF) Obtain the causes by top predicted and observed CSMF
    out$top.csmf.pred <- out$metrics.causes$CSMFpredicted
    names(out$top.csmf.pred) <- out$metrics.causes$Cause
    out$top.csmf.pred <- sort(out$top.csmf.pred, decreasing=TRUE)[1:top]
    if (out$test.known) {
      out$top.csmf.obs <- out$metrics.causes$CSMFobserved
      names(out$top.csmf.obs) <- out$metrics.causes$Cause
      out$top.csmf.obs <- sort(out$top.csmf.obs, decreasing=TRUE)[1:top]
    }
  }
  class(out) <- "nbc_summary"
  return(out)
}


#' Print top predicted causes from a NBC model
#'
#' Prints a summary message from a \code{\link{summary.nbc}} object of
#' the top causes by probability or predicted Cause Specific Mortality Fraction (CSMF). \cr \cr
#' \figure{printnbcex.png}
#'
#' @details See \href{https://rrwen.github.io/nbc4va/methods}{Methods documentation} for details on CSMF and probability from the Naive Bayes Classifier.
#'
#' @param x A \code{\link{summary.nbc}} object.
#' @param ... Additional arguments to be passed if applicable.
#' @return Prints a summary of the top causes of death by probability for the NBC model.
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
#' # Print a summary of all the test data for the top 3 causes by predicted CSMF
#' brief <- summary(results, top=3)
#' print(brief)
#'
#' @family main functions
#' @export
print.nbc_summary <- function(x, ...) {
  msg <- paste("Naive Bayes Classifier (NBC) fitted on", x$train.samples, "deaths\n\n")
  if (!is.null(x$id)) {  # id given
    cat(msg, "Top", x$top, "causes by probabilities:\n")
    print(data.frame(Probability=x$top.prob))
  } else {
    cat(msg, "Top", x$top, "causes by predicted CSMF:\n")
    print(data.frame(Predicted.CSMF=x$top.csmf.pred))
  }
}


#' Bar plot of top predicted causes from a NBC model
#'
#' Plots the results from a \code{\link{nbc}} object as a \code{\link{barplot}} for a number of causes based on
#' predicted Cause Specific Mortality Fraction (CSMF). \cr \cr
#' \figure{plotnbcex.png}
#'
#' @details See \href{https://rrwen.github.io/nbc4va/methods}{Methods documentation} for details on CSMF and CSMF accuracy.
#'
#' @inheritParams internalGetMetrics
#' @param x A \code{\link{nbc}} object.
#' @param top.plot A number that produces top k causes depending on a Cause Specific Mortality Fraction (CSMF) measure.
#' @param min.csmf A number that represents the minimum CSMF measure for a cause to be included in the plot.
#' @param footnote A boolean indicating whether to include a footnote containing details about the nbc or not.
#' @param footnote.color A character specifying the color of the footnote text.
#' @param footnote.size A numeric value specifying the size of the footnote text.
#' @param main A character value of the title to display.
#' @param xlab A character value of the x axis title.
#' @param col A character value of the color to use for the plot.
#' @param horiz Set to TRUE to draw bars horizontally and FALSE to draw bars vertically.
#' @param border A character value of the colors to use for the bar borders. Set to NA to disable.
#' @param las An integer value to determine if labels should be parallel or perpendicular to axis.
#' @param ... Additional arguments to be passed to \code{\link{barplot}}.
#' @return Generates a bar plot the top predicted causes from the NBC model
#'
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
#' # Plot the top 3 causes by CSMF
#' plot(results, top.plot=3)
#'
#' @importFrom graphics barplot par strwidth title
#' @importFrom methods hasArg is
#' @family main functions
#' @seealso \code{\link{barplot}}
#' @export
plot.nbc <- function(x,
                     top.plot=length(x$causes.pred),
                     min.csmf=0,
                     csmfa.obs=NULL,
                     footnote=TRUE,
                     footnote.color="gray48",
                     footnote.size=0.7,
                     main=paste("Naive Bayes Classifier: Top ", top.plot, " Causes by Predicted CSMF", sep=""),
                     xlab="Predicted CSMF",
                     col="dimgray",
                     horiz=TRUE,
                     border=NA,
                     las=1, ...) {

  # (Data) Format the data for plotting ----

  # (Data_Plot) Obtain the plot data depending on if test cases are known
  predCSMF <- table(factor(x$pred.causes, levels=x$causes)) / length(x$pred.causes)
  metrics <- data.frame(Cause=names(predCSMF), CSMFpredicted=as.numeric(predCSMF))
  plotData <- data.frame(x=metrics$CSMFpredicted, y=metrics$Cause)

  # (Data_Filter) Filter by top.plot and min.csmf
  plotData <- plotData[plotData$x > min.csmf, ]  # filter to min.csmf
  plotData <- plotData[order(-plotData$x), ]  # sort
  if (length(plotData$y) < 1) {
    stop(paste("There are no available causes with CSMFs above the min.csmf (", min.csmf, ").", sep=""))
  }
  if (top.plot > length(plotData$y)) {
    warning(paste("Only the top ", length(plotData$y), " causes were plotted as the argument top.plot (", top.plot, ") exceeds the number of available unique causes after applying min.csmf (", min.csmf, ").", sep=""))
    top.plot <- length(plotData$y)
  }
  plotData <- plotData[1:top.plot, ]  # take top csmfs
  plotData$x <- rev(plotData$x)
  plotData$y <- rev(plotData$y)

  # (Plot) Create the plot ----

  # (Adjust_Plot) Adjust cause label margins for length cause titles
  adj <- max(strwidth(as.character(plotData$y), "inch") + 0.4, na.rm = TRUE)
  oldpar <- par(no.readonly=TRUE)
  on.exit(par(oldpar))
  corners <- par("usr")  # xleft, xright, ybottom, ytop

  # (Footnotes_Plot) Include footnotes
  note <- paste("Trained: ", x$train.samples,
                " | Tested: ", x$test.samples,
                " | Symptoms: ", length(x$symptoms),
                " | Causes: ", length(x$causes),
                sep="")
  if (!is.null(csmfa.obs)) {
    csmfa <- internalGetMetrics(x$pred.causes, x$pred.causes, csmfa.obs=csmfa.obs)[["CSMFaccuracy"]]
    note <- paste(note, " | CSMF Acc.: ", round(csmfa, 3), sep="")
  } else if (!is.null(x$obs.causes)) {
    csmfa <- internalGetMetrics(x$pred.causes, x$obs.causes, csmfa.obs=csmfa.obs)[["CSMFaccuracy"]]
    note <- paste(note, " | CSMF Acc.: ", round(csmfa, 3), sep="")
  }

  # (Plot_Create) Plot the causes by predicted CSMF
  xAxisMax <- c(0, max(round(plotData$x + 0.1, 1)))
  xAxisMax <- ifelse(xAxisMax > 1, 1, xAxisMax)
  omai <- par("mai") # original y-axis label margins
  par(mai=c(1.3, adj, 0.82, 0.42))
  barplot(plotData$x,
          xlim=xAxisMax,
          xlab=xlab,
          names.arg=plotData$y,
          main=main,
          col=col,
          horiz=horiz,
          border=border,
          las=las, ...)
  if (footnote) {
    title(sub=note, line=4.8, cex.sub=footnote.size, col.sub=footnote.color)
  }
  par(mai=omai)  # reset to defaults
}

