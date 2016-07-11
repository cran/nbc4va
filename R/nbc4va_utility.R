# Richard Wen (rwenite@gmail.com)
# Code for utility functions in the nbc4va package.


#' Run nbc4va using file input and output
#'
#' Runs \code{\link{nbc}} and uses \code{\link{summary.nbc}} on input data files or dataframes to output
#' result files or dataframes with data on predictions, probabilities, causes, and performance metrics in an easily accessible way.
#'
#' @details See \code{\link{nbc4vaHelpMethods}} for details on the methodology and implementation
#' of the Naive Bayes Classifier algorithm. This function may also act as a wrapper for the
#' main nbc4va package functions.
#'
#' @inheritParams internalCheckNBC
#' @param trainFile A character value of the path to the data to be used as the \emph{train} argument for \code{\link{nbc}} or a dataframe of the \emph{train} argument.
#' @param testFile A character value of the path to the data to be used as the \emph{test} argument for \code{\link{nbc}}  or a dataframe of the \emph{test} argument.
#' @param csmfaFile A character value of the path to the data to be used as the \emph{csmfa.obs} argument for \code{\link{summary.nbc}} or a named vector of the \emph{csmfa.obs} argument.
#' \itemize{
#'   \item If (csmfaFile is char): the file must have only 1 column of the causes per case
#' }
#' @param saveFiles Set to TRUE to save the return object as files or FALSE to return the actual object
#' @param outDir A character value of the path to the directory to store the output results files.
#' @param outExt A character value of the extension (without the period) to use for the result files.
#' \itemize{
#'   \item The default is set to use the "csv" extension
#' }
#' \itemize{
#'   \item The default is the directory of the \emph{testFile}
#' }
#' @param fileHeader A character value of the file header name to use for the output results files.
#' \itemize{
#'   \item The default is to use the name of the \emph{testFile}
#' }
#' @param fileReader A function that is able to read the \emph{trainFile} and the \emph{testFile}.
#' \itemize{
#'   \item The default is set to read csv files using \code{\link{read.csv}}
#' }
#' @param fileReaderIn A character value of the \emph{fileReader} argument name that accepts a file path for reading as an input.
#' @param fileReaderArgs A list of the \emph{fileReader} arguments to be called with \code{\link{do.call}}.
#' @param fileWriter A function that is able to write \code{\link{data.frame}} objects to a file location.
#' \itemize{
#'   \item The default is set to write csv files using \code{\link{write.csv}}
#' }
#' @param fileWriterIn A character value of the \emph{fileWriter} argument name that accepts a dataframe for writing.
#' @param fileWriterOut A character value of the \emph{fileWriter} argument name that accepts a file path for writing as an output.
#' @param fileWriterArgs A list of arguments of the \emph{fileWriter} arguments to be called with \code{\link{do.call}}.
#' @return out Vector or list of respective paths or data from the naive bayes classifier:
#' \itemize{
#'   \item If (\emph{saveFiles} is TRUE) return a named character vector of the following:
#'   \itemize{
#'     \item Names: dir, pred, prob, causes, summary
#'     \item dir (char): the path to the directory of the output files
#'     \item pred (char): the path to the prediction table file, where the columns of Pred1..PredN are ordered by the prediction probability with Pred1 being the most probable cause
#'     \item prob (char): the path to the probability table file, where the columns excluding the CaseID are the cause and each cell has a probability value
#'     \item causes (char): the path to the cause performance metrics table file, where each column is a metric and each row is a cause
#'     \item metrics (char): the path to the overall performance metrics table file, where each column is a metric
#'   }
#'   \item If (\emph{saveFiles} is FALSE) return a list of the following:
#'   \itemize{
#'     \item Names: pred, prob, causes, summary
#'     \item pred (dataframe): the prediction table, where the columns of Pred1..PredN are ordered by the prediction probability with Pred1 being the most probable cause
#'     \item prob (dataframe): the probability table, where the columns excluding the CaseID are the cause and each cell has a probability value
#'     \item causes (dataframe): the cause performance metrics table, where each column is a metric and each row is a cause
#'     \item metrics (dataframe): the summary table, where each column is a performance metric
#'     \item nbc (object): the returned \code{\link{nbc}} object
#'     \item nbc_summary (object): the returned \code{\link{summary.nbc}} object
#'   }
#' }
#'
#' @examples
#' library(nbc4va)
#' data(nbc4vaData)
#'
#' # Split data into train and test sets
#' train <- nbc4vaData[1:50, ]
#' test <- nbc4vaData[51:100, ]
#'
#' # Save train and test data as csv in temp location
#' trainFile <- tempfile(fileext=".csv")
#' testFile <- tempfile(fileext=".csv")
#' write.csv(train, trainFile, row.names=FALSE)
#' write.csv(test, testFile, row.names=FALSE)
#'
#' # Use nbc4vaIO via file input and output
#' # Set "known" to indicate whether test causes are known
#' outFiles <- nbc4vaIO(trainFile, testFile, known=TRUE)
#'
#' # Use nbc4vaIO as a wrapper
#' out <- nbc4vaIO(train, test, known=TRUE, saveFiles=FALSE)
#'
#' @importFrom utils read.csv write.csv
#' @family utility functions
#' @include nbc4va_main.R
#' @export
nbc4vaIO <- function(trainFile,
                     testFile,
                     known=TRUE,
                     csmfaFile=NULL,
                     saveFiles=TRUE,
                     outDir=dirname(testFile),
                     fileHeader=strsplit(basename(testFile), "\\.")[[1]][[1]],
                     fileReader=read.csv,
                     fileReaderIn="file",
                     fileReaderArgs=list(as.is=TRUE, stringsAsFactors=FALSE),
                     fileWriter=write.csv,
                     fileWriterIn="x",
                     fileWriterOut="file",
                     fileWriterArgs=list(row.names=FALSE),
                     outExt="csv") {

  # (Read_Files) Read the files into dataframes
  if (is.character(trainFile)) {
    fileReaderArgs[[fileReaderIn]] <- trainFile
    train <- do.call(fileReader, fileReaderArgs)
  }
  if (is.character(testFile)) {
    fileReaderArgs[[fileReaderIn]] <- testFile
    test <- do.call(fileReader,fileReaderArgs)
  }
  if (!is.null(csmfaFile)) {
    fileReaderArgs[[fileReaderIn]] <- csmfaFile
    csmfa.obs <- do.call(fileReader, fileReaderArgs)
    csmfa.obs <- unlist(csmfa.obs[, 1])
  } else {
    csmfa.obs <- csmfaFile
  }

  # (Run_NBC) Execute the nbc function for results
  results <- nbc(train, test, known=known)
  brief <- summary.nbc(results, top=1, csmfa.obs=csmfa.obs)
  pred <- brief$pred
  prob <- brief$prob
  causesMetrics <- brief$metrics.causes

  # (All_Metrics) Convert overall metrics to a table format
  if (!is.null(brief$metrics.all)) {
    summaryMetricsValues <- brief$metrics.all
    summaryMetrics <- data.frame(matrix(0, nrow=1, ncol=length(summaryMetricsValues)))
    summaryMetrics[1, ] <- summaryMetricsValues
    names(summaryMetrics) <- names(summaryMetricsValues)
  } else {
    summaryMetrics <- NULL
  }

  # (Return) Return the saved file paths or the data in memory
  if (saveFiles) {


    # (Write_Pred) Save the predictions into files at outDir
    predFile <- file.path(outDir, paste(fileHeader, "_pred.", outExt, sep=""))
    fileWriterArgs[[fileWriterIn]] <- pred
    fileWriterArgs[[fileWriterOut]] <- predFile
    do.call(fileWriter, fileWriterArgs)

    # (Write_Prob) Save the probabilities into files at outDir
    probFile <- file.path(outDir, paste(fileHeader, "_prob.", outExt, sep=""))
    fileWriterArgs[[fileWriterIn]] <- prob
    fileWriterArgs[[fileWriterOut]] <- probFile
    do.call(fileWriter, fileWriterArgs)

    # (Write_Causes) Save the cause metrics into files at outDir
    causesFile <- file.path(outDir, paste(fileHeader, "_causes.", outExt, sep=""))
    fileWriterArgs[[fileWriterIn]] <- causesMetrics
    fileWriterArgs[[fileWriterOut]] <- causesFile
    do.call(fileWriter, fileWriterArgs)

    # (Write_Summary) Save the summary metrics into files at ourDir
    if (!is.null(summaryMetrics)) {
      summaryFile <- file.path(outDir, paste(fileHeader, "_metrics.", outExt, sep=""))
      fileWriterArgs[[fileWriterIn]] <- summaryMetrics
      fileWriterArgs[[fileWriterOut]] <- summaryFile
      do.call(fileWriter, fileWriterArgs)
      out <- c(outDir, predFile, probFile, causesFile, summaryFile)
      names(out) <- c("dir", "pred", "prob", "causes", "metrics")
    } else {
      out <- c(outDir, predFile, probFile, causesFile)
      names(out) <- c("dir", "pred", "prob", "causes")
    }

  } else {
    out <- list(pred=pred,
                prob=prob,
                causes=causesMetrics,
                metrics=summaryMetrics,
                nbc=results,
                nbc_summary=brief)
  }
  return(out)
}


#' Web-based graphical user interface in nbc4va
#'
#' A Graphical User Interface (GUI) for the nbc4va package using \href{http://shiny.rstudio.com/}{shiny}. \cr \cr
#' \figure{nbcguiex.png}
#'
#' @details This function requires the shiny package, which can be installed via: \cr \cr
#' \code{install.packages("shiny")} \cr \cr
#' Use \emph{esc} in the R console to stop the GUI. \cr \cr
#' Please use a modern browser (e.g. latest firefox, chrome) for the best experience.
#'
#' @examples
#' \dontrun{
#' library(nbc4va)
#' nbc4vaGUI()
#' }
#'
#' @importFrom utils install.packages
#' @family utility functions
#' @export
nbc4vaGUI <- function() {
  if (!requireNamespace("shiny", quietly=TRUE)) {
    install.packages("shiny", dependencies=TRUE, repos="http://cran.rstudio.com/")
  }
  shiny::runApp(system.file('nbc4vagui', package='nbc4va'), launch.browser=TRUE)
}

