# Richard Wen (rwenite@gmail.com)
# Code for help functions in the nbc4va package.


#' Help functions in nbc4va
#'
#' @section About:
#' This documentation page provides a list of the available help pages in the nbc4va package.
#'
#' @section Available Help Documentation:
#' The nbc4va package has several help functions built in to aid the understanding of the methodology
#' of the package and to guide the use of functions and methods. It is possible to click the links to
#' the functions below to view the help pages or call them which produces the same result.
#' \itemize{
#'   \item \code{\link{nbc4vaHelp}}: opens this help page
#'   \item \code{\link{nbc4vaHelpMethods}}: opens the help page for methodology and implementation
#'   \item \code{\link{nbc4vaHelpData}}: opens the help page for formatting input training/testing data
#'   \item \code{\link{nbc4vaHelpBasic}}: opens the help page for using the package in graphical or file interface
#'   \item \code{\link{nbc4vaHelpAdvanced}}: opens the help page for using the package in R
#'   \item \code{\link{nbc4vaHelpFunctions}}: opens the help page for an organized list of useful functions in the package
#'   \item \code{\link{nbc4vaHelpDev}}: opens the developer help page for function and package dependencies
#' }
#' \subsection{Sample Code for Opening Help Documentation}{
#'   Run the following code using \code{\link{nbc4vaHelp}} in the R console to conveniently open up a variety of help pages.
#'   \preformatted{
#'--------------------------------------------------------------------------------------------------
#'
#'     library(nbc4va)  # load the nbc4va package
#'     nbc4vaHelp()  # view this help page
#'     nbc4vaHelpMethods()  # view methods and implementation help page
#'     nbc4vaHelpData()  # view input train/test data formatting  help page
#'     nbc4vaHelpBasic()  # view graphical or file interface help page
#'     nbc4vaHelpAdvanced()  # view usage in R help page
#'     nbc4vaHelpFunctions()  # view help page of useful functions
#'     nbc4vaHelpDev()  # view developer help page
#'
#'--------------------------------------------------------------------------------------------------
#'   }
#' }
#'
#' @importFrom utils help
#' @seealso Guide for package: \code{\link{nbc4va}}
#' @family help functions
#' @export
nbc4vaHelp <- function(){help(nbc4vaHelp)}


#' Methodology and implementation in nbc4va
#'
#' @section About:
#' This documentation page provides details on the implementation of the Naive Bayes Classifier algorithm,
#' definition of uncommon terms, and calculation of performance metrics.
#'
#' @section Naive Bayes Classifier:
#' The Naive Bayes Classifier (NBC) is a machine learning algorithm that uses training data containing
#' cases of deaths to learn probabilities for known causes of death based on given symptoms.
#' This produces a model that can use the learned probabilities to predict
#' the cause of death for cases in unseen testing data with same symptoms. \cr \cr
#' The nbc4va package implements the NBC algorithm for verbal autopsy data using code and methods built on \href{http://bmcmedicine.biomedcentral.com/articles/10.1186/s12916-015-0521-2}{Miasnikof \emph{et al} (2015)}.
#'
#' @section Terms for Data:
#' \itemize{
#'   \item \strong{Symptom}: Refers to the features or independent variables with binary values of 1 for presence and 0 for absence of a death related condition
#'   \item \strong{Cause}: Refers to the target or dependent variable containing discrete values of the causes of death
#'   \item \strong{Case}: Refers to an individual death containing an identifier, a cause of death (if known), and several symptoms
#'   \item \strong{Training Data}: Refers to a dataset of cases that the NBC algorithm learns probabilities from
#'   \item \strong{Testing Data}: Refers to a dataset of cases used to evaluate the performance of a NBC model; these cases must have the same symptoms as the \emph{Training Data}, but with different cases
#' }
#'
#' @section Terms for Metrics:
#' \itemize{
#'   \item \strong{True Positives}: The number of cases, given a cause, where the predicted cause is equal to the actual observed cause \href{http://people.inf.elte.hu/kiss/13dwhdm/roc.pdf}{(Fawcett, 2005)}.
#'   \item \strong{True Negatives}: The number of cases, given a cause, where the predicted is not the cause and the actual observed is also not the cause \href{http://people.inf.elte.hu/kiss/13dwhdm/roc.pdf}{(Fawcett, 2005)}.
#'   \item \strong{False Positives}: The number of cases, given a cause, where the predicted is the cause and the actual observed is not the cause \href{http://people.inf.elte.hu/kiss/13dwhdm/roc.pdf}{(Fawcett, 2005)}.
#'   \item \strong{False Negatives}: The number of cases, given a cause, where the predicted is not the cause and the actual observed is the cause \href{http://people.inf.elte.hu/kiss/13dwhdm/roc.pdf}{(Fawcett, 2005)}.
#'   \item \strong{CSMF}: The fraction of deaths (predicted or observed) for a particular cause.
#' }
#'
#' @section Calculation of Metrics at the Individual Level:
#' The following metrics measure the performance of a model by comparing its predicted causes individually
#' to the matching true/observed causes.
#' \itemize{
#'   \item \strong{Sensitivity}: proportion of correctly identified positives \href{https://dl.dropboxusercontent.com/u/27743223/201101-Evaluation_JMLT_Postprint-Colour.pdf}{(Powers, 2011)}.
#'     \cr \cr \figure{eqsensitivity.png}
#'     \itemize{
#'       \item \eqn{TP} is the number of true positives
#'       \item \eqn{FN} is the number of false negatives
#'       \item This metric measures a model's ability to correctly predict causes of death
#'     }
#'   \item \strong{PCCC}: partial chance corrected concordance \href{http://pophealthmetrics.biomedcentral.com/articles/10.1186/1478-7954-9-28}{(Murray \emph{et al} 2011)}.
#'     \cr \cr \figure{eqpccc.png}
#'     \itemize{
#'       \item \eqn{C} is the fraction of deaths where the true cause is in the top \eqn{k} causes assigned to that death
#'       \item \eqn{k} is the number of top causes (constant of 1 in this package)
#'       \item \eqn{N} is the number of causes in the study
#'       \item This metric measures how much better a model is than random assignment
#'     }
#' }
#'
#' @section Calculation of Metrics at the Population Level:
#' The following metrics measure the performance of a model by comparing its distribution of cause predictions to
#' a distribution of true/observed causes for similar cases.
#' \itemize{
#'   \item \strong{CSMFmaxError}: cause specific mortality fraction maximum error \href{http://pophealthmetrics.biomedcentral.com/articles/10.1186/1478-7954-9-28}{(Murray \emph{et al} 2011)}.
#'     \cr \cr \figure{eqcsmfmaxerror.png}
#'     \itemize{
#'       \item \eqn{j} is a true/observed cause
#'       \item \eqn{CSMFtruej} is the true/observed CSMF for cause j
#'     }
#'   \item \strong{CSMFaccuracy}: cause specific mortality fraction accuracy \href{http://pophealthmetrics.biomedcentral.com/articles/10.1186/1478-7954-9-28}{(Murray \emph{et al} 2011)}.
#'     \cr \cr \figure{eqcsmfaccuracy.png}
#'     \itemize{
#'       \item \eqn{j} is a cause
#'       \item \eqn{CSMFtruej} is the true/observed CSMF for cause j
#'       \item \eqn{CSMFpredj} is the predicted CSMF for cause j
#'       \item Values range from 0 to 1 with 1 meaning no error in the predicted CSMFs, and 0 being complete error in the predicted CSMFs
#'     }
#' }
#'
#' @references
#' \itemize{
#'   \item Fawcett T. An introduction to ROC analysis. Pattern Recognition Letters[Internet]. 2005 Dec 19[cited 2016 Apr 29];27(8):861-874. Available from: http://people.inf.elte.hu/kiss/13dwhdm/roc.pdf
#'   \item Miasnikof P, Giannakeas V, Gomes M, Aleksandrowicz L, Shestopaloff AY, Alam D, Tollman S, Samarikhalaj, Jha P. Naive Bayes classifiers for verbal autopsies: comparison to physician-based classification for 21,000 child and adult deaths. BMC Medicine. 2015;13:286. doi:10.1186/s12916-015-0521-2.
#'   \item Murray CJL, Lozano R, Flaxman AD, Vahdatpour A, Lopez AD. Robust metrics for assessing the performance of different verbal autopsy cause assignment methods in validation studies.Popul Health Metr. 2011;9:28. doi:10.1186/1478-7954-9-28.
#'   \item Powers DMW. EVALUATION: FROM PRECISION, RECALL AND F-MEASURE TO ROC, INFORMEDNESS, MARKEDNESS & CORRELATION. Journal of Machine Learning Technologies. 2011;2(1)37-63.
#' }
#' @importFrom utils help
#' @seealso Guide for package: \code{\link{nbc4va}}
#' @family help functions
#' @export
# Sensitivity = \frac{TP}{TP+FN}
# PCCC(k) = \frac{C-\frac{k}{N}}{1-\frac{k}{N}}
# CSMF Maximum Error = 2(1-Min(CSMF_{j}^{true})
# CSMFAccuracy = 1-\frac{\sum_{j=1}^{k} |CSMF_{j}^{true} - CSMF_{j}^{pred}|}{CSMF Maximum Error}
nbc4vaHelpMethods <- function(){help(nbc4vaHelpMethods)}


#' Training and testing specifications in nbc4va
#'
#' @section About:
#' This documentation page provides details on the training and testing data formats to be used as inputs in the nbc4va package.
#'
#' @section Training and Testing Data:
#' The training data (consisting of cases, causes of death for each case, and symptoms) is used as input for the Naive Bayes Classifier (NBC) algorithm to learn the probabilities for
#' each cause of death to produce a NBC model. \cr \cr
#' This model can be evaluated for its performance by predicting on the testing data cases, where the predicted causes of death
#' are compared to the causes of death in the testing data. \cr \cr
#' The process of learning the probabilities to produce the NBC model is known as training, and the process of evaluating the predictive performance of the trained model is known as testing. \cr \cr
#' \strong{Key points}:
#' \itemize{
#'   \item The training data is used to build the NBC model
#'   \item The testing data is used to evaluate the NBC model's predictive performance
#'   \item Ideally, the testing data should not have the same cases in the training data
#'   \item Both the training and testing data must have the same symptoms
#' }
#'
#' @section Format:
#' The format of the training and testing data is structured as a table, where each column holds a variable and each row
#' holds a death case. \cr \cr
#' The following format must be met in order to be used with the nbc4va package:
#' \itemize{
#'   \item \strong{Columns (in order)}: ID, Cause, Symptoms1..N
#'   \item \strong{ID}: column of case identifiers formatted as text
#'   \item \strong{Cause}: column of known causes of death formatted as text
#'   \item \strong{Symptoms1..N}: N number of columns representing symptoms with each column containing 1 for presence of the symptom, 0 for absence of the symptom, any other values are treated as unknown
#'   \item If the testing causes are not known, the second column (\emph{Cause}) can be omitted
#'   \item Unknown symptoms are imputed randomly from the distribution of known 1s and 0s; a symptom column will be removed from training and testing if 1s or 0s do not exist
#'   \item Both the training and testing data must be consistent with each other (same symptoms in order) to be meaningful
#' }
#'
#' @section Examples:
#' The image below shows an example of the training data. \cr \cr
#' \figure{nbcdatatrainex.png} \cr
#'
#' The image below shows an example of the corresponding testing data. \cr \cr
#' \figure{nbcdatatestex.png}{options: width=400px;} \cr
#'
#' The image below shows an example of the corresponding testing data without any causes. \cr \cr
#' \figure{nbcdatatestncex.png}
#'
#' @section Symptom Imputation Example:
#' Given a symptom column containing the values of each case (1, 0, 0, 1, 99, 99):
#' \itemize{
#'   \item 1 represents presence of the symptom
#'   \item 0 represents absence of the symptom
#'   \item 99 is treated as unknown as to whether the symptom is present or absent
#' }
#' The imputation is applied as follows:
#' \enumerate{
#'   \item The unknown values (99, 99) are randomly imputed according to the known values (1, 0, 0, 1).
#'   \item The known values contain half (2/4) the values as 1s and half (2/4) the values as 0s.
#'   \item Thus, the imputation results in half (1/2) the unknown values as 1s and half (1/2) of the unknown values as 0s to match the known values distribution.
#'   \item The possible combinations for replacing the unknown values (99, 99) are then (1, 0) and (0, 1).
#' }
#' The symptom imputation method preserves the approximate distribution of the known values in an attempt to avoid dropping entire cases or symptoms.
#'
#' @section Sample Code:
#' Run the following code using \code{\link{nbc4vaData}} in the R console to view the example data included in the nbc4va package.
#' \preformatted{
#'--------------------------------------------------------------------------------------------------
#'
#'     library(nbc4va)  # load the nbc4va package
#'     data(nbc4vaData)  # load the example data
#'     View(nbc4vaData)  # view the sample data in the nbc4va package
#'     data(nbc4vaDataRaw)  # load the example data with unknown symptom values
#'     View(nbc4vaDataRaw)  # view the sample data with unknown symptom values
#'
#'--------------------------------------------------------------------------------------------------
#' }
#'
#' @importFrom utils help
#' @seealso Guide for package: \code{\link{nbc4va}}
#' @family help functions
#' @export
nbc4vaHelpData <- function(){help(nbc4vaHelpData)}


#' Basic usage in nbc4va
#'
#' @section About:
#' This documentation page provides details on the basic usage of the nbc4va package which includes bringing up
#' the Graphic User Interface and running the Naive Bayes Classifier algorithm using file input and output.
#'
#' @section User Interface:
#' The simplest way to use the package is to open the Graphical User Interface (GUI) in your default web browser with \code{\link{nbc4vaGUI}()}. \cr \cr
#' Once the GUI is loaded, follow the instructions to fit a NBC model to your \file{training.csv} and to evaluate its performance with your \file{testing.csv} data.
#' \subsection{Example of GUI}{
#'   If \code{\link{nbc4vaGUI}} is called sucessfully, the GUI shown in the image below should be available in your web browser. \cr \cr
#'   \figure{nbcguiex.png} \cr \cr
#' }
#' \subsection{Sample Code for GUI}{
#'   Run the following code using \code{\link{nbc4vaGUI}} in the R console to open the GUI in your web browser.\cr\cr
#'   Close the GUI by pressing escape while you are in the R console.
#'   \preformatted{
#'--------------------------------------------------------------------------------------------------
#'
#'     library(nbc4va)  # load the package
#'     nbc4vaGUI()  # open the GUI in your web browser
#'
#'--------------------------------------------------------------------------------------------------
#'   }
#' }
#' \subsection{References for GUI}{
#'   \itemize{
#'     \item See the \code{\link{nbc4vaHelpMethods}} section for definitions of performance metrics and terms in the model results
#'   }
#' }
#'
#' @section File Input and Output:
#' The \code{\link{nbc4vaIO}} function can be called to fit a NBC model and save its results
#' using the paths to your \file{training} and \file{testing} files in Comma Separated Values (CSV) format. \cr \cr
#' The saved results will in a selected directory with four CSV files detailing the performance of the model:
#' \itemize{
#'   \item \file{.._pred.csv}: a table of predictions, where the columns Prediction1..PredictionN are the cause of death predictions with Prediction1 being the most probable cause
#'   \item \file{.._prob.csv}: a table of probabilities, where each column is a cause of death and each cell is the probability of a case being that cause
#'   \item \file{.._causes.csv}: a table of metrics for each cause
#'   \item \file{.._metrics.csv}: a table of summary metrics for the model
#'   \item The \emph{..} represents the name of your \file{testing} file
#' }
#' \subsection{Example of File Input and Output}{
#'  The image below shows the input files on the left and the saved results on the right using the \code{\link{nbc4vaIO}} function (with the \emph{fileHeader} argument set to "nbc4va"). \cr \cr
#'  \figure{nbcioex.png}
#' }
#' \subsection{Sample Code for File Input and Output}{
#'   Run the following code using \code{\link{nbc4vaIO}} in the R console to produce NBC model performance results with the \file{training} and \file{testing} files.
#'   \preformatted{
#'--------------------------------------------------------------------------------------------------
#'
#'     library(nbc4va)
#'
#'     # Find paths to your "trainFile" and "testFile"
#'     trainFile <- file.choose()  # select train file first
#'     testFile <- file.choose()  # followed by test file after
#'
#'     # Run NBC model and dump results to the same directory as the "testFile"
#'     nbc4vaIO(trainFile, testFile, known=TRUE)  # set known to indicate whether testing causes are known
#'
#'--------------------------------------------------------------------------------------------------
#'   }
#' }
#' \subsection{References for File Input and Output}{
#'   \itemize{
#'     \item See \code{\link{nbc4vaIO}} for complete function specifications and usage
#'     \item See the \code{\link{nbc4vaHelpData}} section for \file{training} and \file{testing} file formats
#'     \item See the \code{\link{nbc4vaHelpMethods}} section for definitions of performance metrics and terms in the model results
#'   }
#' }
#'
#' @importFrom utils help
#' @seealso Guide for package: \code{\link{nbc4va}}
#' @family help functions
#' @export
nbc4vaHelpBasic <- function(){help(nbc4vaHelpBasic)}


#' Advanced usage in nbc4va
#'
#' @section About:
#' This documentation page provides details on the advanced usage of the nbc4va package which includes
#' training a NBC model, evaluating NBC model performance, and plotting the top predicted causes from
#' the NBC model. The documentation written here is intended for users of R that understand the different
#' data structures of R such as: \code{\link{data.frame}}, \code{\link{list}}, and \code{\link{vector}}. It
#' is also required to understand the basic data types: \code{\link{character}}, and \code{\link{numeric}}.
#'
#' @section Training a NBC Model:
#' Run the following code using \code{\link{nbc}} in the R console to train a NBC model.
#' \preformatted{
#'--------------------------------------------------------------------------------------------------
#'
#'     library(nbc4va)
#'
#'     # Create training and testing dataframes
#'     data(nbc4vaData)  # example data
#'     train <- nbc4vaData[1:50, ]
#'     test <- nbc4vaData[51:100, ]
#'
#'     # Train a nbc model
#'     # The "results" variable is a nbc list-like object with elements accessible by $
#'     # Set "known" to indicate whether or not testing causes are known in "test"
#'     results <- nbc(train, test, known=TRUE)
#'
#'     # Obtain the probabilities and predictions
#'     prob <- results$prob.causes  # vector of probabilities for each test case
#'     pred <- results$pred.causes  # vector of top predictions for each test case
#'
#'     # View the "prob" and "pred", the names are the case ids
#'     head(prob)
#'     head(pred)
#'
#'--------------------------------------------------------------------------------------------------
#' }
#' \subsection{References for Training a NBC Model}{
#'   \itemize{
#'     \item See \code{\link{nbc}} for complete function specifications and usage
#'     \item See the \code{\link{nbc4vaHelpMethods}} section for the NBC algorithm details
#'   }
#' }
#'
#' @section Evaluating a NBC Model:
#' Run the following code using \code{\link{summary.nbc}} in the R console to evaluate a NBC model.
#' \preformatted{
#'--------------------------------------------------------------------------------------------------
#'
#'     library(nbc4va)
#'
#'     # Create training and testing dataframes
#'     data(nbc4vaData)
#'     train <- nbc4vaData[1:50, ]
#'     test <- nbc4vaData[51:100, ]
#'
#'     # Train a nbc model
#'     results <- nbc(train, test, known=TRUE)
#'
#'     # Automatically calculate metrics with summary
#'     # The "brief" variable is a nbc_summary list-like object
#'     # The "brief" variable is "results", but with additional metrics
#'     brief <- summary(results)
#'
#'     # Obtain the calculated metrics
#'     metrics <- brief$metrics.all  # vector of overall metrics
#'     causeMetrics <- brief$metrics.causes  # dataframe of metrics by cause
#'
#'     # Access the calculatd metrics
#'     metrics[["CSMFaccuracy"]]
#'     metrics[["Sensitivity"]]
#'     View(causeMetrics)
#'
#'--------------------------------------------------------------------------------------------------
#' }
#' \subsection{References for Evaluating a NBC Model}{
#'   \itemize{
#'     \item See \code{\link{summary.nbc}} for complete method specifications and usage
#'     \item See the \code{\link{nbc4vaHelpMethods}} section for definitions of performance metrics and terms in the output
#'   }
#' }
#'
#' @section Plotting the Top Predicted Causes:
#' Run the following code using \code{\link{plot.nbc}} in the R console to produce a bar plot of the top predicted causes.
#' \preformatted{
#'--------------------------------------------------------------------------------------------------
#'
#'     library(nbc4va)
#'
#'     # Create training and testing data
#'     data(nbc4vaData)
#'     train <- nbc4vaData[1:50, ]
#'     test <- nbc4vaData[51:100, ]
#'
#'     # Train a nbc model and plot the top 5 causes if possible
#'     results <- nbc(train, test, known=TRUE)
#'     plot(results, top=5)
#'     plot(results, top=5, footnote=FALSE)  # remove footnote
#'
#'--------------------------------------------------------------------------------------------------
#' }
#' \subsection{Example of Plotting the Top Predicted Causes}{
#'   The image below shows a plot of the top causes of death by predicted CSMFs using \code{\link{plot.nbc}} on a NBC model trained using the \code{\link{nbc4vaData}} included in the package. \cr \cr
#'   \figure{plotnbcex.png}
#' }
#' \subsection{References for Plotting the Top Predicted Causes}{
#'   \itemize{
#'     \item See \code{\link{plot.nbc}} for complete method specifications and usage
#'     \item See the \code{\link{nbc4vaHelpMethods}} section for definition of CSMF and related metrics in the footnote of the plot
#'   }
#' }
#'
#' @importFrom utils help
#' @seealso Guide for package: \code{\link{nbc4va}}
#' @family help functions
#' @export
nbc4vaHelpAdvanced <- function(){help(nbc4vaHelpAdvanced)}


#' Functions in nbc4va
#'
#' @section About:
#' This documentation page provides an organized list of the available functions in the nbc4va package and a brief
#' description of what they are used for.
#'
#' @section Utility Functions:
#' \itemize{
#'   \item \strong{\code{\link{nbc4vaGUI}}}: web-based graphical user interface using \href{http://shiny.rstudio.com/}{shiny}
#'   \item \strong{\code{\link{nbc4vaIO}}}: convenient file input and output of model results
#' }
#'
#' @section Main Functions:
#' \itemize{
#'   \item \strong{\code{\link{nbc}}}: fit a NBC model using training data to make predictions on testing data
#'   \item \strong{\code{\link{summary.nbc}}}: summarize \code{\link{nbc}} model results with informative metrics
#'   \item \strong{\code{\link{plot.nbc}}}: plot the top predicted causes of death by cause specific mortality fractions from a \code{\link{nbc}} model
#' }
#'
#' @section Wrapper Functions:
#' \itemize{
#'   \item \strong{\code{\link{topCOD.nbc}}}: get the top cause of death predictions from \code{\link{nbc}}
#'   \item \strong{\code{\link{csmf.nbc}}}: get the predicted cause specific mortality fractions from \code{\link{nbc}}
#' }
#'
#' @section Extra Functions:
#' \itemize{
#'   \item \strong{\code{\link{ova2nbc}}}: wrapper function for accepting \href{https://cran.r-project.org/package=openVA}{openVA} inputs to create a \code{\link{nbc}} model
#' }
#'
#' @section Help Functions:
#' \itemize{
#'   \item \strong{\code{\link{nbc4vaHelpMethods}}}: help page on methodology and implementation of the package
#'   \item \strong{\code{\link{nbc4vaHelpData}}}: help page on training and testing data format in the package
#'   \item \strong{\code{\link{nbc4vaHelpBasic}}}: help page on running the package user interface and file input/output
#'   \item \strong{\code{\link{nbc4vaHelpAdvanced}}}: help page on running the main functions and methods in the package
#'   \item \strong{\code{\link{nbc4vaHelpDev}}}: developer help page on the coding structure and dependencies
#' }
#'
#' @importFrom utils help
#' @seealso Guide for package: \code{\link{nbc4va}}
#' @family help functions
#' @export
nbc4vaHelpFunctions <- function(){help(nbc4vaHelpFunctions)}


#' Developer notes in nbc4va
#'
#' @section About:
#' This documentation page provides information for developers on the structure of the code, as well as any package and function dependencies.
#'
#' @section Internal Functions:
#' \itemize{
#'   \item \strong{\code{\link{internalNBC}}}: execute and return the details of the naive bayes classifier algorithm
#'   \item \strong{\code{\link{internalCheckNBC}}}: checks the \code{\link{nbc}} function arguments
#'   \item \strong{\code{\link{internalCheckNBCSummary}}}: checks the \code{\link{summary.nbc}} function arguments
#'   \item \strong{\code{\link{internalGetCSMFMaxError}}}: calculates the CSMF maximum error given the observed cases
#'   \item \strong{\code{\link{internalGetCSMFAcc}}}: calculates the CSMF accuracy given the predicted and observed cases of any length
#'     \itemize{
#'       \item \strong{Dependencies}: \code{\link{internalGetCSMFMaxError}}
#'     }
#'   \item \strong{\code{\link{internalGetCauseMetrics}}}: creates a dataframe of particular metrics for each cause
#'   \item \strong{\code{\link{internalGetMetrics}}}: creates a vector of overall metrics given the observed and predicted cases
#'     \itemize{
#'       \item \strong{Dependencies}: \code{\link{internalGetCSMFMaxError}}, \code{\link{internalGetCSMFAcc}}
#'     }
#'   \item \strong{\code{\link{internalSubAsRest}}}: substitute a target value for the rest of the values by their distribution per column
#'     \itemize{
#'       \item \strong{Dependencies}: \code{\link{internalRoundFixedSum}}
#'     }
#'   \item \strong{\code{\link{internalRoundFixedSum}}}: round a vector of values to whole numbers while preserving their sum
#' }
#' @section External Functions:
#' \itemize{
#'   \item \strong{\code{\link{nbc}}}: main function for running the naive bayes classifier
#'     \itemize{
#'       \item \strong{Dependencies}: \code{\link{internalCheckNBC}}, \code{\link{internalNBC}}
#'     }
#'   \item \strong{\code{\link{summary.nbc}}}: consumes output from \code{\link{nbc}} to calculate informative metrics
#'     \itemize{
#'       \item \strong{Dependencies}: \code{\link{internalCheckNBCSummary}}, \code{\link{internalGetCauseMetrics}}, \code{\link{internalGetMetrics}}, \code{\link{internalGetCSMFMaxError}}, \code{\link{internalGetCSMFAcc}}
#'     }
#'   \item \strong{\code{\link{print.nbc_summary}}}: consumes output from \code{\link{summary.nbc}} to print a message of the top causes
#'     \itemize{
#'       \item \strong{Dependencies}: \code{\link{summary.nbc}}, \code{\link{nbc}}
#'     }
#'   \item \strong{\code{\link{plot.nbc}}}: consumes output from \code{\link{nbc}} to plot the top causes
#'     \itemize{
#'       \item \strong{Dependencies}: \code{\link{summary.nbc}}, \code{\link{nbc}}
#'     }
#'   \item \strong{\code{\link{nbc4vaIO}}}: file input and output for the main package functions
#'     \itemize{
#'       \item \strong{Dependencies}: \code{\link{summary.nbc}}, \code{\link{nbc}}
#'     }
#'   \item \strong{\code{\link{nbc4vaGUI}}}: graphical user interface to run the main package functions
#'     \itemize{
#'       \item \strong{Dependencies}: \code{\link{nbc4vaIO}}
#'       \item \strong{Required Packages}: \pkg{shiny}
#'     }
#'   \item \strong{\code{\link{csmf.nbc}}}: consumes output from \code{\link{nbc}} to get the predicted csmfs
#'     \itemize{
#'       \item \strong{Dependencies}: \code{\link{nbc}}, \code{\link{summary.nbc}}
#'     }
#'   \item \strong{\code{\link{topCOD.nbc}}}: consumes output from \code{\link{nbc}} to get the top predictions
#'     \itemize{
#'       \item \strong{Dependencies}: \code{\link{nbc}}
#'     }
#' }
#' @section Extra Functions:
#' \itemize{
#'   \item \strong{\code{\link{ova2nbc}}}: consumes arguments specified in the \href{https://cran.r-project.org/package=openVA}{openVA} package to get a modified \code{\link{nbc}} object
#'     \itemize{
#'       \item \strong{Suggested Packages}: \href{https://cran.r-project.org/package=openVA}{openVA}
#'     }
#' }
#'
#' @importFrom utils help
#' @seealso Guide for package: \code{\link{nbc4va}}
#' @family help functions
#' @export
nbc4vaHelpDev <- function(){help(nbc4vaHelpDev)}

