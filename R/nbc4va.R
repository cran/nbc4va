# Richard Wen (rwenite@gmail.com)
# nbc4va guide documentation.
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("To view the 'nbc4va' package guide see: help(nbc4va) \nTo cite the original method used in the 'nbc4va' package see: citation(\"nbc4va\")")
}

#' Guide for the nbc4va package
#'
#' @description
#' The nbc4va package implements the Naive Bayes Classifier (NBC) algorithm for verbal autopsy data based on code and methods
#' provided by \href{http://bmcmedicine.biomedcentral.com/articles/10.1186/s12916-015-0521-2}{Miasnikof \emph{et al} (2015)}. \cr \cr
#' This package is intended to be used for experimenting with the NBC algorithm to predict causes of death using
#' verbal autopsy data.
#'
#' @section Acknowledgements:
#' The original NBC algorithm code was developed by Peter Miaskinof and Vasily Giannakeas.
#' The original performance metrics code was provided by Dr. Mireille Gomes whom also offered guidance in metrics implementation and user testing.
#' Special thanks to Richard Zehang Li for providing a standard structure for the package and Patrycja Kolpak for user testing of the GUI.
#'
#' @section Summary:
#' \itemize{
#'   \item \link{nbc4vaHelpMethods}: methodology and terms
#'   \item \link{nbc4vaHelpData}: input training/testing data format
#'   \item \link{nbc4vaHelpBasic}: basic usage with user interface and file input/output
#'   \item \link{nbc4vaHelpAdvanced}: advanced usage in R
#'   \item \link{nbc4vaHelpFunctions}: organized list of functions
#'   \item \link{nbc4vaHelp}: used to view help pages by section
#'   \item \link{help}: used to view help pages for any R function, object, or package
#' }
#'
#' @section Guide:
#' \subsection{Background}{
#'   Before using the nbc4va package, ensure that the training and testing data inputs are formatted correctly and that
#'   the terms used in this package are understood:
#'   \itemize{
#'     \item It is strongly suggested to review the \link{nbc4vaHelpMethods} section for an understanding of terms and metrics
#'     \item The input data has specifications detailed in the \link{nbc4vaHelpData} section
#'   }
#' }
#' \subsection{Package Usage}{
#'   The nbc4va package contains help sections with code samples and references for usage:
#'   \itemize{
#'     \item The \link{nbc4vaHelpBasic} section is suited for users of all levels
#'     \item The \link{nbc4vaHelpAdvanced} section is suited for users of the package that have some R programming experience
#'     \item The \link{nbc4vaHelpFunctions} section provides an organized list of package functions with brief descriptions
#'   }
#' }
#' @section Getting Help:
#' \subsection{Using nbc4va Help Sections}{
#'   The nbc4va package includes convenient functions to open help sections via the R console using \code{\link{nbc4vaHelp}}.
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
#' \subsection{Using R's Help Function}{
#'   In the R console, the command \code{\link{help}} (short form using \emph{?}) can be used to access help pages for R objects, functions, and packages. \cr \cr
#'   \preformatted{
#'--------------------------------------------------------------------------------------------------
#'
#'     ?nbc4va  # view this guide, same as help(nbc4va)
#'     ?nbc4vaGUI  # view help page for the user interface, same as help(nbc4vaGUI)
#'     ?nbc  # view help page for the nbc function
#'     ?summary.nbc  # view help page for the summary.nbc method
#'     ?plot.nbc # view help page for the plot.nbc method
#'     ?nbc4vaData  # view help page for example data
#'     ?nbc4vaHelp  # view the available nbc4va help sections
#'     ?topCOD.nbc  # view help page for wrapper function topCOD.nbc
#'
#'--------------------------------------------------------------------------------------------------
#'   }
#' }
#'
#' @references
#' Use \code{citation("nbc4va")} to view citation information for the nbc4va package.
#' \itemize{
#'   \item Miasnikof P, Giannakeas V, Gomes M, Aleksandrowicz L, Shestopaloff AY, Alam D, Tollman S, Samarikhalaj, Jha P. Naive Bayes classifiers for verbal autopsies: comparison to physician-based classification for 21,000 child and adult deaths. BMC Medicine. 2015;13:286. doi:10.1186/s12916-015-0521-2.
#' }
#'
#' @seealso Developer Help Page: \code{\link{nbc4vaHelpDev}}
#' @author Richard Wen <\email{rwenite@@gmail.com}>
#' @docType package
#' @name nbc4va
NULL
