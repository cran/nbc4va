# Richard Wen
# rrwen.dev@gmail.com
# nbc4va guide documentation.
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("To view the 'nbc4va' package description see: \nhelp(nbc4va) \n\nTo view the 'nbc4va' package documentation, see: \nhttps://rrwen.github.io/nbc4va \n\nTo cite the original method used in the 'nbc4va' package see: \ncitation(\"nbc4va\")")
}

#' nbc4va: Bayes Classifier for Verbal Autopsy Data
#'
#' An implementation of the Naive Bayes Classifier (NBC) algorithm
#' used for Verbal Autopsy (VA) built on code from \href{http://bmcmedicine.biomedcentral.com/articles/10.1186/s12916-015-0521-2}{Miasnikof et al (2015)} <DOI:10.1186/s12916-015-0521-2>.
#' \cr\cr
#' For documentation and help, please see:
#' \cr\cr
#' \url{https://rrwen.github.io/nbc4va}
#'
#' @section Acknowledgements:
#' This package was developed at the Centre for Global Health Research (CGHR) in Toronto, Ontario, Canada. The original NBC algorithm code was developed by Pierre Miaskinof and Vasily Giannakeas.
#' The original performance metrics code was provided by Dr. Mireille Gomes whom also offered guidance in metrics implementation and user testing.
#' Special thanks to Richard Zehang Li for providing a standard structure for the package and Patrycja Kolpak for user testing of the GUI.
#'
#' @references
#' Use \code{citation("nbc4va")} to view citation information for the nbc4va package.
#' \itemize{
#'   \item Miasnikof P, Giannakeas V, Gomes M, Aleksandrowicz L, Shestopaloff AY, Alam D, Tollman S, Samarikhalaj, Jha P. Naive Bayes classifiers for verbal autopsies: comparison to physician-based classification for 21,000 child and adult deaths. BMC Medicine. 2015;13:286. doi:10.1186/s12916-015-0521-2.
#' }
#'
#' @examples
#' \dontrun{
#' library(nbc4va)
#'
#' # Quick start
#' # Follow the instructions in the web interface
#' nbc4vaGUI()
#'
#' # View user guides for the nbc4va package
#' browseVignettes("nbc4va")
#' }
#'
#' @author Richard Wen <\email{rrwen.dev@@gmail.com}>
#' @docType package
#' @name nbc4va
NULL

