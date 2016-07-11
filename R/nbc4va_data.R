# Richard Wen (rwenite@gmail.com)
# Code for data functions in the nbc4va package.


#' Example of clean data in nbc4va
#'
#' A random generation of clean verbal autopsy synthetic data for use in demonstrating the nbc4va package.
#'
#' @format A dataframe with 100 rows and 102 columns:
#' \itemize{
#'   \item id (vectorof char): the case identifiers
#'   \item cause (vectorof char): the cause of death for each case
#'   \item symptom1..100 (vectorsof (1 OR 0)): whether the symptom is recorded as present (1) or not (0) for each case (row)
#'   \item Example:
#'   \tabular{ccccc}{
#'     id \tab cause \tab symptom1 \tab symptom2 \tab symptom3 \cr
#'     "a27" \tab "cause10" \tab 1 \tab 0 \tab 0\cr
#'     "k37" \tab "cause2" \tab 0 \tab 0 \tab 1\cr
#'     "e57" \tab "cause8" \tab 1 \tab 0 \tab 0
#'   }
#' }
#' @source Random generation using the \code{\link{sample}} function with \code{\link{set.seed}} set to 1.
#'
#' @examples
#' library(nbc4va)
#' data(nbc4vaData)
"nbc4vaData"


#' Example of unclean data in nbc4va
#'
#' A random generation of unclean verbal autopsy synthetic data for use in demonstrating the nbc4va package.
#'
#' @details \strong{Warning}: This data may produce errors depending on how you use it in the package.
#'
#' @format A dataframe with 100 rows and 102 columns:
#' \itemize{
#'   \item id (vectorof char): the case identifiers
#'   \item cause (vectorof char): the cause of death for each case
#'   \item symptom1..100 (vectorsof (1 OR 0 OR 99)): whether the symptom is recorded as present (1), absent (0), or unknown (99) for each case (row)
#'   \item Example:
#'   \tabular{ccccc}{
#'     id \tab cause \tab symptom1 \tab symptom2 \tab symptom3 \cr
#'     "a27" \tab "cause10" \tab 99 \tab 0 \tab 1\cr
#'     "k37" \tab "cause2" \tab 0 \tab 99 \tab 1\cr
#'     "e57" \tab "cause8" \tab 1 \tab 0 \tab 99
#'   }
#' }
#' @source Random generation using the \code{\link{sample}} function with \code{\link{set.seed}} set to 1.
#'
#' @examples
#' library(nbc4va)
#' data(nbc4vaDataRaw)
"nbc4vaDataRaw"


#' Substitute values in a dataframe proportionally to all other values
#'
#' Substitute a target value proportionally to the distribution of the rest of the values in a column, given the following conditions:
#' \itemize{
#'   \item If a column contains only the target value, the column is removed
#'   \item If there are not enough target values to be distributed, then each target value will be
#'   randomly sampled from the rest of the column values with replacement
#' }
#'
#' @details Pseudocode of algorithm:
#' \preformatted{
#'   SET dataset = table of values with columns and rows
#'   SET x = target value for substitution
#'
#'   IF x in dataset:
#'     FOR EACH column y in a dataset:
#'       SET xv = all x values in y
#'       SET rest = all values not equal to x in y
#'       IF xv == values in y:
#'         REMOVE y in dataset
#'       IF number of unique values of rest == 1:
#'         MODIFY xv = rest
#'       IF number of xv values < number of unique values of rest:
#'         SET xn = number of xv values
#'         MODIFY xv = random sample of rest with size xn
#'       ELSE:
#'         SET xn = number of xv values
#'         SET p = proportions of rest
#'         SET xnp = xn * p
#'         IF xnp has decimals:
#'           MODIFY xnp = round xnp such that sum(xnp) == xn via largest remainder method
#'         MODIFY xv = rest values with distribution of xnp
#'   RETURN dataset
#' }
#'
#' @param dataset A dataframe with value(s) of \emph{x} in it.
#' @param x A target value in dataframe to replace with the rest of values per column.
#' @param cols A numeric vector of columns to consider for substitution.
#' @param ignore A vector of the rest of the values to ignore for substitution.
#' @param removal Set to TRUE to remove column(s) that consist only of \emph{x} values.
#' @return out A dataframe or list depending on \emph{removal}:
#' \itemize{
#'   \item if (\emph{removal} is FALSE) return the \emph{dataset} with values of \emph{x} substituted by the rest of the values per column
#'   \item if (\emph{removal} is TRUE) return a list with the following:
#'   \itemize{
#'     \item $removed (vectorof numeric): the removed column indices if the column(s) consists only of \emph{x} values
#'     \item $dataset (dataframe): the \emph{dataset} with values of \emph{x} substituted by the rest of the values per column
#'   }
#' }
#'
#' @examples
#' library(nbc4va)
#' data(nbc4vaDataRaw)
#' unclean <- nbc4vaDataRaw
#' clean <- nbc4va:::internalSubAsRest(unclean, 99)
#'
#' @family data functions
#' @keywords internal
internalSubAsRest <- function(dataset, x, cols=1:ncol(dataset), ignore=c(NA, NaN), removal=FALSE) {
  if (length(x) != 1) stop(x, " is not a valid value (length must equal 1).")

  # (Sub_Rest) Substitute x as rest distribution with conditions
  ydrop <- c()
  for (y in cols) {

    # (Sub_Vars) Set variables for substitution
    yv <- dataset[, y]  # all values in y
    yv <- yv[!yv %in% ignore]
    yn <- length(yv)
    xi <- which(dataset[, y] == x)  # idx of y which are x
    xn <- length(xi)  # number of values which are x
    rest <- yv[yv != x]  # values of y which are not x
    restu <- unique(rest)

    # (Sub_Cond) Apply conditions before substituting
    if (length(xi) > 0) {
      if ((xn == yn) && removal) {  # all x values, remove col
        ydrop <- c(ydrop, y)
      } else if (length(restu) == 1) {  # only 1 value of rest, set to rest
        dataset[xi, y] <- restu
      } else if (xn < length(restu)) {  # not enough values of x, sample from rest
        dataset[xi, y] <- sample(rest, xn, replace=TRUE)
      } else {  # sub proportionate
        restp <- sapply(restu,
                        function(v, yv, n) length(yv[yv==v])/n,
                        yv=yv,
                        n=length(rest))  # prop of rest
        xnp <- restp * xn
        xnp <- internalRoundFixedSum(xnp) # round if there are decimals
        dataset[xi, y] <- rep(restu, xnp)
      }
    }
  }

  # (Return) Return the substituted dataframe
  if (length(ydrop) > 0) {
    dataset <- dataset[, -ydrop]
  }
  if (removal) {  # return removed col indices if needed
    out <- list(removed=ydrop, dataset=dataset)
  } else {
    out <- dataset
  }
  return(out)
}


#' Round values to whole numbers while preserving the sum
#'
#' Rounds a vector of values to whole numbers while preserving the sum (rounded if it is not a whole number) using the largest remainder method \href{https://www.tcd.ie/Political_Science/staff/michael_gallagher/ElectoralStudies1991.pdf}{(Gallagher, 1991)}.
#'
#' @param v A vector of values with decimal values and a whole number sum to round.
#' @param roundSum If the sum of the values in \emph{v} is not a whole number, choose a rounding method to ensure it is a whole number.
#' @return out A vector of \emph{v} with the values rounded to whole numbers but with the whole number sum preserved.
#'
#' @references
#' \itemize{
#'   \item Gallagher M. Proportionality, disproportionality and electoral systems. Electoral Studies. 1991;10(1)33-51. doi:10.1016/0261-3794(91)90004-C.
#' }
#'
#' @examples
#' library(nbc4va)
#' dec <- c(rep(50/2, 2), rep(50/3, 3))
#' whole <- nbc4va:::internalRoundFixedSum(dec)
#'
#' @importFrom utils tail
#' @family data functions
#' @keywords internal
internalRoundFixedSum <- function (v, roundSum=round) {
  if (all(v%%1 == 0)) {
    out <- v
  } else {
    vfloor <- floor(v)
    vfrac <- v - vfloor  # fractional parts f
    kfrac <- roundSum(sum(v)) - sum(vfloor)  # k largest fractional parts f
    i <- tail(order(vfrac), kfrac)  # idx of k largest frac in v
    vfloor[i] <- vfloor[i] + 1  # add 1 to k largest frac in v
    out <- vfloor
  }
  return(out)
}

