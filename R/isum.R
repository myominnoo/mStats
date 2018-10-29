#' A Quick Summary function with Graph Display
#'
#' @description
#' isum() lets you do a quick summary on your data type.
#'
#' For "factor" type, it tabulates and gives frequency, percentages and
#'     cummulative percentages.
#' For "integer" type, it provides seven summary measures:
#'     mean, standard deviation, median, Q1, Q3, minimum and maximum.
#' For "data.frame" type, it gives a list of all the variables based on
#'     their data types.
#'
#' @param x An R object: it can be a dataframe or a vector.
#' @param ... Additional parameters
#' @param l.size An integer value determining whether the variable should be treated
#'     as factor
#'
#' @param rnd an integer indicating the number of decimal places:
#' @param na.rm A logical value indicating whether "NA" missing values should be
#'     removed before the computation proceeds.
#'
#' @seealso isum.factor, isum.numeric, isum.data.frame
#' @keywords summarize, isum, basic statistics, quick summary
#' @export
#' @examples
#' isum(iris)

isum <- function(x, ...) {
  UseMethod("isum", x)
}

