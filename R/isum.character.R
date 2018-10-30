#' A Quick Summary function with Graph Display
#'
#' @description
#' isum() lets you do a quick summary on your data
#'
#' For the class "factor", it tabulates and gives frequency, percentages and
#'     cummulative percentages. The same is with the class "character".
#'
#' For the class "integer", it provides seven summary measures:
#'     mean, standard deviation, median, Q1, Q3, minimum and maximum.
#'
#' For the class "data.frame", it gives a list of all the variables based on
#'     their data types.
#'
#' @param ... Additional arguments
#' @seealso isum.factor, isum.numeric, isum.data.frame
#' @keywords summarize, isum, basic statistics, quick summary
#' @export
#' @examples
#' isum(iris)

isum.character <- function(...) {
  isum.factor(...)
}
