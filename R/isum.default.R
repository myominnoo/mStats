#' A Quick Summary function with Graph Display
#'
#' @description
#' isum() lets you do a quick summary on your data type.
#'
#' For "factor" data type, it tabulates and gives frequency, percentages and
#'     cummulative percentages.
#' For "integer" data type, it provides seven summary measures:
#'     mean, standard deviation, median, Q1, Q3, minimum and maximum.
#'
#' @seealso isum.factor, isum.numeric, isum.data.frame
#' @keywords summarize, isum, basic statistics, quick summary
#' @examples
#' isum()


isum <- function(x, ..., l.size = 10L) {
  if (is.data.frame(x)) {
    print("...x is a dataframe ...")
  } else {
    if (length(unique(x)) < l.size) {
      x <- factor(x)
      UseMethod("isum", x)
    } else {
      UseMethod("isum", x)
    }
  }
}
