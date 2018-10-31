#' Check if an R object is of Date class
#'
#' @description
#' This function tests if an R object is an object of class "Date" representing
#' calendar dates.
#'
#' @param x an R object
#' @seealso isum.factor, isum.numeric, isum.data.frame
#' @keywords summarize, isum, basic statistics, quick summary
#' @export
#' @examples
#' isum(iris)

is.date <- function(x) {
  return(ifelse(class(x) == "Date", TRUE, FALSE))
}
