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

isum.data.frame <- function(x, ...) {
  v.names <- names(x)
  ds <- data.frame()
  n.var <- vector(mode="numeric", length=0)
  freq <- list()
  for(i in 1:ncol(x)) {
    if(ncol(isum(x[[i]])) > 3) {
      ds <- rbind(ds, isum(x[[i]], ...))
      n.var <- c(n.var, i)
    } else {
      freq[[v.names[i]]] <- isum(x[[i]], ...)
    }
  }
  row.names(ds) <- v.names[n.var]
  return(list(number.summary = ds,
              freq.summary = freq))
}

