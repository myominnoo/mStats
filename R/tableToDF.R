#' @title Table to Data Frame (Reverse Table)
#'
#' @description
#' \code{\link{tableToDF}} transforms a contigency table into individual data frame.
#' @param table a table input
#' @param var.names specify names of two variables
#' @param x.name a vector corresponding to the levels in Column variable
#' @param y.name a vector corresponding to the levels in Row variable
#' @details
#' by default, it takes a table and returns a data frame with values corresponding to the
#' numbers in that table. This is only for categorical data. If names are not specified,
#' it returns data that contains "x" and "y" variables. Names and levels can be specified. See the following examples.
#'
#' @seealso \code{\link{isum}}, \code{\link{itab}}, \code{\link{inumsum}},
#' \code{\link{ibarplot}}, \code{\link{iboxplot}}, \code{\link{ikdplot}},
#' \code{\link{plotOR}}
#' @keywords table to data frame, get raw data
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' t <- table(infert$education, infert$case)
#' t
#'
#' ## get raw data from table
#' tableToDF(t)
#'
#' ## change variables' names
#' tableToDF(t, var.names = c("edu", "case"))
#'
#' ## specify names of levels of variables
#' tableToDF(t, x.name = c("No", "Yes"))
#'
#' str(tableToDF(t, var.names = c("edu", "case"), x.name = c("No", "Yes")))
#' table(tableToDF(t, var.names = c("edu", "case"), x.name = c("No", "Yes")))

#' @export
tableToDF <- function(table, var.names = NULL, x.name = NULL, y.name = NULL)

{
  if (is.null(x.name)) x.name <- colnames(table)
  if (is.null(y.name)) y.name <- rownames(table)
  if (is.null(var.names)) var.names <- names(dimnames(table))
  if (all(var.names == "")) var.names <- c("x", "y")

  x <- factor(rep(x.name, times = sapply(1:ncol(table), function(z) sum(table[, z]))),
               levels = x.name)
  y <- NULL
  for (i in 1:ncol(table)) {
    y <- c(y, rep(y.name, times = sapply(1:nrow(table), function(z) sum(table[z, i]))))
  }
  y <- factor(y, levels = y.name)
  df <- data.frame(y = y, x = x)
  colnames(df) <- var.names
  return(df)
}
