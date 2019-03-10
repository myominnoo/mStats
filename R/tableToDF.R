#' @title Convert Table to Data Frame (Reversing Table)
#'
#' @description
#' \code{\link{tableTOdf}} transforms a contigency table into individual data frame.
#' @param table a table input
#' @param vars specify names of two variables
#' @param x.levels a vector corresponding to the levels in Column variable
#' @param y.levels a vector corresponding to the levels in Row variable
#' @details
#' By default, it takes a table and returns a data frame with rows corresponding to the
#' numbers in that table. If names are not specified, it returns data that contains "x"
#' and "y" variables. Names and levels can be specified.
#' See examples below.
#'
#' @seealso \code{\link{isum}}, \code{\link{itab}}
#' @keywords table to data frame, get raw data
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' t <- table(infert$education, infert$case)
#' t
#'
#' ## get raw data from table
#' head(tableTOdf(t))
#'
#' ## change variables' names
#' head(tableTOdf(t, vars = c("edu", "case")))
#'
#' ## specify names of levels of variables
#' head(tableTOdf(t, vars = c("edu", "case"), x.levels = c("No", "Yes")))
#'
#' str(tableTOdf(t, vars = c("edu", "case"), x.levels = c("No", "Yes")))
#' table(tableTOdf(t, vars = c("edu", "case"), x.levels = c("No", "Yes")))

#' @export
tableTOdf <- function(table, vars = NULL, x.levels = NULL, y.levels = NULL)

{
  if (!is.table(table)) stop("Input must be a table.")

  if (is.null(x.levels)) x.levels <- colnames(table)
  if (is.null(y.levels)) y.levels <- rownames(table)
  if (is.null(vars)) vars <- names(dimnames(table))
  if (all(vars == "")) vars <- c("x", "y")

  x <- factor(rep(x.levels, times = sapply(1:ncol(table), function(z) sum(table[, z]))),
               levels = x.levels)
  y <- NULL
  for (i in 1:ncol(table)) {
    y <- c(y, rep(y.levels, times = sapply(1:nrow(table), function(z) sum(table[z, i]))))
  }
  y <- factor(y, levels = y.levels)
  df <- data.frame(y = y, x = x)
  colnames(df) <- vars
  return(df)
}





#' @title Convert aggregate data frame to table
#' @param x a character

#' @export
aggreTOtable <- function(x) {
  return(x)
}
