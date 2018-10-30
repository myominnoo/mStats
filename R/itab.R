#' Cross-tabulation between two categorical variables with Chi-Squre Test
#'
#' @description
#' itab() quickly provides three cross-tabulation results between two categorical variables
#' and quickly adds Chi-Square result as well as Fisher Exact Test result.
#'
#' @param x An R Object: represents Row variable
#' @param y An R Object: represents Column variable
#' @param use.na A logical value indicating whether to include NA values in the table or not.
#' By Default, the value is FALSE.
#' @param rnd an integer indicating the number of decimal places:
#' @seealso isum, isum.factor, isum.numeric, isum.data.frame, rall
#' @keywords cross-tabulation, categorical, basic statistics, quick summary
#' @export
#' @examples
#' itab(x, y)

itab <- function(x, y, use.na = FALSE, rnd = 1) {
  # if(all(is.factor(x), is.factor(y))) {
    if(use.na) {use.na <- "always"} else {use.na <- "no"}
    tbl <- table(x, y, useNA = use.na)
    tbl.col <- rbind(tbl, Total = colSums(tbl))
    tbl.row <- cbind(tbl, Total = rowSums(tbl))
    tbl.total <- cbind(tbl.col, Total = rowSums(tbl.col))
    pct.col <- vector("numeric", 0)
    for(i in 1:ncol(tbl.total)) {
      pct.col <- cbind(pct.col, tbl.total[ ,i] / tbl.total[nrow(tbl.total),i])
    }
    pct.row <- vector("numeric", 0)
    for(i in 1:nrow(tbl.col)) {
      pct.row <- rbind(pct.row, tbl.total[i,] / tbl.total[i, ncol(tbl.total)])
    }
    colnames(pct.col) <- c(rep("Col.", ncol(pct.col)-1), "Col.total")
    colnames(pct.row) <- c(rep("Row.", ncol(pct.col)-1), "Row.total")
    pct.col <- round(pct.col, rnd)
    pct.row <- round(pct.row, rnd)
    tab.col <- vector("numeric", 0)
    var.col <- vector("numeric", 0)
    for(i in 1:ncol(tbl.total)) {
      tab.col <- cbind(tab.col, tbl.total[ ,i], pct.col[ ,i])
      var.col <- c(var.col, colnames(tbl.total)[i], colnames(pct.col)[i])
    }
    colnames(tab.col) <- var.col
    tab.row <- vector("numeric", 0)
    var.row <- vector("numeric", 0)
    for(i in 1:ncol(tbl.total)) {
      tab.row <- cbind(tab.row, tbl.total[ ,i], pct.row[ ,i])
      var.row <- c(var.row, colnames(tbl.total)[i], colnames(pct.row)[i])
    }
    colnames(tab.row) <- var.row
    if(min(tbl) < 5) {c.correct <- TRUE}
    result <- list(Count. = tbl.total,
         Col.Percentage = tab.col,
         Row.Percentage = tab.row,
         Chi.Square.Test = chisq.test(tbl, correct = c.correct),
         fisher.Exact.Test = fisher.test(tbl))
    plot(tbl)
    return(result)
  # } else {stop("... x or y object is not a factor ...")}
}
