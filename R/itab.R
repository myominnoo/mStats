#' Cross-tabulation between two categorical variables with Chi-Squre Test
#'
#' @description
#' itab() quickly provides three cross-tabulation results between two categorical variables
#' and quickly adds Chi-Square result as well as Fisher Exact Test result.
#'
#' @param x An R Object: represents Row variable
#' @param y An R Object: represents Column variable
#' @param rnd an integer indicating the number of decimal places:
#' @param na.rm A logical value indicating to remove NA values in the table or not.
#' By Default, the value is TRUE
#' @seealso isum, isum.factor, isum.numeric, isum.data.frame, rall
#' @keywords cross-tabulation, categorical, basic statistics, quick summary
#' @export
#' @examples
#' itab(x, y)

itab <- function(x, y, data = NULL, rnd = 1, na.rm = TRUE) {
  na.rm <- ifelse(na.rm, "no", "always")
  if(is.null(data)) {tbl <- table(x, y, useNA = na.rm)} else {
    tbl <- table(data[[x]], data[[y]], useNA = na.rm)}
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
  colnames(pct.col) <- c(rep("(Pct)", ncol(pct.col)-1), "(Pct.Total)")
  colnames(pct.row) <- c(rep("(Pct)", ncol(pct.col)-1), "(Pct.Total)")
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
  c.correct <- ifelse(min(tbl) < 5, TRUE, FALSE)
  result <- list(Count. = tbl.total,
                 Row.Percentage = tab.row,
                 Col.Percentage = tab.col,
                 Chi.Square.Test = chisq.test(tbl, correct = c.correct),
                 fisher.Exact.Test = fisher.test(tbl))
  mosaicplot(tbl, color = rainbow(nrow(tbl)),
             main = paste0("Mosaic Plot: '", deparse(substitute(x)),
                           "' ~ '", deparse(substitute(y)), "'"),
             xlab = deparse(substitute(x)),
             ylab = deparse(substitute(y)))
  return(result)
}
