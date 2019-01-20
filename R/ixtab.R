#' @title Cross-Tabulation with Graph Display
#'
#' @description
#' ixtab is a function that produces three contingency tables: one with counts only, another one with
#' counts and percentages for rows and the last one with counts and percentages for columns. In
#' addition, it automatically calculates Chi Square test and Fisher Exact test as well as displays
#' mosaic plot.
#'
#' Plot can be disabled by setting plot.display to FALSE.
#'
#' @param x An R object, categorical data which represents Row variable
#' @param y An R object, categorical data which represents Column variable
#' @param data an optional data frame (or object coercible by as.data.frame to a data frame)
#' containing the variables for contigency table.
#' @param rnd an integer indicating the number of decimal places:
#' @param na.rm A logical value indicating to remove NA values in the table or not.
#' By Default, the value is TRUE
#' @param plot.display A logical value, indicating whether the data will be plotted and display
#' @param plot.title A character value, specifying the name of the plot in categorical or continuous data. This argument does not work in summaring data.frame object, where the plot will be automatically mapped to the names of the data.frame.
#' @seealso isum, igroup
#' @keywords cross-tabulation, contingency table, categorical summary
#' @examples
#' str(infert)
#' ixtab(induced, case, infert)
#' ixtab(education, case, infert)
#' ixtab(spontaneous, case, infert)
#' ixtab(spontaneous, case, infert, plot.display = FALSE) # disable plotting

#' @export
ixtab <- function(x, y, data = NULL, rnd = 1, na.rm = TRUE, plot.display = TRUE, plot.title = NULL) {
  na.rm <- ifelse(na.rm, "no", "always")
  if(is.null(data)) {tbl <- table(x, y, useNA = na.rm)} else {
    arguments <- as.list(match.call())
    x <- eval(arguments$x, data)
    y <- eval(arguments$y, data)
    tbl <- table(x, y, useNA = na.rm)
  }

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
                 fisher.Exact.Test = fisher.test(tbl, simulate.p.value = TRUE))
  if (plot.display) {
    mosaicplot(tbl, color = rainbow(nrow(tbl)),
               main = paste0("Mosaic Plot: ", arguments$x, " ~ ", arguments$y),
               xlab = arguments$x,
               ylab = arguments$y)
  }
  return(result)
}

