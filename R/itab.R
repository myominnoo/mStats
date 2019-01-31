#' @title Tabulation, Cross Tabulation and Graphical Display
#'
#' @description
#' itab is a simple function for data exploration and used to build frequency table of a factor variable or contigency table of the counts at each combination of factor levels.
#'
#' @param x a vector describing the bars which make up the plot. It is usually on x axis.
#' @param by a vector describing the grouping of x. By default, the plot generates a faceted barplot.
#' @param data an optional data frame (or object coercible by as.data.frame to a data frame) containing the variables for contigency table.
#' @param rnd an integer indicating the number of decimal places:
#' @param na.rm A logical value indicating to remove NA values in the table or not. By Default, the value is TRUE
#' @param main a main title for the plot.
#' @param xlab a label for the x axis, defaults to a description of x.
#' @param ylab a label for the y axis, defaults to a description of y.
#' @param legend.text a text for the title of the plot
#' @param facet a logical value. If TRUE, the plot forms a matrix of panels defined by column faceting variables. It is most useful when you have two discrete variables, and all combinations of the variables exist in the data. By default, it is set to TRUE.
#' @param ... additional arguments affecting the display of graph: See the following arguments.
#' @param bar.color a text to depict the outer line of bars. You can set "black", "green", "blue" and so on.
#' @param vjust a number to place the value label in percentage to the bar.
#' @param hjust a number to place the value label in percentage to the bar.
#' @param legend.position the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param save.plot a logical value. If TRUE, it saves the plot generated in the current working directory.
#' @param plot.name a text for plot filename. Suffix can be ".png", ".tiff" and ".pdf"
#' @param width a value in inches
#' @param height a value in inches
#' @param dpi a value for resolution of the plot saved.
#' @seealso ibarplot, inumsum, ikdplot
#' @keywords tabulation, frequency table, cross-tabulation, contigency table
#' @examples
#' str(infert)
#' data(infert)
#' # tabulation of one factor variable
#' itab(education, data = infert)
#' itab(parity, data = infert)
#' itab(case, data = infert)
#' itab(spontaneous, data = infert)
#' # cross-tabulation
#' itab(case, education, infert)
#' itab(case, education, infert, facet = F)
#' itab(case, parity, infert)
#' itab(case, parity, infert, facet = F)
#' itab(case, parity, infert, plot.display = FALSE)

#' @export
itab <- function(x, by  = NULL, data = NULL, rnd = 1, na.rm = FALSE,
                 plot.display = TRUE, main = NULL, xlab = NULL, ylab = NULL,
                 legend.text = NULL, facet = TRUE, ... )
{
  # levels of useNA in table() has c("no", "ifany", "always)
  include.na <- ifelse(na.rm, "no", "ifany")

  # data input
  if (!is.null(data)) {
    arguments <- as.list(match.call())
    x <- eval(arguments$x, data)
    lab.x <- arguments$x
    by <- eval(arguments$by, data)
    lab.by <- arguments$by
  } else {
    lab.x <- deparse(substitute(x))
    if (!is.null(by)) {lab.by <- deparse(substitute(by))}
  }

  # plot axis labels & legends
  if (is.null(main)) {
    if (is.null(by)) {main <- paste0('Plot of ', lab.x)} else {
      main <- paste0('Plot of ', lab.x, ' ~ ', lab.by)
    }
  }
  if (is.null(xlab)) xlab <- lab.x
  if (is.null(ylab)) ylab <- 'Freq'
  if (is.null(legend.text)) legend.text <- ifelse(facet, xlab, lab.by)

  # table
  if (is.null(by)) {
    # frequency table
    tbl <- table(x, useNA = include.na)
    t.count <- c(tbl, Total = sum(tbl))
    c.count <- c(cumsum(tbl), sum(tbl))
    names(c.count) <- names(t.count)
    r.freq <- t.count / sum(tbl) * 100
    c.freq <- c.count / sum(tbl) * 100
    names(c.freq) <- names(t.count)
    df <- as.data.frame(cbind(Count. = t.count,
                              Rel.Freq = round(r.freq, rnd),
                              Cum.Freq = round(c.freq, rnd)))
  } else {
    # contigency table
    tbl <- table(x, by, useNA = include.na)
    tbl.col <- rbind(tbl, Total = colSums(tbl))
    tbl.row <- cbind(tbl, Total = rowSums(tbl))
    tbl.total <- cbind(tbl.col, Total = rowSums(tbl.col))
    pct.col <- vector("numeric", 0)
    for(i in 1:ncol(tbl.total)) {
      pct.col <- cbind(pct.col, tbl.total[ ,i] / tbl.total[nrow(tbl.total),i] * 100)
    }
    pct.row <- vector("numeric", 0)
    for(i in 1:nrow(tbl.col)) {
      pct.row <- rbind(pct.row, tbl.total[i,] / tbl.total[i, ncol(tbl.total)] * 100)
    }
    colnames(pct.col) <- c(rep("(%)", ncol(pct.col)-1), "(%)")
    colnames(pct.row) <- c(rep("(%)", ncol(pct.col)-1), "(%)")
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
    names(dimnames(tbl.total)) <- c(xlab, lab.by)
    names(dimnames(tab.row)) <- c(xlab, lab.by)
    names(dimnames(tab.col)) <- c(xlab, lab.by)
    df <- list(Count. = tbl.total,
                   Row.Percentage = tab.row,
                   Col.Percentage = tab.col)
  }

  # plot display
  if (plot.display) {
    if (is.null(by)) {
      plot(ibarplot(x = x, data = data, rnd = rnd, na.rm = na.rm,
                    main = main, xlab = xlab, ylab = ylab, legend.text = legend.text,
                    facet = facet, ...))
    } else {
      plot(ibarplot(x = x, by = by, data = data, rnd = rnd, na.rm = na.rm,
                    main = main, xlab = xlab, ylab = ylab, legend.text = legend.text,
                    facet = facet, ...))
    }
  }
  return(df)
}
