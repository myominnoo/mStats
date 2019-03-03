#' @title A Quick Summary of data frame or data
#'
#' @description
#' A simple yet powerful function to produce a quick summary of various types of data or data frame
#'
#' @param x a vector describing the bars which make up the plot. It is usually on x axis.
#' @param y a vector describing the y axis or second variable in cross-tabulation or data relationship.
#' @param by a vector describing the grouping of x. By default, the plot generates a faceted barplot.
#' @param data an optional data frame (or object coercible by as.data.frame to a data frame) containing the variables for contigency table.
#' @param rnd an integer indicating the number of decimal places:
#' @param na.rm A logical value indicating to remove NA values in the table or not. By Default, the value is TRUE
#' @param pct type of percentages in cross-tabulation: by default, it shows row percentages in two
#' @param groupby a character value, indicating patterns of aggregation either by one of the followings:
#'
#' "y" - Year along
#'
#' "ym" - Year + Month
#'
#' "ymd" - Year + Month + Day
#'
#' @param plot.display logical value, indicating whether plot will be displayed or not
#' @param main a main title for the plot.
#' @param xlab a label for the x axis, defaults to a description of x.
#' @param ylab a label for the y axis, defaults to a description of y.
#' @param show.legend show or hide the legend. Hide the legend by default
#' @param legend.text Legend title
#' @param plot.type if bivariate analysis, type of plot can be specified. By default, this generates a facted plot. 'p' for parrallel barplot, 's' for stacked barplot, 'fs' for full stacked percentage barplot.
#' @param facet.ncol number of columns to be faceted
#' @param plot.save a logical value. If TRUE, it saves the plot generated in the current working directory.
#' @param plot.name a text for plot filename. Suffix can be ".png", ".tiff" and ".pdf"
#' @param width a value in inches
#' @param height a value in inches
#' @param dpi a value for resolution of the plot saved.
#' @import ggplot2
#' @seealso ibarplot, inumsum, ikdplot, iboxplot, isum
#' @keywords tabulation, frequency table, cross-tabulation, contigency table, summary statistics, dates
#' @examples
#' str(infert)
#' # summarize data.frame
#' isum(infert)
#' isum(age, data = infert) # numeric data
#' isum(ltf$age)
#' isum(ltf$age, boxplot = FALSE) # calling kernel density plot
#' isum(age, education, data = infert) # continuous vs category data
#' isum(age, education, data = infert, boxplot = FALSE)
#' isum(age, pooled.stratum, data = infert) # continuous vs continuos data
#' isum(age, pooled.stratum, data = infert, boxplot = FALSE)
#' infert$case <- factor(infert$case) # stratified variable must be a factor
#' isum(age, education, case, data = infert) # stratified analysis
#' isum(age, education, case, data = infert, boxplot = FALSE)
#' isum(age, education, case, data = infert, boxplot = FALSE, alpha = 1) # block transparency
#' isum(age, pooled.stratum, education, data = infert)
#'
#' isum(education, data = infert) # categorical data
#' isum(education, case, data = infert) # row percentage by default
#' isum(education, case, data = infert, pct = 'col')
#' isum(education, case, data = infert, pct = 'all')
#' isum(education, case, data = infert, pct = 'none')
#' isum(education, case, data = infert) # faceted plot by default
#' isum(education, case, data = infert, plot.type = 'p')
#' isum(education, case, data = infert, plot.type = 's')
#' isum(education, case, data = infert, plot.type = 'fs')
#' isum(education, case, data = infert)
#'
#' str(iris)
#' isum(iris)
#' isum(Sepal.Length, data = iris)
#' isum(Sepal.Length, Sepal.Width, data = iris)
#' isum(Sepal.Length, Species, data = iris)
#' isum(Sepal.Length, Species, data = iris, boxplot = FALSE)
#' isum(Sepal.Length, Species, data = iris, boxplot = FALSE, alpha = 1)


#' @export
isum <- function(x, y = NULL, by = NULL, data = NULL, rnd = 1, na.rm = FALSE,
                 pct = "row", groupby = "ym",
                 plot.display = TRUE, plot.type = "f", boxplot = TRUE,
                 main = NULL, xlab = NULL, ylab = NULL, alpha = 0.1,
                 show.legend = TRUE, legend.text = NULL, facet.ncol = 2,
                 plot.save = FALSE, plot.name = 'isum.tiff',
                 width = 5, height = 4, dpi = 150)
{
  if (!is.null(data)) {
    arguments <- as.list(match.call())
    x <- eval(substitute(x), data)
    y <- eval(substitute(y), data)
    by <- eval(substitute(by), data)

    lab.x <- arguments$x
    lab.y <- arguments$y
    lab.by <- arguments$by
  } else {
    lab.x <- deparse(substitute(x))
    if (!is.null(y)) lab.y <- deparse(substitute(y))
    if (!is.null(by)) lab.by <- deparse(substitute(by))
  }

  # depend on x
  if (is.numeric(x) | class(x) == "difftime") {
    if (is.table(x)) stop(paste0(lab.x, " is a ", class(x), "."))
    if (class(x) == "difftime")  x <- as.numeric(x)
    df <- inumsum(x = x, y = y, by = by, rnd = rnd, na.rm = na.rm,
                   x.varname = lab.x, y.varname = lab.y, by.varname = lab.by,
                   plot.display = plot.display, boxplot = boxplot,
                   main = main, xlab = xlab, ylab = ylab, alpha = alpha,
                   show.legend = show.legend, legend.text = legend.text,
                   facet.ncol = facet.ncol, plot.save = plot.save, plot.name = plot.name,
                   width = width, height = height, dpi = dpi)
  } else if (is.character(x) | is.factor(x) | is.logical(x)) {
    df <- itab(x = x, y = y, by = by, rnd = rnd, na.rm = na.rm,
                x.varname = lab.x, y.varname = lab.y, by.varname = lab.by,
                pct = pct, plot.display = plot.display, main = main, xlab = xlab,
                ylab = ylab, show.legend = show.legend, legend.text = legend.text,
                plot.type = plot.type, facet.ncol = facet.ncol, plot.save = plot.save,
                plot.name = plot.name, width = width, height = height, dpi = dpi)
  } else if (is.Date(x)) {
    if (is.null(xlab)) xlab <- lab.x
    df <- idatesum(x = x, groupby = groupby, rnd = rnd, plot.display = plot.display,
                   xlab = xlab)
  } else if (is.data.frame(x)) {
    # create logical vector for date type
    v_date <- unlist(lapply(names(x), function(y)
      ifelse(is.Date(x[[y]]), TRUE, FALSE)))
    v_num <- unlist(lapply(names(x), function(y)
      ifelse(is.numeric(x[[y]]), TRUE, FALSE)))
    v_freq <- !v_date & !v_num

    # number summary
    num <- do.call(rbind, lapply(names(x)[v_num], function(z)
      inumsum(x = x[,z], x.varname = z, rnd = rnd, na.rm = na.rm, plot.display = FALSE)
    ))
    # row.names(num) <- names(x)[v_num]

    # factor summary
    freq <- lapply(names(x)[v_freq], function(z)
      itab(x = x[,z], x.varname = z, rnd = rnd, na.rm = na.rm, pct = pct,
           plot.display = FALSE))
    freq <- structure(freq, names = names(x)[v_freq])

    # date summary
    date <- lapply(names(x)[v_date], function(z)
      idatesum(x = x[,z], rnd = rnd, plot.display = FALSE))
    date <- structure(date, names = names(x)[v_date])

    # data.frame summary output
    if (any(v_num)) flag <- "a"
    if (any(v_freq)) flag <- "b"
    if (any(v_date)) flag <- "c"
    if (any(v_num) & any(v_freq)) flag <- "d"
    if (any(v_freq) & any(v_date)) flag <- "e"
    if (any(v_num) & any(v_date)) flag <- "f"
    if (any(v_num) & any(v_freq) & any(v_date)) flag <- "g"
    df <- switch(flag,
                 a = num,
                 b = freq,
                 c = date,
                 d = list(Number.Summary = num, Freq.Summary = freq),
                 e = list(Freq.Summary = freq, Date.Summary = date),
                 f = list(Number.Summary = num, Date.Summary = date),
                 g = list(Number.Summary = num, Freq.Summary = freq,
                          Date.Summary = date))
  } else {
    print(paste0(lab.x, " is a ", class(x), "."))
  }
  return(df)
}
