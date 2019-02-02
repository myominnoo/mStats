#' @title A Quick Summary function with Graph Display using ggplot2
#'
#' @description
#' isum is a simple yet powerful function to produce a quick summary results of various types of data.
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
#' @seealso itab, ibarplot, inumsum, ikdplot
#' @keywords tabulation, frequency table, cross-tabulation, contigency table, summary statistics, dates
#' @examples
#' str(infert)
#' # summarize data.frame
#' isum(infert)
#'
#' # summarize coninuous data
#' isum(infert$age)
#' isum(infert$age, main = "My Plot", xlab = "Age") # edit plot title and label
#' # using data arugment
#' isum(age, data = infert, main = "My Plot", xlab = "Age")
#'
#' # summarize two variables
#' isum(age, education, infert)
#' isum(age, education, infert, facet = F)
#' isum(age, education, infert, main = "My Plot: Age ~ Education", xlab = "Age", legend.text = "Education", facet = F)
#'
#' str(iris)
#' isum(Sepal.Length, Species, iris)
#' isum(Sepal.Length, Species, iris, facet = F)

#' @export
isum <- function(x, by = NULL, data = NULL, rnd = 1, na.rm = TRUE,
                 plot.display = TRUE,  main = NULL, xlab = NULL, ylab = NULL,
                 legend.text = NULL, facet = TRUE, ...)
{
  # data input
  if (!is.null(data)) {
    arguments <- as.list(match.call())
    x <- eval(arguments$x, data)
    lab.x <- arguments$x
    by <- eval(arguments$by, data)
    lab.by <- arguments$by
  } else {
    lab.x <- deparse(substitute(x))
    if (!is.null(by)) {
      data <- data.frame(x = x, by = by)
      lab.by <- deparse(substitute(by))
    }
  }

  # number summary
  if (is.numeric(x)) {
    # plot title and labels
    if (is.null(xlab)) xlab <- lab.x
    if (is.null(ylab)) ylab <- 'Density'
    main <- ifelse(is.null(main),
                   ifelse(is.null(by), paste0('Kernel Density Plot of ', lab.x),
                   paste0('Kernel Density Plot of ', lab.x, ' ~ ', lab.by)), main)
    if (!is.null(by)) {
      by <- factor(by)
      legend.text <- ifelse(is.null(legend.text), lab.by, legend.text)
    }
    df <- inumsum(x = x, by = by, data = data, rnd = rnd, na.rm = na.rm,
            plot.display = plot.display, main = main, xlab = xlab, ylab = ylab,
            legend.text = legend.text, facet = facet, ...)
  }

  # tabulation
  if (is.character(x) | is.factor(x) | is.logical(x)) {
    # plot axis labels & legends
    if (is.null(main)) {
      if (is.null(by)) {main <- paste0('Plot of ', lab.x)} else {
        main <- paste0('Plot of ', lab.x, ' ~ ', lab.by)
      }
    }
    if (is.null(xlab)) xlab <- lab.x
    if (is.null(ylab)) ylab <- 'Freq'
    if (is.null(legend.text))
      legend.text <- ifelse(facet, xlab, lab.by)

    df <- itab(x = x, by = by, data = data, rnd = rnd, na.rm = na.rm,
                  plot.display = plot.display, main = main, xlab = xlab, ylab = ylab,
                  legend.text = legend.text, facet = facet, ...)
  }

  # date summary
  if (is.Date(x)) {
    if (is.null(xlab)) xlab <- lab.x
    df <- idatesum(x = x, groupby = "y", rnd = rnd, xlab = xlab)
  }

  # data frame summary
  if (is.data.frame(x)) {
    # create logical vector for date type
    v_date <- unlist(lapply(names(x), function(y)
      ifelse(is.Date(x[[y]]), TRUE, FALSE)))
    v_num <- unlist(lapply(names(x), function(y)
      ifelse(is.numeric(x[[y]]), TRUE, FALSE)))
    v_freq <- !v_date & !v_num

    # number summary
    num <- do.call(rbind, lapply(names(x)[v_num], function(y)
      inumsum(x = x[,y], by = by, data = data, rnd = rnd, na.rm = na.rm,
           plot.display = plot.display,
           main = paste0('Kernel Density Plot of ', toString(y)),
           xlab = toString(y))
    ))
    row.names(num) <- names(x)[v_num]

    # factor summary
    freq <- lapply(names(x)[v_freq], function(y)
      itab(x = x[,y], by = by, data = data, rnd = rnd, na.rm = na.rm,
           plot.display = plot.display,
           main = paste0('Plot of ', toString(y)),
           xlab = toString(y),
           legend.text = toString(y)))
    freq <- structure(freq, names = names(x)[v_freq])

    # date summary
    date <- lapply(names(x)[v_date], function(y)
      idatesum(x = x[,y], rnd = rnd, plot.display = plot.display,
               xlab = toString(y)))
    date <- structure(date, names = names(x)[v_date])

    # data.frame summary output
    if(any(v_num)) flag <- "a"
    if(any(v_num) & any(v_freq)) flag <- "b"
    if(any(v_num) & any(v_freq) & any(v_date)) flag <- "c"
    df <- switch(flag,
                 a = num,
                 b = list(Number.Summary = num,
                          Freq.Summary = freq),
                 c = list(Number.Summary = num,
                          Freq.Summary = freq,
                          Date.Summary = date))
  }
  return(df)
}
