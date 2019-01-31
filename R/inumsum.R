#' @title Number Summary and Plot using ggplot2
#'
#' @description
#' inumsum (I number-summarize) generates seven-number summary for one variable which include number of observations, missing records, mean, standard deviation, median, interquartile range, minimum, maximum. If the x variable is summarized by a factor variable, then the output will include seven-number summary plus standard error, 95% confidence intervals for means. In addition, the function generates automated Kernel density plot with optional parameters for graphical display.
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
#' @param legend.position the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param save.plot a logical value. If TRUE, it saves the plot generated in the current working directory.
#' @param plot.name a text for plot filename. Suffix can be ".png", ".tiff" and ".pdf"
#' @param width a value in inches
#' @param height a value in inches
#' @param dpi a value for resolution of the plot saved.
#' @seealso itab, ibarplot, ikdplot
#' @keywords number summary, summary statistics, summary grouping
#' @examples
#' inumsum(x = age, by = education, data = infert)
#' inumsum(age, data = infert)
#' inumsum(infert$age)
#' inumsum(infert$age, infert$education)

#' @export
inumsum <- function(x, by  = NULL, data = NULL, rnd = 1, na.rm = TRUE,
                    plot.display = TRUE, main = NULL, xlab = NULL, ylab = NULL,
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
      lab.by <- deparse(substitute(by))
    }
  }

  # missing value removed
  if (!is.null(by)) {
    if (length(x) > length(by)) {
      by <- c(by, rep(NA, length(x) - length(by)))
    } else {
      x <- c(x, rep(NA, length(by) - length(x)))
    }
    data <- data.frame(x = x, by = by)
  }
  # plot title and labels
  if (is.null(main)) {
    main <- ifelse(is.null(by), paste0('Kernel Density Plot of ', lab.x),
                   paste0('Plot of ', lab.x, ' ~ ', lab.by))
  }
  if (is.null(xlab)) xlab <- lab.x
  if (is.null(ylab)) ylab <- 'Density'
  if (is.null(legend.text)) legend.text <- ifelse(is.null(by), NA, lab.by)

  # condition for by variable
  if (is.null(by)) {
    df <- inumsum.numeric(x = x, rnd = rnd, na.rm = na.rm)
  } else {
    df <- do.call(rbind, lapply(levels(by),
                                function(i) inumsum.numeric(x[by == i], rnd, na.rm)))
    df <- do.call(rbind, list(Overall = inumsum.numeric(x = x, rnd = rnd, na.rm = na.rm), df))
    df <- cbind(df, SE = round(df$SD / sqrt(df$Mean), rnd))
    df <- cbind(df,
                CI.Lwr = round(df$Mean - (1.96 * df$SE), rnd),
                CI.Upr = round(df[,3] + (1.96 * df[,5]), rnd))
    df <- do.call(rbind, list(df[1,], "_" = "_", df[-1,]))
    row.names(df)[-c(1,2)] <- levels(by)
  }

  # plot display
  if (plot.display) {
    if (is.null(by)) {
      plot(ikdplot(x = x, by = NULL, data = data, rnd = rnd, na.rm = na.rm,
                   main = main, xlab = xlab, ylab = ylab, legend.text = legend.text,
                   facet = facet, ...))
    } else {
      plot(ikdplot(x = x, by = by, data = data, rnd = rnd, na.rm = na.rm,
                   main = main, xlab = xlab, ylab = ylab, legend.text = legend.text,
                   facet = facet, ...))
    }
  }
  return(df)
}

#' @rdname inumsum
#' @export
inumsum.numeric <- function(x, rnd = 1, na.rm = TRUE) {
  # one variable
  len <- length(x)
  na <- length(x[is.na(x)])
  if(na > 0) na.rm <- TRUE
  mu <- mean(x, na.rm = na.rm)
  std <- sd(x, na.rm)
  q <- round(quantile(x, probs = c(0, .25, .5, .75, 1), na.rm = na.rm), rnd)
  v <- round(c(mu, std, q), rnd)
  df <- data.frame(Obs. = len, NA. = na, Mean = v[1], SD = v[2],
                   Median = v[5], Q1 = v[4], Q3 = v[6],
                   Min = v[3], Max = v[7],
                   row.names = "")
  return(df)
}
