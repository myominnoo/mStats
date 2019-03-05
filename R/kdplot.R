#' @title Delightful Kernel Density Plots
#'
#' @description
#' \code{kdplot} generates optimized Kernel Density plots
#' for univariate, bivariate and stratified analyses.
#' @param x a factor object
#' @param y a factor object; if ignored, frequency distribution on x is generated.
#' @param by a factor object; if ignored, cross-tabulation on x and y is generated.
#' @param data an optional data frame
#' @param na.rm A logical value to specify missing values, <NA>:
#' @param main title of the plot
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param legend.text title of legend
#' @param alpha set transparency of the density
#' @param facet.ncol number of columns to be faceted
#' @param plot.save save the plot
#' @param plot.name specify for name of the plot file to be saved.
#' Filename can be in either of these extension formats: ".tiff", ".png", and ".pdf"
#' @param width integer measured in inches
#' @param height integer measured in inches
#' @param dpi specify resolution of the plot to be saved
#' @details
#' Exploring data before jumping into complex analysis is always a necessity.
#' The first step of an analysis should be to summarize and display data.
#' By doing so, invaluable insight can be gained through the familiarity with the data.
#'
#' @import ggplot2
#' @seealso \code{\link{isum}}, \code{\link{iboxplot}}, \code{\link{itab}}
#' @keywords kernel density plot, kdensity, density
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' data(infert)
#' str(infert)
#'
#' ## univariate analysis
#' kdplot(infert$age)
#' kdplot(age, data = infert, main = "MY Plot: Age", xlab = "Age in years",
#'        ylab = "Density Value")
#'
#' ## bivariate analysis
#' kdplot(infert$age, infert$education)
#' kdplot(age, education, data = infert, main = "My Plot", xlab = "Age",
#'        ylab = "Density Value", legend.text = "Education")
#'
#' kdplot(age, pooled.stratum, data = infert)
#' kdplot(age, pooled.stratum, data = infert, main = "My Plot", xlab = "Age",
#'        ylab = "Pooled Stratum")
#'
#' ## stratified analysis
#' kdplot(age, pooled.stratum, education, infert)
#' kdplot(age, pooled.stratum, education, infert, main = "My Plot", xlab = "Age",
#'        ylab = "Pooled Stratum", legend.text = "Education")
#'
#' infert$case <- factor(infert$case)
#'
#' ## iris data set
#' data(iris)
#' str(iris)
#' kdplot(age, education, case, data = infert)
#' kdplot(Sepal.Length, data = iris)
#' kdplot(Sepal.Length, Sepal.Width, data = iris)
#' kdplot(Sepal.Length, Sepal.Width, Species, data = iris)

#' @export
kdplot <- function(x, y = NULL, by = NULL, data = NULL, na.rm = TRUE,
                   main = NULL, xlab = NULL, ylab = NULL, legend.text = NULL,
                   alpha = 0.1, facet.ncol = 2,
                   plot.save = FALSE, plot.name = '<unnamed>.tiff',
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

  type <- ifelse(is.null(y), "uni", ifelse(is.null(by), "bi", "strata"))
  p <- switch(
    type,
    uni = {
      if (is.null(main)) main <- paste0('Plot of ', lab.x)
      if (is.null(ylab)) ylab <- 'Density'
      if (is.null(xlab)) xlab <- lab.x
      uni = uni.kdplt(x, main, xlab, ylab)
    },
    bi = {
      if (is.null(main)) main <- paste0('Plot: ', lab.x, ' ~ ', lab.y)
      if (is.null(xlab)) xlab <- lab.x

      if (is.factor(y) | is.character(y) | is.logical(y)) {
        if (is.null(ylab)) ylab <- "Density"
        if (is.null(legend.text)) legend.text <- lab.y
        bi = bi.kdplt.cat(x, y, na.rm, main, xlab, ylab, legend.text, alpha)
      } else if (is.numeric(y)) {
        if (is.null(ylab)) ylab <- lab.y
        bi = bi.boxplt.num(x, y, na.rm, main, xlab, ylab)
      } else {
        message('y is not specified as factor or as number.')
      }
    },
    strata = {
      if (is.null(main)) main <- paste0('Plot: ', lab.x, ' ~ ', lab.y, ' | ',
                                        lab.by)
      if (is.null(xlab)) xlab <- lab.x

      if (is.factor(y) | is.character(y) | is.logical(y)) {
        if (is.null(ylab)) ylab <- "Density"
        if (is.null(legend.text)) legend.text <- lab.y
        strata = str.kdplt.cat(x, y, by, na.rm, main, xlab, ylab, legend.text,
                               alpha, facet.ncol)
      } else if (is.numeric(y)) {
        if (is.null(ylab)) ylab <- lab.y
        if (is.null(legend.text)) legend.text <- lab.by
        strata = str.boxplt.num(x, y, by, na.rm, main, xlab, ylab, legend.text)
      } else {
        message('y is not specified as factor or as number.')
      }
    })
  if (plot.save) {
    plot(p)
    ggplot2::ggsave(plot.name, width = width, height = height, dpi = dpi)
    dev.off()
    cat(paste0("note: plot saved to \"", getwd(), "/", plot.name, "\"\n"))
  }
  return(p)
}

uni.kdplt <- function(x, main = 'Plot: ', xlab = 'x', ylab = 'Density')
{
  data <- data.frame(x = x[!is.na(x)])
  pvalue <- htest.pvalue(x, test = 'spw')
  p <- ggplot2::ggplot(data, aes(x = x)) +
    geom_density(color = "white", fill = "light grey") +
    geom_rug(color = "red", size = .1) +
    geom_vline(aes(xintercept = mean(x, na.rm = TRUE)),
               color = "blue", linetype = "dashed", size = 0.2) +
    scale_x_continuous(breaks = pretty(x, n = 5)) +
    labs(title = main, subtitle = paste0("Shapiro-Wilk Normality Test: ", pvalue),
         x = xlab, y = ylab) +
    theme_classic() +
    theme(panel.border = element_rect(linetype = "solid",
                                      colour = "black", fill = "NA"))
  return(p)
}

bi.kdplt.cat <- function(x, y, na.rm = TRUE, main = 'Plot: ', xlab = 'x', ylab = 'Density',
                         legend.text = 'y', alpha = 0.1)
{
  data <- data.frame(x = x, y = y)
  if (na.rm) data <- na.omit(data)
  pvalue <- htest.num(x, y, na.rm)[1]
  grp.mean <- aggregate(x = x, by = list(y = y), mean)
  p <- ggplot2::ggplot(data, aes(x = x, color = y, fill = y)) +
    geom_density(alpha = alpha) +
    geom_rug() +
    geom_vline(data = grp.mean, aes(xintercept = x, color = y),
               linetype = "dashed", size = 0.5) +
    guides(fill = FALSE) +
    scale_x_continuous(breaks = pretty(x, n = 5)) +
    labs(title = main, subtitle = pvalue, x = xlab, y = ylab, color = legend.text) +
    theme_classic() +
    theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = "NA"))
  return(p)
}

str.kdplt.cat <- function(x, y, by, na.rm = TRUE, main = 'Plot: ', xlab = 'x', ylab = 'y',
                          legend.text = 'by', alpha = 0.1, facet.ncol = 2)
{
  data <- data.frame(x = x, y = y, by = by)
  if (na.rm) data <- na.omit(data)
  grp.mean <- aggregate(x = x, by = list(y = y, by = by), mean)
  p <- ggplot2::ggplot(data, aes(x = x, color = y, fill = y)) +
    geom_density(alpha = alpha) +
    geom_rug() +
    facet_wrap(~ by, ncol = facet.ncol) +
    geom_vline(data = grp.mean, aes(xintercept = x, color = y, group = by),
               linetype = "dashed", size = 0.5) +
    guides(fill = FALSE) +
    labs(title = main, x = xlab, y = ylab, color = legend.text) +
    theme_classic() +
    theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = "NA"))
  return(p)
}
