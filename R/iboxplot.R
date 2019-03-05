#' @title Delightful Box plots and Scatter Plots
#'
#' @description
#' \code{iboxplot} generates optimized box plots and scatter plots 
#' for univariate, bivariate and stratified analyses.
#' @param x a factor object
#' @param y a factor object; if ignored, frequency distribution on x is generated. 
#' @param by a factor object; if ignored, cross-tabulation on x and y is generated.
#' @param data an optional data frame 
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#' @param na.rm A logical value to specify missing values, <NA>:
#' @param main title of the plot
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param legend.text title of legend
#' @param text.size size of text in the plot
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
#' \strong{Box plot}
#' 
#' Described by Tukey (1977), displays five figure summary of a distribution: 
#' the median, quartiles, minimum and maximum. In addition, any observations whose 
#' distance is more than 1.5 times the interquartile range are denoted as outliers 
#' and shown as red points. This type of plot is useful for showing the comparison 
#' of several groups. 
#' 
#' \strong{Scatter plot}
#' 
#' Scaling two continous variables along horizontal and vertical axis, this plot is 
#' known as scatter plot or diagram. Each pair of observations is displayed with a
#' hollow circle to better visualize the overlapping density. This can be further 
#' stratified in color by a third factor variable. For the association of two variables, a blue best fitted line is drawn by linear 
#' method. 
#' 
#' \strong{References:}
#' \enumerate{
#'   \item An Introduction to MEdical Statistics, Martin Bland, Thrid Edition, 
#'   Chapter 4, page 58 & 75-7
#' }
#' 
#' @import ggplot2
#' @seealso \code{\link{isum}}, \code{\link{itab}}, \code{\link{ibarplot}}
#' @keywords box plot, whisker plot, scatter plot, full stacked bar
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' data(infert)
#' str(infert)
#' 
#' ## univariate analysis
#' iboxplot(infert$age)
#' iboxplot(age, data = infert)
#' iboxplot(age, data = infert, main = "My Plot: Age", xlab = 'Age in years')
#' iboxplot(age, data = infert)
#' iboxplot(pooled.stratum, data = infert)
#' 
#' ## bivariate analysis
#' iboxplot(infert$age, infert$parity)
#' iboxplot(age, pooled.stratum, data = infert)
#' iboxplot(age, parity, data = infert, main = 'My plot: age x parity', 
#'          xlab = 'Age in years', ylab = 'Parity')
#'          
#' iboxplot(age, education, data = infert)
#' infert$case = factor(infert$case)
#' iboxplot(age, case, data = infert, main = 'My plot: age x case', 
#'          xlab = 'Case', ylab = 'Age in years')
#' 
#' ## stratified analysis 
#' iboxplot(infert$age, infert$parity, infert$education)
#' iboxplot(age, parity, education, data = infert, main = 'Stratified Plot', 
#'          xlab = 'Age', ylab = 'Parity', legend.text = 'Education')
#' 
#' iboxplot(age, education, case, data = infert)
#' iboxplot(age, case, education, data = infert, main = 'Stratified box plot', 
#'          xlab = 'Case', ylab = 'Age in years', legend.text = 'Education')

#' @export
iboxplot <- function(x, y = NULL, by = NULL, data = NULL, rnd = 1, na.rm = TRUE,
                     main = NULL, xlab = NULL, ylab = NULL,
                     legend.text = NULL, text.size = 12, 
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
      if (is.null(main)) main <- paste0('Plot: ', lab.x)
      if (is.null(xlab)) xlab <- lab.x
      uni.boxplt(x, main, xlab, text.size)
    }, 
    bi = {
      if (is.null(main)) main <- paste0('Plot: ', lab.x, ' ~ ', lab.y)
      if (is.null(xlab)) xlab <- lab.x
      if (is.null(ylab)) ylab <- lab.y
      
      if (is.factor(y) | is.character(y) | is.logical(y)) {
        bi = bi.boxplt.cat(x, y, na.rm, main, ylab, xlab, text.size)
      } else if (is.numeric(y)) {
        bi = bi.boxplt.num(x, y, na.rm, main, xlab, ylab, text.size)
      } else {
        message('y is not specified as factor or as number.')
      }
    }, 
    strata = {
      if (is.null(main)) main <- paste0('Plot: ', lab.x, ' ~ ', lab.y, ' | ', 
                                        lab.by)
      if (is.null(xlab)) xlab <- lab.x
      if (is.null(ylab)) ylab <- lab.y
      if (is.null(legend.text)) legend.text <- lab.by
      
      if (is.factor(y) | is.character(y) | is.logical(y)) {
        strata = str.boxplt.cat(x, y, by, na.rm, main, ylab, xlab, legend.text, 
                                text.size)
      } else if (is.numeric(y)) {
        strata = str.boxplt.num(x, y, by, na.rm, main, xlab, ylab, legend.text, 
                                text.size)
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

uni.boxplt <- function(x, main = 'Plot: ', xlab = 'x', text.size = 12) 
{
  data <- data.frame(x = x[!is.na(x)])
  pvalue <- htest.pvalue(x, test = 'spw')
  p <- ggplot2::ggplot(data = data, aes(y = x)) +
    stat_boxplot(geom = "errorbar") +
    geom_boxplot(outlier.colour = "red") +
    xlim(-2, 2) + 
    # scale_y_continuous(breaks = pretty(x, n = 10)) +
    labs(title = main, subtitle = paste0('p-value (Shapiro-Wilk test): ', pvalue), 
         y = xlab) +
    geom_hline(yintercept = mean(data$x), linetype = "dashed", 
               color = "grey", size = 0.2) + 
    theme_classic() +
    theme(text = element_text(size = text.size),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_rect(linetype = "solid",
                                      colour = "black", fill = "NA"))
  return(p)
}

bi.boxplt.num <- function(x, y, na.rm = TRUE, main = 'Plot: ', xlab = 'x', ylab = 'y', 
                          text.size = 12) 
{
  data <- data.frame(x = x, y = y)
  if (na.rm) data <- na.omit(data)
  pvalue <- htest.pvalue(x, y, test = 'corr')
  p <- ggplot2::ggplot(data = data, aes(x = x, y = y)) +
    geom_point(shape = 1) +
    geom_smooth(method = lm, se = FALSE, size = 0.2) +
    labs(title = main, subtitle = paste0('Correlation Coefficient: ', pvalue), 
         x = xlab, y = ylab) +
    theme_classic() +
    theme(text = element_text(size = text.size),
          panel.border = element_rect(linetype = "solid",
                                      colour = "black", fill = "NA"))
  return(p)
}

bi.boxplt.cat <- function(x, y, na.rm = TRUE, 
                          main = 'Plot: ', xlab = 'y', ylab = 'x', 
                          text.size = 12) 
{
  data <- data.frame(x = x, y = y)
  if (na.rm) data <- na.omit(data)
  pvalue <- htest.num(x, y, na.rm)[1]
  p <- ggplot2::ggplot(data = data, aes(x = y, y = x, fill = y)) +
    stat_boxplot(geom = "errorbar") +
    geom_boxplot(outlier.colour = "red", show.legend = FALSE) +
    labs(title = main, subtitle = pvalue, x = xlab, y = ylab) +
    theme_classic() +
    theme(text = element_text(size = text.size),
          panel.border = element_rect(linetype = "solid",
                                      colour = "black", fill = "NA"))
  return(p)
}

str.boxplt.num <- function(x, y, by, na.rm = TRUE, 
                           main = 'Plot: ', xlab = 'x', ylab = 'y', 
                           legend.text = 'by', text.size = 12)
{
  data <- data.frame(x = x, y = y, by = by)
  if (na.rm) data <- na.omit(data)
  p <- ggplot2::ggplot(data = data, aes(x = x, y = y, color = by)) +
    geom_point(shape = 1) +
    labs(title = main, x = xlab, y = ylab, color = legend.text) +
    theme_classic() +
    theme(text = element_text(size = text.size),
          panel.border = element_rect(linetype = "solid",
                                      colour = "black", fill = "NA"))
  return(p)
}

str.boxplt.cat <- function(x, y, by, na.rm = TRUE, 
                           main = 'Plot: ', xlab = 'y', ylab = 'x', 
                           legend.text = 'by', text.size = 12)
{
  data <- data.frame(x = x, y = y, by = by)
  if (na.rm) data <- na.omit(data)
  p <- ggplot2::ggplot(data = data, aes(x = y, y = x, fill = by)) +
    stat_boxplot(geom = "errorbar") +
    geom_boxplot(outlier.colour = "red") +
    labs(title = main, x = xlab, y = ylab, fill = legend.text)  +
    theme_classic() +
    theme(text = element_text(size = text.size),
          panel.border = element_rect(linetype = "solid",
                                      colour = "black", fill = "NA"))
  return(p)
}