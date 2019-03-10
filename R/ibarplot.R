#' @title Delightful Bar Plots 
#'
#' @description
#' \code{ibarplot} generates optimized bar plots for univariate, bivariate and 
#' stratified analyses.
#' @param x a factor object
#' @param y a factor object; if ignored, frequency distribution on x is generated. 
#' @param by a factor object; if ignored, cross-tabulation on x and y is generated.
#' @param data an optional data frame 
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#' @param na.rm A logical value to specify missing values, <NA>:
#' @param main title of the plot
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param text.size size of text in the plot. Font size for data level is 14:5 ratio 
#' to axis font size
#' @param label.pct show percentage for data label
#' @param flip.axis logical value to flip x-axis to y-axis
#' @param legend.show show or hide the legend 
#' @param legend.text title of legend
#' @param plot.type Only for bivariate analysis: four type of plot can be specified. 
#' \enumerate{
#'   \item \code{p}: parallel bar plot (default)
#'   \item \code{f}: \code{x} bar plot faceted by \code{y}
#'   \item \code{s}: stacked bar plot
#'   \item \code{fs}: full stacked or percentage bar plot
#' }
#' @param facet.ncol number of columns to be faceted in faceted plot
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
#' \strong{Bar Plot}
#' 
#' Frequency data is displayed in the form of horizontal or vertical bars. This type 
#' of plot is used to display single variable or the association between two or three 
#' categorical variables.
#' 
#' \strong{References:}
#' \enumerate{
#'   \item An Introduction to MEdical Statistics, Martin Bland, Thrid Edition, 
#'   Chapter 4, page 73
#' }
#' 
#' @import ggplot2
#' @import graphics
#' @seealso \code{\link{itab}}, \code{\link{isum}}, \code{\link{iboxplot}}
#' @keywords bar chart, bar plot, faceted plot, stacked bar, full stacked bar
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' data(infert)
#' 
#' ## univariate display
#' ibarplot(infert$education)
#' ibarplot(education, data = infert)
#' ibarplot(education, data = infert, main = "Plot of eduation", 
#'          xlab = "Education Level", ylab = "Number")
#' 
#' ibarplot(education, data = infert, label.pct = TRUE)
#' 
#' ## bivariate display
#' ibarplot(education, case, data = infert) # parallel bar chart
#' ibarplot(education, case, data = infert, plot.type = 'f') # facet plot
#' ibarplot(education, case, data = infert, plot.type = 's') # stacked plot
#' ibarplot(education, case, data = infert, plot.type = 'fs') # full stacked
#' 
#' ## stratified display
#' ibarplot(education, case, parity, infert)
#' ibarplot(education, parity, case, infert) 

#' @export
ibarplot <- function(x, y = NULL, by = NULL, data = NULL, rnd = 1, na.rm = FALSE,
                     main = NULL, xlab = NULL, ylab = NULL, 
                     text.size = 12, label.pct = FALSE, flip.axis = FALSE,
                     legend.show = TRUE, legend.text = NULL,
                     plot.type = "p", facet.ncol = 2,
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
    if (!is.null(y)) {lab.y <- deparse(substitute(y))}
    if (!is.null(by)) {lab.by <- deparse(substitute(by))}
  }
  
  if (na.rm) {include.na = "no"} else {include.na = "ifany"}
  type <- ifelse(is.null(y), "uni", ifelse(is.null(by), "bi", "strata"))
  
  p <- switch(
    type,
    uni = {
      if (is.null(main)) main <- paste0('Plot: ', lab.x)
      if (is.null(xlab)) xlab <- lab.x
      if (is.null(ylab)) ylab <- 'Frequency'
      uni <- uni.barplt(x, rnd, include.na, main, xlab, ylab, text.size, label.pct,
                        flip.axis)
    }, 
    bi = {
      if (is.null(main)) main <- paste0('Plot of ', lab.x, ' ~ ', lab.y)
      if (is.null(xlab)) xlab <- lab.x
      if (is.null(ylab))
        ylab <- ifelse(plot.type == "fs", 'Percentage (%)', 'Frequency')
      if (is.null(legend.text)) legend.text <- lab.y
      bi <- bi.barplt(x, y, rnd, include.na, main, xlab, ylab, text.size, label.pct, 
                      legend.show, legend.text, plot.type, facet.ncol)
    }, 
    strata = {
      if (is.null(main)) main <- paste0('Plot: ', lab.x, ' ~ ', lab.y, 
                                        ', stratified by ', lab.by)
      if (is.null(xlab)) xlab <- lab.x
      if (is.null(ylab)) ylab <- 'Frequency'
      if (is.null(legend.text)) legend.text <- lab.y
      strata <- str.barplt(x, y, by, rnd, include.na, main, xlab, ylab, text.size, 
                           label.pct, legend.show, legend.text, facet.ncol)
    })
  if (plot.save) {
    plot(p)
    ggplot2::ggsave(plot.name, width = width, height = height, dpi = dpi)
    dev.off()
    cat(paste0("note: plot saved to \"", getwd(), "/", plot.name, "\"\n"))
  }
  return(p)
}

uni.barplt <- function(x, rnd = 1, include.na = 'ifany', 
                       main = 'Frequency distribution', xlab = 'x', 
                       ylab = 'Frequency', text.size = 14, 
                       label.pct = FALSE, flip.axis = FALSE) 
{
  tbl <- table(x, useNA = include.na)
  data <- data.frame(tbl)
  data$pct <- round(data$Freq / sum(data$Freq) * 100, rnd)
  if (label.pct) lbl <- paste0(data$pct, '%') else lbl <- data$Freq
  
  p <- ggplot2::ggplot(data = data, aes(x = x, y = data$Freq, fill = x)) +
    geom_bar(stat = "identity", show.legend = FALSE) + 
    ylim(0, (max(data$Freq) + (max(data$Freq) * 0.1))) + 
    labs(title = main, x = xlab, y = ylab) +
    theme_classic() +
    theme(text = element_text(size = text.size),
          panel.border = element_rect(linetype = "solid", colour = "black", fill = "NA"))
  
  if (flip.axis) {
    p <- p + coord_flip()
    hjust = -0.25
    vjust = 0.5
  } else {
    hjust = 0.5
    vjust = -0.5
  }
  
  p <- p + 
    geom_text(aes(label = lbl, hjust = hjust, vjust = vjust), 
              size = text.size / (14/5), position = position_dodge(1)) 
  return(p)
}

bi.table <- function(x, y, rnd = 1, include.na = 'ifany') 
{
  tbl <- table(x, y, useNA = include.na)
  tblr <- round(tbl / rowSums(tbl) * 100, rnd) # row percentage
  tblc <- round(t(t(tbl) / colSums(tbl)) * 100, rnd) # column percentage
  data <- cbind(data.frame(tbl), rpct = data.frame(tblr)[,3],
                cpct = data.frame(tblc)[,3])
  data$lbl.rpct <- paste0(data$rpct, '%')
  data$lbl.cpct <- paste0(data$cpct, '%')
  return(data)
}

bi.barplt <- function(x, y, rnd = 1, include.na = 'ifany',
                      main = 'Plot:', xlab = 'x', ylab = 'Frequency', 
                      text.size = 14, label.pct = FALSE, 
                      legend.show = TRUE, legend.text = 'y',
                      plot.type = "f", facet.ncol = 2) 
{
  tbl <- table(x, y, useNA = include.na)
  data <- bi.table(x, y, rnd, include.na)
  if (label.pct) lbl <- paste0(data$rpct, '%') else lbl <- data$Freq
  
  if (any(tbl < 5)) 
    pvalue <- tryCatch({
      suppressWarnings(fisher.test(x, y, simulate.p.value = TRUE)$p.value)
    }, error = function(err) {
      return(NA)
    }) else 
      pvalue <- tryCatch({
        suppressWarnings(chisq.test(x, y, correct = FALSE)$p.value)
      }, error = function(err) {
        return(NA)
      })
  pvalue <- c(ifelse(pvalue < 0.001, "<0.001", round(pvalue, 3)),
              rep("", nrow(tbl) - 1))
  pvalue.name <- ifelse(any(tbl < 5), 'p-value (Fisher Exact Test): ', 
                        'p-value (Chi-Square Test): ')
  
  p <- ggplot2::ggplot(data = data)
  p <- switch(
    plot.type,
    f = {
      p + aes(x = x, y = data$Freq, fill = x) +
        geom_bar(stat = "identity", position = "dodge2", show.legend = FALSE) +
        ylim(0, (max(data$Freq) + (max(data$Freq) * 0.1))) +
        geom_text(aes(label = lbl, hjust = 0.5, vjust = -0.5), 
                  size = text.size / (14/5), position = position_dodge(1)) +
        facet_wrap(~ y, ncol = facet.ncol) +
        labs(title = main, subtitle = paste0(pvalue.name, pvalue),
             x = xlab, y = ylab) 
    }, 
    p = {
      p + aes(x = x, y = data$Freq, fill = y) +
        geom_bar(stat = "identity", position = "dodge2", 
                 show.legend = legend.show) +
        ylim(0, (max(data$Freq) + (max(data$Freq) * 0.1))) +
        geom_text(aes(label = lbl, hjust = 0.5, vjust = -0.5), 
                  size = text.size / (14/5), position = position_dodge(1)) +
        labs(title = main, subtitle = paste0(pvalue.name, pvalue),
             x = xlab, y = ylab, fill = legend.text) 
    },
    s = {
      p + aes(x = x, y = data$Freq, fill = y) +
        geom_bar(stat = "identity", position = "stack", show.legend = legend.show) +
        geom_text(aes(label = lbl), size = text.size / (14/5), 
                  position = position_stack(vjust = 0.5)) +
        labs(title = main, subtitle = paste0(pvalue.name, pvalue),
             x = xlab, y = ylab, fill = legend.text) 
    },
    fs = {
      p + aes(x = x, y = data$rpct, fill = y) +
        geom_bar(stat = "identity", position = "stack", show.legend = legend.show) +
        geom_text(aes(label = data$Freq), size = text.size / (14/5), 
                  position = position_stack(vjust = 0.5)) +
        labs(title = main, subtitle = paste0(pvalue.name, pvalue),
             x = xlab, y = ylab, fill = legend.text)
    })
  p <- p + 
    theme_classic() +
    theme(text = element_text(size = text.size),
          panel.border = element_rect(linetype = "solid", colour = "black", fill = "NA"))
  return(p)
}

str.barplt <- function(x, y, by, rnd = 1, include.na = 'ifany',
                       main = 'Plot:', xlab = 'x', ylab = 'Frequency', 
                       text.size = 14, label.pct = FALSE, 
                       legend.show = TRUE, legend.text = 'y', facet.ncol = 2)
{
  if (include.na == 'no') lvl <- unique(na.omit(by)) else lvl <- unique(by)
  data <- do.call(
    rbind,
    lapply(lvl, function(z) {
      df <- bi.table(x[by == z], y[by == z], rnd, include.na)
      df <- cbind(df, by = z)
    }))
  if (label.pct) lbl <- paste0(data$rpct, '%') else lbl <- data$Freq
  
  p <- ggplot2::ggplot(data = data)
  p <- p + aes(x = x, y = data$Freq, fill = y) +
    geom_bar(stat = "identity", position = "dodge", show.legend = legend.show) +
    ylim(0, (max(data$Freq) + (max(data$Freq) * 0.1))) +
    facet_wrap(~ by, ncol = facet.ncol) +
    geom_text(aes(label = lbl, hjust = 0.5, vjust = -0.5),
              size = text.size / (14/5), position = position_dodge(1)) +
    labs(title = main, x = xlab, y = ylab, fill = legend.text) +
    theme_classic() +
    theme(text = element_text(size = text.size),
          panel.border = element_rect(linetype = "solid", colour = "black", fill = "NA"))
  return(p)
}
