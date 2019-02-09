#' @title Create and Save beautiful Bar Plot using ggplot2
#'
#' @description
#' ibarplot is a function that generate a barplot with several options for optimal data visualization of factor variables using ggplot2 package. If one factor is provided as input, a simple barplot will be generated. If two factors, it produces a faceted barplot by default. Grouped bar plot can also be specified. Several parameters including plot title, labels of x and y axes and saving plot can be set by the user.
#'
#' @param x a vector describing the bars which make up the plot. It is usually on x axis.
#' @param y a vector describing the y axis or second variable in cross-tabulation or data relationship.
#' @param by a vector describing the grouping of x. By default, the plot generates a faceted barplot.
#' @param data an optional data frame (or object coercible by as.data.frame to a data frame) containing the variables for contigency table.
#' @param rnd an integer indicating the number of decimal places:
#' @param na.rm A logical value indicating to remove NA values in the table or not. By Default, the value is TRUE
#' @param main a main title for the plot.
#' @param xlab a label for the x axis, defaults to a description of x.
#' @param ylab a label for the y axis, defaults to a description of y.
#' @param show.legend show or hide the legend. Hide the legend by default
#' @param legend.text Legend title
#' @param plot.type if bivariate analysis, type of plot can be specified. By default, this generates a facted plot. 'p' for parrallel barplot, 's' for stacked barplot, 'fs' for full stacked percentage barplot.
#' @param facet.ncol number of columns to be faceted
#' @param save.plot a logical value. If TRUE, it saves the plot generated in the current working directory.
#' @param plot.name a text for plot filename. Suffix can be ".png", ".tiff" and ".pdf"
#' @param width a value in inches
#' @param height a value in inches
#' @param dpi a value for resolution of the plot saved.
#' @import ggplot2
#' @seealso iboxplot, itab, inumsum, isum
#' @keywords barplot, cross-tabulation, faceted plot
#' @examples
#' ibarplot(infert$education)
#' ibarplot(x = education, data = infert)
#' ibarplot(x = education, y = case, data = infert)
#' ibarplot(x = education, y = case, data = infert, plot.type = 'f')
#' ibarplot(x = education, y = case, data = infert, plot.type = 'p')
#' ibarplot(x = education, y = case, data = infert, plot.type = 's')
#' ibarplot(x = education, y = case, data = infert, plot.type = 'fs')
#' education <- factor(rep(c("None", "Primary", "Secondary", "graduate"), c(50, 100, 25, 25)))
#' sex <- factor(rep(c("M", "F", NA), c(100, 75, 25)))
#' marital <- factor(rep(c("Single", "married", "widowed", NA), c(25, 100, 70, 5)))
#' ibarplot(education)
#' ibarplot(sex)
#' ibarplot(marital)
#' ibarplot(sex, marital)
#' ibarplot(sex, marital, na.rm = TRUE)
#' ibarplot(sex, marital, education)

#' @export
ibarplot <- function(x, y = NULL, by = NULL, data = NULL, rnd = 1, na.rm = FALSE,
                     main = NULL, xlab = NULL, ylab = NULL,
                     show.legend = TRUE, legend.text = NULL,
                     plot.type = "f", facet.ncol = 2,
                     save.plot = FALSE, plot.name = 'ibarplot.tiff',
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

  # levels of useNA in table() has c("no", "ifany", "always)
  if (na.rm) {include.na = "no"} else {include.na = "ifany"}

  # switch mechanisms
  gr.type <- ifelse(is.null(y), "uni", ifelse(is.null(by), "bi", "strata"))
  p <- switch(gr.type,
              uni = {
                if (is.null(main)) main <- paste0('Plot of ', lab.x)
                if (is.null(xlab)) xlab <- lab.x
                if (is.null(ylab)) ylab <- 'Number'
                # univariate
                tbl <- table(x, useNA = include.na)
                data <- data.frame(tbl)
                data$pct <- round(data$Freq / sum(data$Freq) * 100, rnd)
                p <- ggplot2::ggplot(data = data, aes(x = x, y = Freq, fill = x)) +
                  geom_bar(stat = "identity", show.legend = FALSE) +
                  geom_text(aes(label = paste0(pct, '%'),
                                hjust = 0.5, vjust = -0.5),
                            position = position_dodge(1)) +
                  ylim(0, (max(data$Freq) + (max(data$Freq) * 0.1))) +
                  labs(title = main, x = xlab, y = ylab) +
                  theme_light()
              },
              bi = {
                # labels
                if (is.null(main)) main <- paste0('Plot of ', lab.x, ' ~ ', lab.y)
                if (!is.null(xlab)) lab.x <- xlab
                if (is.null(ylab))
                  ylab <- ifelse(plot.type == "fs", 'Percentage (%)', 'Number')
                if (is.null(legend.text)) legend.text <- lab.y
                tbl <- table(x, y, useNA = include.na)
                tblr <- round(tbl / rowSums(tbl) * 100, rnd) # row percentage
                tblc <- round(t(t(tbl) / colSums(tbl)) * 100, rnd) # column percentage
                data <- cbind(data.frame(tbl), rpct = data.frame(tblr)[,3],
                              cpct = data.frame(tblc)[,3])
                colnames(data)[1:2] <- c("x", "y")
                p <- ggplot2::ggplot(data = data)
                p <- switch(
                  plot.type,
                  f = {
                    p + aes(x = x, y = Freq, fill = x) +
                      geom_bar(stat = "identity", position = "dodge2", show.legend = FALSE) +
                      ylim(0, (max(data$Freq) + (max(data$Freq) * 0.1))) +
                      geom_text(aes(label = paste0(rpct, '%'), hjust = 0.5, vjust = -0.5),
                                position = position_dodge(1)) +
                      facet_wrap(~ y, ncol = facet.ncol) +
                      labs(title = main, x = lab.x, y = ylab) +
                      theme_light()
                  },
                  p = {
                    p + aes(x = x, y = Freq, fill = y) +
                      geom_bar(stat = "identity", position = "dodge2", show.legend = show.legend) +
                      ylim(0, (max(data$Freq) + (max(data$Freq) * 0.1))) +
                      geom_text(aes(label = paste0(rpct, '%'), hjust = 0.5, vjust = -0.5),
                                position = position_dodge(1)) +
                      labs(title = main, x = lab.x, y = ylab, fill = legend.text) +
                      theme_light()
                  },
                  s = {
                    p + aes(x = x, y = Freq, fill = y) +
                      geom_bar(stat = "identity", position = "stack", show.legend = show.legend) +
                      geom_text(aes(label = paste0(rpct, '%')),
                                position = position_stack(vjust = 0.5)) +
                      labs(title = main, x = lab.x, y = ylab, fill = legend.text) +
                      theme_light()
                  },
                  fs = {
                    p + aes(x = x, y = rpct, fill = y) +
                      geom_bar(stat = "identity", position = "stack", show.legend = show.legend) +
                      geom_text(aes(label = Freq), position = position_stack(vjust = 0.5)) +
                      labs(title = main, x = lab.x, y = ylab, fill = legend.text) +
                      theme_light()})
              },
              strata = {
                # labels
                if (!is.null(xlab)) lab.x <- xlab
                if (is.null(ylab))  ylab <- 'Number'
                if (is.null(main)) main <- paste0('Plot of ', lab.x, ' ~ ', lab.y,
                                                  ', stratified by ', lab.by)
                if (is.null(legend.text)) legend.text <- lab.y
                by.cat <- levels(by)
                tab.freq <- function(tbl, by.cat) {
                  data <- data.frame(tbl)
                  tblr <- round(tbl / rowSums(tbl) * 100, rnd) # row percentage
                  tblc <- round(t(t(tbl) / colSums(tbl)) * 100, rnd) # column percentage
                  data <- cbind(data.frame(tbl), rpct = data.frame(tblr)[,3],
                                cpct = data.frame(tblc)[,3])
                  data <- cbind(data, by = rep(by.cat, nrow(data)))
                  colnames(data)[1:2] <- c("x", "y")
                  data
                }
                data <- do.call(
                  rbind,
                  lapply(by.cat, function(z) tab.freq(table(x[by == z], y[by == z]), z)))
                if (!na.rm) {
                  data <- rbind(data, tab.freq(table(x[is.na(by)], y[is.na(by)]), "<NA>"))
                }

                p <- ggplot2::ggplot(data = data)
                p + aes(x = x, y = Freq, fill = y) +
                  geom_bar(stat = "identity", position = "dodge", show.legend = show.legend) +
                  ylim(0, (max(data$Freq) + (max(data$Freq) * 0.1))) +
                  facet_wrap(~ by) +
                  geom_text(aes(label = paste0(rpct, '%'), hjust = 0.5, vjust = -0.5),
                            position = position_dodge(1)) +
                  labs(title = main, x = lab.x, y = ylab, fill = legend.text) +
                  theme_light()
              }
  )
  if (save.plot) {
    ggplot2::ggsave(plot.name, width = width, height = height, dpi = dpi)
    dev.off()
  }
  p
}
