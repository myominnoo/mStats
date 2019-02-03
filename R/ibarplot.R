#' @title Create and Save beautiful Bar Plot using ggplot2
#'
#' @description
#' ibarplot is a function that generate a barplot with several options for optimal data visualization of factor variables using ggplot2 package. If one factor is provided as input, a simple barplot will be generated. If two factors, it produces a faceted barplot by default. Grouped bar plot can also be specified. Several parameters including plot title, labels of x and y axes and saving plot can be set by the user.
#'
#' @param x a vector describing the bars which make up the plot. It is usually on x axis.
#' @param by a vector describing the grouping of x. By default, the plot generates a faceted barplot.
#' @param data an optional data frame (or object coercible by as.data.frame to a data frame) containing the variables for contigency table.
#' @param rnd an integer indicating the number of decimal places:
#' @param na.rm A logical value indicating to remove NA values in the table or not. By Default, the value is TRUE
#' @param main a main title for the plot.
#' @param xlab a label for the x axis, defaults to a description of x.
#' @param ylab a label for the y axis, defaults to a description of y.
#' @param stack indicate for stack bar chart in bivariate analysis
#' @param stack.pct indicate stacked percent bar chart
#' @param stack.vjust number to place value label in stacked bar chart
#' @param legend.text a text for the title of the plot
#' @param facet a logical value. If TRUE, the plot forms a matrix of panels defined by column faceting variables. It is most useful when you have two discrete variables, and all combinations of the variables exist in the data. By default, it is set to TRUE.
#' @param facet.ncol number of columns to be faceted
#' @param facet.strip.pos indicate position of facet strip: "top" by default: others include "bottom", "left", "right".
#' @param legend.show show or hide the legend. Hide the legend by default
#' @param legend.text Legend title
#' @param legend.position the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param horizontal logical value, indicating the direction of bar: horizontal by default and vertical.
#' @param flip.levels if there are more than k categories, bar chart will be automatically flip to horizontal direction in order to generate a better visual. This is the same as setting horizontal to TRUE.
#' @param bar.color a text to depict the outer line of bars. You can set "black", "green", "blue" and so on.
#' @param dvsize font size of value label in stacked bar chart.
#' @param vjust a number to place the value label in percentage to the bar.
#' @param hjust a number to place the value label in percentage to the bar.
#' @param position.dodge shrink width of bars.
#' @param save.plot a logical value. If TRUE, it saves the plot generated in the current working directory.
#' @param plot.name a text for plot filename. Suffix can be ".png", ".tiff" and ".pdf"
#' @param width a value in inches
#' @param height a value in inches
#' @param dpi a value for resolution of the plot saved.
#'
#' @import ggplot2
#'
#' @seealso itab, inumsum, isum
#' @examples
#' ibarplot(x = education, data = infert)
#' ibarplot(x = education, data = infert, legend.show = FALSE)
#' ibarplot(infert$education, legend.show = TRUE)
#' ibarplot(x = education, by = case, data = infert)
#' ibarplot(x = education, by = case, data = infert, facet = FALSE)
#' ibarplot(induced, case, infert)

#' @export
ibarplot <- function(x, by = NULL, data = NULL, rnd = 1, na.rm = FALSE,
                     main = NULL, xlab = NULL, ylab = NULL,
                     stack = FALSE, stack.pct = TRUE, stack.vjust = 0.5,
                     facet = TRUE, facet.ncol = 3, facet.strip.pos = "top",
                     legend.show = FALSE, legend.text = NULL, legend.position = "right",
                     horizontal = FALSE, flip.levels = 10, bar.color = "white",
                     dv.size = 3.5, vjust = -0.5, hjust = 0.5, position.dodge = 1,
                     save.plot = FALSE, plot.name = 'ibarplot.tiff',
                     width = 5, height = 4, dpi = 150)
{
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

  # title and legends - labels
  if (is.null(main)) {
    if (is.null(by)) {main <- paste0('Plot of ', lab.x)} else {
      main <- paste0('Plot of ', lab.x, ' ~ ', lab.by)
    }
  }
  if (is.null(xlab)) xlab <- lab.x
  if (is.null(ylab)) ylab <- 'Freq'
  if (is.null(legend.text))
    legend.text <- ifelse(stack, lab.by, ifelse(facet, xlab, lab.by))

  # levels of useNA in table() has c("no", "ifany", "always)
  if (na.rm) {include.na = "no"} else {include.na = "ifany"}

  if (is.null(by)) {
    # univariate
    tbl <- table(x, useNA = include.na)
    data <- data.frame(tbl)
    data$pct <- round(data$Freq / sum(data$Freq) * 100, rnd)
    p <- ggplot2::ggplot(data = data, aes(x = x, y = Freq, fill = x)) +
      geom_bar(stat = "identity", color = bar.color, show.legend = legend.show)
    if (horizontal | length(levels(x)) > flip.levels) p <- p + coord_flip()
    p <- p +
      geom_text(aes(label = paste0(pct, '%'), hjust = hjust, vjust=vjust),
                position = position_dodge(position.dodge)) +
      ylim(0, (max(data$Freq) + (max(data$Freq) * 0.1))) +
      labs(title = main, x = xlab, y = ylab) +
      guides(fill = guide_legend(title = legend.text)) +
      theme_light()
  } else {
    # bivariate
    tbl <- table(x, by, useNA = include.na)
    data <- data.frame(tbl)
    data$pct <- round(data$Freq / sum(data$Freq) * 100, rnd)
    data <- transform(data, by.pct = ave(Freq, by,
                                         FUN = function(x) round(x/sum(x), rnd) * 100))
    data <- transform(data, x.pct = ave(Freq, x,
                                        FUN = function(x) round(x/sum(x), rnd) * 100))
    type <- ifelse(stack, ifelse(stack.pct, "stack.full", "stack"),
                   ifelse(facet, "facet", "parallel"))
    p <- ggplot2::ggplot(data = data)
    p <- switch(type,
                stack = p + aes(x = x, y = Freq, fill = by) +
                  geom_bar(stat = "identity") +
                  geom_text(aes(label = paste0(x.pct, '%')), size = dv.size,
                            position = position_stack(vjust = stack.vjust)),
                stack.full = p + aes(x = x, y = x.pct, fill = by) +
                  geom_bar(stat = "identity"),
                facet = p + aes(x = x, y = Freq, fill = x) +
                  facet_wrap(~ by, ncol = facet.ncol, strip.position = facet.strip.pos) +
                  geom_bar(stat="identity", position = "dodge2", color = bar.color,
                           show.legend = legend.show) +
                  geom_text(aes(label = paste0(by.pct, '%'), hjust = hjust, vjust=vjust),
                            position = position_dodge(position.dodge)) +
                  ylim(0, (max(data$Freq) + (max(data$Freq) * 0.1))),
                parallel = p + aes(x = x, y = Freq, fill = by) +
                  geom_bar(stat="identity", position = "dodge2", color = bar.color,
                           show.legend = TRUE) +
                  geom_text(aes(label = paste0(by.pct, '%'), hjust = hjust, vjust=vjust),
                            position = position_dodge(position.dodge)) +
                  ylim(0, (max(data$Freq) + (max(data$Freq) * 0.1)))
    )
    p <- p +
      labs(title = main, x = xlab, y = ylab) +
      guides(fill = guide_legend(title = legend.text)) +
      theme_light()
  }
  if (save.plot) {
    ggplot2::ggsave(plot.name, width = width, height = height, dpi = dpi)
    dev.off()
  }
  return(p)
}


