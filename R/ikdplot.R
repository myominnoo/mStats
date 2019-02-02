#' @title Kernel Density Plot, Grouped plotting using ggplot2
#'
#' @description
#' ikdplot (I kdplot) lets you generate k density curve for one variable and curves grouped by a factor variable. These grouped curves can be either overlapped or presented in a faceted fashion.
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
#' @import ggplot2
#' @examples
#' ikdplot(infert$age)
#' ikdplot(age, data = infert)
#' ikdplot(infert$age, infert$education)
#' ikdplot(infert$age, infert$education, facet = TRUE)
#' ikdplot(infert$age, infert$education, facet = FALSE, legend.show = TRUE)
#' ikdplot(infert$age, infert$education, facet = TRUE, legend.show = FALSE)
#' # facet option
#' ikdplot(age, education, infert, facet = T)
#' ikdplot(age, education, infert, facet = F)
#' ikdplot(age, education, infert, alpha = 1)
#' # alpha option
#' ikdplot(age, education, infert, facet = F, alpha = .3)
#' ikdplot(age, education, infert, alpha = .5)

#' @export
ikdplot <- function(x, by = NULL, data = NULL, rnd = 1, na.rm = FALSE,
                    main = NULL, xlab = NULL, ylab = NULL, facet = TRUE,
                    legend.show = TRUE, legend.text = NULL, legend.position = "right",
                    line.color = "black", line.type = "solid", fill = "white", alpha = 0,
                    vline = TRUE, vline.color = "blue", vline.type = "dotted",
                    vline.size = 0.5, rug.position = "b", rug.color = "red",
                    save.plot = FALSE, plot.name = 'ikdplot.tiff',
                    width = 5, height = 4, dpi = 150)
{
  # data frame
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

  # missing value removed
  if (na.rm) {
    if (!is.null(by)) by <- by[!is.na(x)]
    x <- na.omit(x)
  }
  # plot title and labels
  if (is.null(main)) {
    main <- ifelse(is.null(by), paste0('Kernel Density Plot of ', lab.x),
                   paste0('Kernel Density Plot of ', lab.x, ' ~ ', lab.by))
  }
  if (is.null(xlab)) xlab <- lab.x
  if (is.null(ylab)) ylab <- 'Density'
  if (is.null(legend.text)) legend.text <- ifelse(is.null(by), xlab, lab.by)

  # plot display
  if (is.null(by)) {
    p <- ggplot2::ggplot(data, aes(x = x)) +
      geom_density(color = line.color, linetype = line.type, fill = fill, alpha = alpha) +
      geom_rug(sides = rug.position, color = rug.color)
    # mean line
    if (vline) {
      p <- p +
        geom_vline(aes(xintercept = mean(x)), color = vline.color,
                   linetype = vline.type, size = vline.size)
    }
  } else {
    p <- ggplot2::ggplot(data, aes(x = x, color = by, fill = by)) +
      geom_density(alpha = alpha, show.legend = legend.show) +
      geom_rug(sides = rug.position, show.legend = legend.show)
    if (facet) {
      p <- p + facet_grid(. ~ factor(by))
    } else {vline <- FALSE}
    if (vline) {
      grp.mean <- aggregate(x = x, by = list(by = by), "mean")
      p <- p +
        geom_vline(data = grp.mean, aes(xintercept = x, color = by),
                 linetype = vline.type, show.legend = legend.show)
    }
    p <- p + guides(fill = FALSE)
  }

  # Title and Labels
  p <- p +
    labs(title = main) +
    scale_color_discrete(legend.text) +
    xlab(label = xlab) +
    ylab(label = ylab) +
    theme_light()

  if (save.plot) {
    ggplot2::ggsave(plot.name, width = width, height = height, dpi = dpi)
    dev.off()
  }
  return(p)
}
