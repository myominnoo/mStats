#' @title Population Pyramid Graph
#'
#' @description
#' generates a population pyramid graph and data output.
#'
#' @param x a vector describing the bars which make up the plot. It is usually on x axis.
#' @param y a vector describing the y axis or second variable in cross-tabulation or data relationship.
#' @param data an optional data frame (or object coercible by as.data.frame to a data frame) containing the variables for contigency table.
#' @param rnd an integer indicating the number of decimal places:
#' @param na.rm A logical value indicating to remove NA values in the table or not. By Default, the value is TRUE
#' @param binwidth a number to specify the cut points of the continous data. If NULL, it is set to 5 by default. A sequence of number can be specified. See examples.
#' @param main a main title for the plot.
#' @param xlab a label for the x axis, defaults to a description of x.
#' @param ylab a label for the y axis, defaults to a description of y.
#' @param show.legend show or hide the legend. Hide the legend by default
#' @param legend.text Legend title
#' @param save.plot a logical value. If TRUE, it saves the plot generated in the current working directory.
#' @param plot.name a text for plot filename. Suffix can be ".png", ".tiff" and ".pdf"
#' @param width a value in inches
#' @param height a value in inches
#' @param dpi a value for resolution of the plot saved.
#' @import ggplot2
#' @seealso itab, ibarplot, inumsum, ikdplot, iboxplot, isum
#' @keywords tabulation, frequency table, cross-tabulation, contigency table
#' @examples
#' set.seed(100)
#' age <- sample(1:100, 1000, replace = TRUE)
#' sex <- factor(sample(c("male", "female"), 1000, replace = TRUE))
#' library(ggplot2)
#' ipyramid(age, sex, binwidth = 20)
#' ipyramid(age, sex, binwidth = 20, getdata = "p")
#' ipyramid(age, sex, binwidth = 20, getdata = "tp")
#' ipyramid(age, sex, binwidth = 20, getdata = "all")
#' ipyramid(age, sex, binwidth = 10)
#' ipyramid(age, sex, binwidth = c(0, 20, 40, 60, 80, 100))
#' ipyramid(age, sex, binwidth = c(1, 15, 40, 60, 100))

#' @export
ipyramid <- function(x, y, data = NULL, rnd = 1, na.rm = FALSE,
                     binwidth = NULL, main = NULL, xlab = NULL, ylab = NULL,
                     legend.text = NULL, getdata = 'tp',
                     save.plot = FALSE, plot.name = 'ipyramid.tiff',
                     width = 5, height = 4, dpi = 150)
{
  if (!is.null(data)) {
    arguments <- as.list(match.call())
    x <- eval(substitute(x), data)
    y <- eval(substitute(y), data)

    lab.x <- arguments$x
    lab.y <- arguments$y

  } else {
    lab.x <- deparse(substitute(x))
    lab.y <- deparse(substitute(y))
  }

  main <- ifelse(is.null(main),
                 paste0("Population Pyramid of ", lab.x, " ~ ", lab.y), main)
  xlab <- ifelse(is.null(xlab), lab.x, xlab)
  ylab <- ifelse(is.null(ylab), "Count", ylab)
  legend.text <- ifelse(is.null(legend.text), lab.y, legend.text)

  data <- data.frame(x = x, y = y)
  data <- na.omit(data)

  if (is.null(binwidth)) {
    binwidth <- 5
    x.brk <- seq(min(x, na.rm = TRUE) - 1, max(x, na.rm = TRUE), binwidth)
  } else {
    if (length(binwidth) == 1) {
      x.brk <- seq(min(x, na.rm = TRUE) - 1, max(x, na.rm = TRUE), binwidth)
    } else {
      x.brk <- binwidth
    }
  }
  if (x.brk[length(x.brk)] != max(x, na.rm = TRUE))
    x.brk <- c(x.brk, max(x, na.rm = TRUE))
  x.lbl.upr <- x.brk[-1]
  x.lbl.lwr <- c(x.brk[1] + 1, x.brk[-c(1, length(x.brk))] + 1)
  data$x.cat <- cut(data$x,
                    breaks = x.brk,
                    labels = 1:length(x.lbl.upr),
                    include.lowest = TRUE)
  x.lbl <- paste(x.lbl.lwr, x.lbl.upr, sep = "-")
  y.cat <- as.character(unique(y))
  data$count <- 1
  df <- with(data, aggregate(count, by = list(x.cat = x.cat, y = y), sum))
  df$rx <- ifelse(df$y == y.cat[1], -df$x, df$x)

  y.min <- min(df$rx, na.rm = TRUE)
  y.max <- max(df$rx, na.rm = FALSE)
  y.brk <- pretty(seq(y.min, y.max))
  y.limits <- c(y.min + (y.min * 0.1), y.max + (y.max * 0.1))
  p <- ggplot2:: ggplot(df,  aes(x = x.cat, y = rx, fill = y)) +
    geom_col() +
    geom_hline(yintercept = 0, color = "white", size = 1) +
    coord_flip() +
    scale_x_discrete(labels = x.lbl) +
    scale_y_continuous(breaks = y.brk, limits = y.limits, labels = abs) +
    geom_text(aes(label = x, hjust = ifelse(rx >= 0, -0.25, 1.25))) +
    labs(title = main, x = xlab, y = ylab, fill = legend.text) +
    theme_classic() + # remove panel background and gridlines
    theme(panel.border = element_rect(linetype = "solid",
                                      colour = "black", fill = "NA"))
  x.cut <- factor(data$x.cat, levels = 1:length(x.lbl.upr), labels = x.lbl)
  res <- switch(getdata,
                p = p,
                tp = list(ContigencyTable = isum(x.cut, y), PyramindPlot = p),
                all = list(xvector = x.cut,
                           ContigencyTable = isum(x.cut, y), PyramindPlot = p))
  if (save.plot) {
    ggplot2::ggsave(plot.name, width = width, height = height, dpi = dpi)
    dev.off()
  }
  return(res)
}
