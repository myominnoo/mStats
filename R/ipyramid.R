#' @title Population Pyramid Plot
#'
#' @description
#' \code{ipyramid} generates a population pyramid graph and data output.
#'
#' @param x a vector describing the bars which make up the plot. It is usually on x axis.
#' @param y a vector describing the y axis or second variable in cross-tabulation or data relationship.
#' @param data an optional data frame (or object coercible by as.data.frame to a data frame) containing the variables for contigency table.
#' @param na.rm A logical value indicating to remove NA values in the table or not. By Default, the value is TRUE
#' @param binwidth a number to specify the cut points of the continous data. If NULL, it is set to 10 by default. A sequence of number can be specified. See examples.
#' @param main a main title for the plot.
#' @param xlab a label for the x axis, defaults to a description of x.
#' @param ylab a label for the y axis, defaults to a description of y.
#' @param legend.text Legend title
#' @param plot.save a logical value. If TRUE, it saves the plot generated in the current working directory.
#' @param plot.name a text for plot filename. Suffix can be ".png", ".tiff" and ".pdf"
#' @param width a value in inches
#' @param height a value in inches
#' @param dpi a value for resolution of the plot saved.
#' @import ggplot2
#' @seealso \code{\link{isum}}, \code{\link{itab}}, \code{\link{inumsum}}, \code{\link{ibarplot}}
#' @keywords tabulation, frequency table, cross-tabulation, contigency table
#' @examples
#' # create random vectors
#' set.seed(100)
#' age <- sample(11:100, 1000, replace = TRUE)
#' sex <- factor(sample(c("male", "female"), 1000, replace = TRUE))
#' ipyramid(age, sex)
#' ipyramid(age, sex, binwidth = 20)
#' ipyramid(age, sex, binwidth = 20, getdata = "p") # only plot
#' ipyramid(age, sex, binwidth = 20, getdata = "tp") # plot with cross-tabulation
#' ipyramid(age, sex, binwidth = 20, getdata = "all") # plot + cross-tabulation + data
#' ipyramid(age, sex, binwidth = 10)
#' ipyramid(age, sex, binwidth = c(0, 20, 40, 60, 80, 100))
#' ipyramid(age, sex, binwidth = c(1, 15, 40, 60, 100))
#' data <- data.frame(age, sex)
#' ipyramid(age, sex, data = data)
#' ipyramid(age, sex, data = data, binwidth = 30)

#' @export
ipyramid <- function(x, y, data = NULL, na.rm = FALSE,
                     binwidth = NULL, main = NULL, xlab = NULL, ylab = NULL,
                     legend.text = NULL, getdata = 'tp',
                     plot.save = FALSE, plot.name = 'ipyramid.tiff',
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
  if (is.null(binwidth)) {
    binwidth <- 10
    x.brk <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), binwidth)
    x.brk <- c(x.brk[1], x.brk[2:(length(x.brk)-1)] - 1, x.brk[length(x.brk)])
  } else {
    if (length(binwidth) == 1) {
      x.brk <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), binwidth)
      x.brk <- c(x.brk[1], x.brk[2:(length(x.brk)-1)] - 1, x.brk[length(x.brk)])
    } else {
      x.brk <- binwidth
    }
  }

  if (x.brk[length(x.brk)] < max(x, na.rm = TRUE))
    x.brk <- c(x.brk, max(x, na.rm = TRUE))

  x.lbl.lwr <- c(x.brk[1], x.brk[-c(1, length(x.brk))] + 1)
  x.lbl.upr <- x.brk[-1]
  x.lbl <- paste(x.lbl.lwr, x.lbl.upr, sep = "-")

  data$x.cut <- cut(data$x, breaks = x.brk, labels = x.lbl, right = TRUE,
                    include.lowest = TRUE)

  y.cat <- as.character(unique(y))

  p.value <- suppressWarnings(chisq.test(with(data, table(x.cut, y)))$p.value)
  p.value <- ifelse(p.value < 0.001, "<0.001", round(p.value, 3))

  data$count <- 1
  df <- with(data, aggregate(count, by = list(x.cut = x.cut, y = y), sum))
  df$rx <- ifelse(df$y == y.cat[1], -df$x, df$x)
  y.min <- min(df$rx, na.rm = TRUE)
  y.max <- max(df$rx, na.rm = FALSE)
  y.brk <- pretty(seq(y.min, y.max))
  y.limits <- c(y.min + (y.min * 0.1), y.max + (y.max * 0.1))

  p <- ggplot2:: ggplot(df,  aes(x = x.cut, y = rx, fill = y)) +
    geom_col() +
    geom_hline(yintercept = 0, color = "white", size = 1) +
    scale_x_discrete(labels = x.lbl) +
    scale_y_continuous(breaks = y.brk, limits = y.limits, labels = abs) +
    geom_text(aes(label = x, hjust = ifelse(rx >= 0, -0.25, 1.25))) +
    labs(title = main, subtitle = paste0("Pearson's Chi-Square Test: ", p.value),
         x = xlab, y = ylab, fill = legend.text) +
    coord_flip() +
    theme_classic() +
    theme(panel.border = element_rect(linetype = "solid",
                                      colour = "black", fill = "NA"))

  res <- switch(getdata,
                p = p,
                tp = list(ContigencyTable = isum(x.cut, y, data = data), PyramidPlot = p),
                all = list(xvector = data$x.cut,
                           ContigencyTable = isum(x.cut, y, data = data), PyramidPlot = p))
  if (plot.save) {
    plot(p)
    ggplot2::ggsave(plot.name, width = width, height = height, dpi = dpi)
    dev.off()
    cat(paste0("\n... plot saved to \"", getwd(), "/", plot.name, "\" ...\n\n"))
  }
  return(res)
}
