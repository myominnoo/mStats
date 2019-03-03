#' @title Plotting Odds Ratios or Hazard Ratios
#'
#' @description
#' \code{\link{plotOR}} produce an Odds Ratios Plot with 95% CI values.
#'
#' @param odds a vector containing Odds Ratios (OR) values
#' @param cilow a vector containing lower levels of Odds Ratios (OR) values
#' @param cihigh a vector containing upper levels of Odds Ratios (OR) values
#' @param vars a vector with names of categories or sub-categories for OR
#' @param log produce odds ratios and 95\% confidence intervals on log scale
#' @param main a main title for the plot.
#' @param xlab a label for the x axis, defaults to a description of x.
#' @param ylab a label for the y axis, defaults to a description of y.
#' @param pe.shape shape of OR point estimate
#' @param pe.size size of OR estimate: by default, OR x 2 (to magnify the point in display)
#' @param pe.color color of OR estimate
#' @param ci.barsize a double to change the size of the 95\% CI bars. 0.5 by default.
#' @param ci.barheight a double to change the height of the 95\% CI bars. 0.5 by default.
#' @param ci.barcolor a character or sequence of numeric vector to specify the color of the bar. "black" by default.
#' @param ci.linetype a character vector for specifying line type
#' @param vline.xintercept intercept value at x axis for vertical line
#' @param vline.size size of vertical line
#' @param vline.type type of vertical line
#' @param vline.color color of vertical line
#' @param xlimits specify two points to limit x axis. by default, it is c(0, max(cihigh))
#' @param text.size Text size for the axis labels
#' @param plot.save a logical value. If TRUE, it saves the plot generated in the current working directory.
#' @param plot.name a text for plot filename. Suffix can be ".png", ".tiff" and ".pdf"
#' @param width a value in inches
#' @param height a value in inches
#' @param dpi a value for resolution of the plot saved.
#' @import ggplot2
#' @seealso \code{\link{isum}}, \code{\link{itab}}, \code{\link{inumsum}}, \code{\link{ibarplot}}, \code{\link{iboxplot}}, \code{\link{ikdplot}}
#' @keywords OR, Odds Ratios Plot
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' str(infert)
#' or <- logistic.output("education", case, data = infert)
#' or
#' infert$induced <- factor(infert$induced)
#' infert$spontaneous <- factor(infert$spontaneous)
#' or <- logistic.output(c("education", "induced", "spontaneous"), case,
#' data = infert, print.to.console = TRUE)
#' library(ggplot2)
#' plotOR(or[,1], or[,2], or[,3])
#' plotOR(or[,1], or[,2], or[,3], log = TRUE)
#' plotOR(or[,1], or[,2], or[,3], vars = rownames(or))
#' plotOR(or[,1], or[,2], or[,3], vars = rownames(or), log = TRUE)
#'
#' plotOR(or[,1], or[,2], or[,3], vars = rownames(or), log = TRUE,
#'     pe.size = 5, pe.color = "orange")
#'
#' plotOR(or[,1], or[,2], or[,3], vars = rownames(or), main = "Odds Ratios Plot")
#' plotOR(or[,1], or[,2], or[,3], vars = rownames(or), xlab = "Adjusted OR")
#' plotOR(or[,1], or[,2], or[,3], vars = rownames(or), xlimits = c(0, 30))
#' plotOR(or[,1], or[,2], or[,3], vars = rownames(or), pe.color = "orange")
#' plotOR(or[,1], or[,2], or[,3], vars = rownames(or), pe.size = 8,
#'     pe.color = "green")
#'
#' plotOR(or[,1], or[,2], or[,3], vars = rownames(or), pe.size = 1,
#'     pe.color = "blue")
#'
#' # increase font size of axis labels
#' plotOR(or[,1], or[,2], or[,3], vars = rownames(or), pe.size = 1, pe.color = "blue",
#' text.size = 16)
#'
#' plotOR(or[,1], or[,2], or[,3], vars = rownames(or),
#' log = TRUE, pe.size = 6, pe.color = "green", ci.barcolor = "orange")
#'
#' plotOR(or[,1], or[,2], or[,3], vars = rownames(or), log = TRUE,
#'     pe.size = 6, pe.color = "green", ci.barcolor = "orange", xlimits = c(-3, 4))
#'
#' plotOR(or[,1], or[,2], or[,3], vars = rownames(or), pe.size = 6,
#'     pe.color = "green", ci.barcolor = "orange", plot.save = TRUE)

#' @export
plotOR <- function(odds, cilow, cihigh, vars = NULL, log = FALSE, rnd = 1,
                   main = NULL, xlab = NULL, ylab = NULL,
                   pe.shape = 15, pe.size = 3, pe.color = NULL,
                   ci.barsize = 0.25, ci.barheight = 0.25, ci.barcolor = "red",
                   ci.linetype = "solid",
                   vline.xintercept = 1, vline.size = 0.25, vline.type = "dashed",
                   vline.color = "blue",
                   xlimits = NULL, text.size = 12,
                   plot.save = FALSE, plot.name = 'plotOR.tiff',
                   width = 5, height = 4, dpi = 150)
{
  if (is.null(vars)) vars <- 1:length(odds)
  xlab <- ifelse(is.null(xlab),
                 ifelse(log, "Odds Ratios on LOG SCALE", "Odds Ratios"), xlab)
  if (is.null(pe.color)) pe.color <- "black"

  data <- data.frame(odds = odds, cilow = cilow, cihigh = cihigh, vars = vars)

  p <- ggplot2::ggplot(data,  aes(x = odds, y = 1:length(odds))) +
    geom_vline(aes(xintercept = vline.xintercept), size = vline.size,
               linetype = vline.type, color = vline.color) +
    geom_errorbarh(aes(xmax = cihigh, xmin = cilow), linetype = ci.linetype,
                   size = ci.barsize, height = ci.barheight, color = ci.barcolor) +
    geom_point(shape = pe.shape, size = pe.size, color = pe.color)
  if (log) {
    p <- p + scale_x_continuous(trans = "log2", name = xlab, limits = xlimits)
  } else {
    p <- p + scale_x_continuous(name = xlab, limits = xlimits)
  }
  p <- p +
    labs(title = main) +
    scale_y_continuous(breaks = 1:length(odds),
                       labels = vars,
                       name = ylab,
                       sec.axis = dup_axis(~.,
                             breaks = 1:length(odds),
                             labels = paste0(sprintf("%.1f", round(odds, rnd)),
                                        " (", sprintf("%.1f", round(cilow, rnd)),
                                        "-", sprintf("%.1f", round(cihigh, rnd)),
                                                           ")"),
                                           name = "")) +
    theme_light() +
    theme(text = element_text(size = text.size),
          panel.border = element_rect(linetype = "solid",
                                      colour = "black", fill = "NA"))

  if (plot.save) {
    plot(p)
    ggplot2::ggsave(plot.name, width = width, height = height, dpi = dpi)
    dev.off()
    cat(paste0("\n... plot saved to \"", getwd(), "/", plot.name, "\" ...\n\n"))
  }
  p
}
