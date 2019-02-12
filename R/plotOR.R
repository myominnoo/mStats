#' @title Odds Ratios Plot
#'
#' @description
#' produce an Odds Ratios Plot with 95% CI values.
#' @param odds a vector containing Odds Ratios (OR) values
#' @param cilow a vector containing lower levels of Odds Ratios (OR) values
#' @param cihigh a vector containing upper levels of Odds Ratios (OR) values
#' @param vars a vector with names of categories or sub-categories for OR
#' @param main a main title for the plot.
#' @param xlab a label for the x axis, defaults to a description of x.
#' @param ylab a label for the y axis, defaults to a description of y.
#' @param ci.barsize a double to change the size of the 95\% CI bars. 0.5 by default.
#' @param ci.barheight a double to change the height of the 95\% CI bars. 0.5 by default.
#' @param ci.barcolor a character or sequence of numeric vector to specify the color of the bar. "black" by default.
#' @param ci.linetype a character vector for specifying line type
#' @param vline.xintercept intercept value at x axis for vertical line
#' @param vline.size size of vertical line
#' @param vline.type type of vertical line
#' @param vline.color color of vertical line
#' @param or.shape shape of OR point estimate
#' @param or.size size of OR estimate: by default, OR x 2 (to magnify the point in display)
#' @param or.color color of OR estimate
#' @param xlimits specify two points to limit x axis. by default, it is c(0, max(cihigh))
#' @param text.size Text size for the axis labels
#' @param save.plot a logical value. If TRUE, it saves the plot generated in the current working directory.
#' @param plot.name a text for plot filename. Suffix can be ".png", ".tiff" and ".pdf"
#' @param width a value in inches
#' @param height a value in inches
#' @param dpi a value for resolution of the plot saved.
#' @import ggplot2
#' @seealso iboxplot, itab, inumsum, isum
#' @keywords OR, Odds Ratios Plot
#' @examples
#' str(infert)
#' or <- logistic.output("education", case, data = infert)
#' or
#' infert$induced <- factor(infert$induced)
#' infert$spontaneous <- factor(infert$spontaneous)
#' or <- logistic.output(c("education", "induced", "spontaneous"), case, data = infert)
#' plotOR(or[,1], or[,2], or[,3])
#' plotOR(or[,1], or[,2], or[,3], vars = rownames(or))
#' plotOR(or[,1], or[,2], or[,3], vars = rownames(or), main = "Odds Ratios Plot")
#' plotOR(or[,1], or[,2], or[,3], vars = rownames(or), xlab = "Adjusted OR")
#' plotOR(or[,1], or[,2], or[,3], vars = rownames(or), xlimits = c(0, 30))
#' plotOR(or[,1], or[,2], or[,3], vars = rownames(or), or.color = "black")
#' plotOR(or[,1], or[,2], or[,3], vars = rownames(or), or.size = 3, or.color = "black")
#' plotOR(or[,1], or[,2], or[,3], vars = rownames(or), or.size = 3, or.color = "black")
#' plotOR(or[,1], or[,2], or[,3], vars = rownames(or), or.size = 3, or.color = "black", save.plot = TRUE)

#' @export
plotOR <- function(odds, cilow, cihigh, vars = NULL,
                   main = NULL, xlab = NULL, ylab = NULL,
                   ci.barsize = 0.5, ci.barheight = 0.5, ci.barcolor = "black",
                   ci.linetype = "solid",
                   vline.xintercept = 1, vline.size = 0.25, vline.type = "dashed",
                   vline.color = "blue",
                   or.shape = 15, or.size = odds * 2, or.color = NULL,
                   xlimits = c(0, max(cihigh)), text.size = 16,
                   save.plot = FALSE, plot.name = 'plotOR.tiff',
                   width = 5, height = 4, dpi = 150)
{
  if (is.null(vars)) vars <- 1:length(odds)
  if (is.null(xlab)) xlab <- "Odds Ratios"
  if (is.null(or.color)) or.color <- 1:length(vars)

  data <- data.frame(odds = odds, cilow = cilow, cihigh = cihigh, vars = vars)
  p <- ggplot2::ggplot(data,  aes(x = odds, y = vars)) +
    geom_vline(aes(xintercept = vline.xintercept), size = vline.size,
               linetype = vline.type, color = vline.color) +
    geom_errorbarh(aes(xmax = cihigh, xmin = cilow), linetype = ci.linetype,
                   size = ci.barsize, height = ci.barheight, color = ci.barcolor) +
    geom_point(shape = or.shape, size = or.size, color = or.color) +
    scale_x_continuous(name = xlab, limits = xlimits) +
    labs(title = main, y = ylab) +
    theme_light() +
    theme(text = element_text(size = text.size),
          panel.border = element_rect(linetype = "solid",
                                      colour = "black", fill = "NA"))
  if (save.plot) {
    ggplot2::ggsave(plot.name, width = width, height = height, dpi = dpi)
    dev.off()
  }
  p
}
