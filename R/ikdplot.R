#' @title Kernel Density Plot, Grouped plotting using ggplot2
#'
#' @description
#' generate kernel density curves.
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
#' @param alpha set transparency of the density
#' @param show.legend show or hide the legend. Hide the legend by default
#' @param legend.text Legend title
#' @param facet.ncol number of columns to be faceted
#' @param plot.save a logical value. If TRUE, it saves the plot generated in the current working directory.
#' @param plot.name a text for plot filename. Suffix can be ".png", ".tiff" and ".pdf"
#' @param width a value in inches
#' @param height a value in inches
#' @param dpi a value for resolution of the plot saved.
#' @import ggplot2
#' @seealso ibarplot, iboxplot, inumsum, isum, itab
#' @keywords kernel density plot, kdensity, density
#' @examples
#' ikdplot(infert$age)
#' ikdplot(age, data = infert)
#' ikdplot(infert$age, infert$education)
#' infert$case <- factor(infert$case)
#' ikdplot(age, case, data = infert) # NA is included as one of the categories
#' ikdplot(age, case, data = infert, na.rm = TRUE)
#' ikdplot(age, pooled.stratum, data = infert)
#' ikdplot(age, pooled.stratum, education, data = infert)
#' str(iris)
#' ikdplot(Sepal.Length, data = iris)
#' ikdplot(Sepal.Length, Sepal.Width, data = iris)
#' ikdplot(Sepal.Length, Sepal.Width, Species, data = iris)

#' @export
ikdplot <- function(x, y = NULL, by = NULL, data = NULL, rnd = 1, na.rm = FALSE,
                    main = NULL, xlab = NULL, ylab = NULL, alpha = 0.1,
                    show.legend = TRUE, legend.text = NULL, facet.ncol = 2,
                    plot.save = FALSE, plot.name = 'ikdplot.tiff',
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

  # switch mechanisms
  gr.type <- ifelse(is.null(y), "uni", ifelse(is.null(by), "bi", "strata"))

  p <- switch(
    gr.type,
    uni = {
      if (is.null(main)) main <- paste0('Plot of ', lab.x)
      if (is.null(ylab)) ylab <- 'Density'
      if (is.null(xlab)) xlab <- lab.x

      data <- data.frame(x = x[!is.na(x)])
      sp.test <- shapiro.test(x)$p.value
      sp.test <- ifelse(sp.test < 0.001, "< 0.001", round(sp.test, 3))
      p <- ggplot2::ggplot(data, aes(x = x)) +
        geom_density(color = "white", fill = "light grey") +
        geom_rug(color = "red", size = .1) +
        geom_vline(aes(xintercept = mean(x, na.rm = TRUE)),
                   color = "blue", linetype = "dashed", size = 0.2) +
        scale_x_continuous(breaks = pretty(x, n = 5)) +
        labs(title = main,
             subtitle = paste0("Shapiro-Wilk Normality Test: ", sp.test),
             x = xlab, y = ylab) +
        theme_classic() +
        theme(panel.border = element_rect(linetype = "solid",
                                          colour = "black", fill = "NA"))
    },
    bi = {
      if (is.null(main)) main <- paste0('Plot of ', lab.x, ' ~ ', lab.y)
      data <- data.frame(x = x, y = y)
      if (na.rm) data <- na.omit(data)
      if (is.numeric(y)) {
        if (is.null(xlab)) xlab <- lab.x
        if (is.null(ylab)) ylab <- lab.y
        p <- ggplot2::ggplot(data = data, aes(x = x, y = y)) +
          geom_point(size = .5, shape = 1) +
          geom_smooth(method = lm, se = FALSE, size = .2) +
          labs(title = main,
               subtitle = paste0("Pearson's Correlation Coef: ",
                                 round(cor(x, y, use = "na.or.complete"), 3)),
               x = xlab, y = ylab) +
          theme_classic() + # remove panel background and gridlines
          theme(panel.border = element_rect(linetype = "solid",
                                            colour = "black", fill = "NA"))
      } else {
        if (is.null(ylab)) ylab <- 'Density'
        if (is.null(xlab)) xlab <- lab.x
        if (is.null(legend.text)) legend.text <- lab.y
        ncat <- ifelse(na.rm, nlevels(y),
                       ifelse(any(is.na(y)), nlevels(addNA(y)), nlevels(y)))
        p.value <- ifelse(ncat <= 2, t.test(x ~ y)$p.value,
                          summary(aov(x ~ y, data = data))[[1]][1,5])
        p.value <- ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
        if (!na.rm) y <- addNA(y)
        grp.mean <- aggregate(x = x, by = list(y = y), "mean")
        p <- ggplot2::ggplot(data, aes(x = x, color = y, fill = y)) +
          geom_density(alpha = alpha, show.legend = show.legend) +
          geom_rug(show.legend = show.legend) +
          geom_vline(data = grp.mean, aes(xintercept = x, color = y),
                     linetype = "dashed", size = 0.5, show.legend = show.legend) +
          guides(fill = FALSE) +
          scale_x_continuous(breaks = pretty(x, n = 5)) +
          labs(title = main,
               subtitle = paste0("p-value (", ifelse(ncat <= 2, "t-test",
                        "One-way ANOVA"), "): ", p.value),
               x = xlab, y = ylab, color = legend.text) +
          theme_classic() + # remove panel background and gridlines
          theme(panel.border = element_rect(linetype = "solid",
                                            colour = "black", fill = "NA"))
      }
    },
    strata = {
      if (is.null(main)) main <- paste0('Plot of ', lab.x, ' ~ ', lab.y,
                                        ', stratified by ', lab.by)
      data <- data.frame(x = x, y = y, by = by)
      if (na.rm) data <- na.omit(data)
      if (is.numeric(y)) {
        if (is.null(xlab)) xlab <- lab.x
        if (is.null(ylab)) ylab <- lab.y
        if (is.null(legend.text)) legend.text <- lab.by
        p <- ggplot2::ggplot(data = data, aes(x = x, y = y, color = by)) +
          geom_point() +
          labs(title = main, x = xlab, y = ylab, color = legend.text) +
          theme_classic() + # remove panel background and gridlines
          theme(panel.border = element_rect(linetype = "solid",
                                            colour = "black", fill = "NA"))
      } else {
        if (is.null(xlab)) xlab <- lab.x
        if (is.null(ylab)) ylab <- "Density"
        if (is.null(legend.text)) legend.text <- lab.y
        if (!na.rm) {
          y <- addNA(y)
          by <- addNA(by)
        }
        grp.mean <- aggregate(x = x, by = list(y = y, by = by), "mean")
        p <- ggplot2::ggplot(data, aes(x = x, color = y, fill = y)) +
          geom_density(alpha = alpha, show.legend = show.legend) +
          geom_rug(show.legend = show.legend) +
          facet_wrap(~ by, ncol = facet.ncol) +
          geom_vline(data = grp.mean, aes(xintercept = x, color = y, group = by),
                     linetype = "dashed", size = 0.5, show.legend = show.legend) +
          guides(fill = FALSE) +
          labs(title = main, x = xlab, y = ylab, color = legend.text) +
          theme_classic() + # remove panel background and gridlines
          theme(panel.border = element_rect(linetype = "solid",
                                            colour = "black", fill = "NA"))
      }
    })

  if (plot.save) {
    ggplot2::ggsave(plot.name, width = width, height = height, dpi = dpi)
    dev.off()
  }
  p
}
