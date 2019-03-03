#' @title Number Summary and Plot using ggplot2
#'
#' @description
#' inumsum (I number-summarize) generates seven-number summary for one variable which include number of observations, missing records, mean, standard deviation, median, interquartile range, minimum, maximum. If the x variable is summarized by a factor variable, then the output will include seven-number summary plus standard error, 95% confidence intervals for means. In addition, the function generates automated box plots or Kernel density plot with optional parameters for graphical display.
#'
#' @param x a vector describing the bars which make up the plot. It is usually on x axis.
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
#' @keywords number summary, summary statistics, summary grouping
#' @examples
#' inumsum(x = age, data = infert)
#' inumsum(x = age, data = infert, boxplot = FALSE) # calling Kernel density plot
#' inumsum(infert$age)
#' inumsum(age, data = infert)
#' inumsum(x = age, y = education, data = infert)
#' infert$case <- factor(infert$case)
#' inumsum(case, education, data = infert)
#' inumsum(x = age, y = case, by = education, data = infert, na.rm = TRUE)
#' inumsum(x = age, y = case, by = education, data = infert, na.rm = TRUE, boxplot = FALSE) # calling Kernel density plot
#' inumsum(x = age, y = case, by = education, data = infert, na.rm = TRUE, boxplot = FALSE, facet.ncol = 3)
#' str(iris)
#' inumsum(Sepal.Length, data = iris)
#' inumsum(Sepal.Length, Species, data = iris)
#' inumsum(Sepal.Length, Species, data = iris, boxplot = FALSE)
#' inumsum(Sepal.Length, Sepal.Width, data = iris)
#' inumsum(Sepal.Length, Sepal.Width, data = iris, boxplot = FALSE)
#' inumsum(Sepal.Length, Sepal.Width, Species, data = iris)
#' inumsum(Sepal.Length, Sepal.Width, Species, data = iris, boxplot = F)

#' @export
inumsum <- function(x, y = NULL, by = NULL, data = NULL, rnd = 1, na.rm = FALSE,
                    x.varname = NULL, y.varname = NULL, by.varname = NULL,
                    plot.display = TRUE, boxplot = TRUE,
                    main = NULL, xlab = NULL, ylab = NULL, alpha = 0.1,
                    show.legend = TRUE, legend.text = NULL, facet.ncol = 2,
                    plot.save = FALSE, plot.name = 'inumsum.tiff',
                    width = 5, height = 4, dpi = 150)
{
  if (!is.null(data)) {
    arguments <- as.list(match.call())
    x <- eval(substitute(x), data)
    y <- eval(substitute(y), data)
    by <- eval(substitute(by), data)

    lab.x <- ifelse(is.null(x.varname), arguments$x, x.varname)
    lab.y <- ifelse(is.null(y.varname), arguments$y, y.varname)
    if (is.null(by)) {lab.by <- NULL} else {
      lab.by <- ifelse(is.null(by.varname), arguments$by, by.varname)
    }
  } else {
    lab.x <- ifelse(is.null(x.varname), deparse(substitute(x)), x.varname)
    if (!is.null(y)) lab.y <- ifelse(is.null(y.varname), deparse(substitute(y)), y.varname)
    if (!is.null(by)) lab.by <- ifelse(is.null(by.varname), deparse(substitute(by)), by.varname)
  }

  # levels of useNA in table() has c("no", "ifany", "always)
  include.na <- ifelse(na.rm, "no", "ifany")

  # switch mechanisms
  type <- ifelse(is.null(y), "uni", ifelse(is.null(by), "bi", "strata"))

  df <- switch(
    type,
    uni = inum(x, rnd, na.rm, lab.x),
    bi = {
      if (is.numeric(y)) {
        bi = inum.con(x, y, rnd, na.rm, lab.x, lab.y)
      } else {
        bi = inum.cat(x, y, rnd, na.rm, lab.x)
      }
      },
    strata = {
      if (is.numeric(y)) {
        by.levels <- as.character(unique(na.omit(by)))
        df <- lapply(by.levels,
                     function(z) inum.con(x[by == z], y[by == z], rnd, na.rm, lab.x, lab.y))
        if (any(is.na(by)) & !na.rm) {
          df[["NA"]] <- inum.con(x[is.na(by)], y[is.na(by)], rnd, na.rm, lab.x, lab.y)
          by.levels <- c(by.levels, "NA")
        }

        df <- structure(df, names = by.levels)
      } else {
        by.levels <- as.character(unique(na.omit(by)))
        df <- lapply(by.levels,
                     function(z) inum.cat(x[by == z], y[by == z], rnd, na.rm, lab.x))

        if (any(is.na(by)) & !na.rm) {
          df[["NA"]] <- inum.cat(x[is.na(by)], y[is.na(by)], rnd, na.rm, lab.x)
          by.levels <- c(by.levels, "NA")
        }

        df <- structure(df, names = by.levels)
      }
      })

  if (plot.display) {
    if (boxplot) {
      main <- ifelse(is.null(main),
              ifelse(is.null(y), paste0('Plot of ', lab.x),
              ifelse(is.null(by), paste0('Plot of ', lab.x, ' ~ ', lab.y),
                   paste0('Plot of ', lab.x, ' ~ ', lab.y, ', stratified by ', lab.by))),
                     main)
      if (!is.null(y)) {
        xlab <- ifelse(is.numeric(y),
                       ifelse(is.null(xlab), lab.x, xlab),
                       ifelse(is.null(xlab), lab.y, xlab))
        ylab <- ifelse(is.numeric(y),
                       ifelse(is.null(ylab), lab.y, ylab),
                       ifelse(is.null(ylab), lab.x, ylab))
        legend.text <- ifelse(is.null(legend.text),
                              ifelse(is.null(by), lab.y, lab.by), legend.text)
      } else {
        ylab <- ifelse(is.null(xlab), lab.x, xlab)
      }
      plot(iboxplot(x = x, y = y, by = by, rnd = rnd, na.rm = na.rm, main = main,
                    xlab = xlab, ylab = ylab, show.legend = show.legend,
                    legend.text = legend.text, plot.save = plot.save,
                    plot.name = plot.name, width = width, height = height, dpi = dpi))
    } else {
      main <- ifelse(is.null(main),
                     ifelse(is.null(y), paste0('Plot of ', lab.x),
                            ifelse(is.null(by), paste0('Plot of ', lab.x, ' ~ ', lab.y),
                                   paste0('Plot of ', lab.x, ' ~ ', lab.y, ', stratified by ',
                                          lab.by))), main)
      xlab <- ifelse(is.null(xlab), lab.x, xlab)
      ylab <- ifelse(is.null(ylab),
                     ifelse(is.numeric(y), lab.y, 'Density'), ylab)
      if (!is.null(y)) legend.text <- ifelse(is.null(legend.text), lab.y, legend.text)
      plot(ikdplot(x = x, y = y, by = by, rnd = rnd, na.rm = na.rm, main = main,
                   xlab = xlab, ylab = ylab, alpha = alpha, show.legend = show.legend,
                   legend.text = legend.text, facet.ncol = facet.ncol,
                   plot.save = plot.save, plot.name = plot.name, width = width,
                   height = height, dpi = dpi))
    }
  }
  df
}

#' @export
inum <- function(x, rnd = 1, na.rm = TRUE, rnames = "var1") {
  # one variable
  len <- ifelse(na.rm, length(x[!is.na(x)]), length(x))
  na <- ifelse(na.rm, 0, length(x[is.na(x)]))
  if (na > 0 & !na.rm) {
    # cat(paste0("\nNote: ", na, " records missing! descriptive statistics run as 'na.rm = TRUE' ... \n\n"))
    na.rm <- TRUE
  }
  mu <- mean(x, na.rm = na.rm)
  std <- sd(x, na.rm)
  q <- round(quantile(x, probs = c(0, .25, .5, .75, 1), na.rm = na.rm), rnd)
  v <- round(c(mu, std, q), rnd)
  df <- data.frame(Obs. = len, NA. = na, Mean = v[1], SD = v[2],
                   Median = v[5], Q1 = v[4], Q3 = v[6],
                   Min = v[3], Max = v[7],
                   row.names = rnames)
  return(df)
}

#' @export
inum.con <- function(x, y, rnd = 1, na.rm = TRUE, lab.x, lab.y) {
  df <- rbind(inum(x, rnd, na.rm, lab.x),
              inum(y, rnd, na.rm, lab.y))
  corr <- round(cor(x, y, use = "na.or.complete"), 3)
  df <- cbind(df, "Pearson's Corr" = c(corr, rep("", nrow(df) - 1)))
  return(df)
}

#' @export
inum.cat <- function(x, y, rnd = 1, na.rm = TRUE, rnames = "var1") {
  df <- do.call(rbind, lapply(levels(y),
                              function(i) inum(x[y == i], rnd, na.rm, i)))
  df <- do.call(rbind, list(inum(x, rnd, na.rm, rnames), df))
  df <- cbind(df, SE = round(df$SD / sqrt(df$Mean), rnd))
  df <- cbind(df,
              CI.Lwr = round(df$Mean - (1.96 * df$SE), rnd),
              CI.Upr = round(df[,3] + (1.96 * df[,5]), rnd))
  df <- do.call(rbind, list(df[1,], "_" = "_", df[-1,]))
  ncat <- ifelse(na.rm, nlevels(y),
                 ifelse(any(is.na(y)), nlevels(addNA(y)), nlevels(y)))
  stats <- ifelse(ncat <= 2, t.test(x ~ y)$statistic,
                  summary(aov(x ~ y))[[1]][1,4])
  p.value <- ifelse(ncat <= 2, t.test(x ~ y)$p.value,
                    summary(aov(x ~ y))[[1]][1,5])
  stats <- c(round(stats, rnd), rep("", nrow(df) - 1))
  p.value <- c(ifelse(p.value < 0.001, "<0.001", round(p.value, 3)),
               rep("", nrow(df) - 1))
  df <- cbind(df, stats = stats, p.value = p.value)
  names(df)[13] <- ifelse(ncat <= 2, "t value", "F value")
  return(df)
}
