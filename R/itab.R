#' @title Tabulation, Cross Tabulation and Graphical Display
#'
#' @description
#' generates simple tabulation, cross-tabulation and stratified cross-tabulation tables.
#'
#' @param x a vector describing the bars which make up the plot. It is usually on x axis.
#' @param y a vector describing the y axis or second variable in cross-tabulation or data relationship.
#' @param by a vector describing the grouping of x. By default, the plot generates a faceted barplot.
#' @param data an optional data frame (or object coercible by as.data.frame to a data frame) containing the variables for contigency table.
#' @param rnd an integer indicating the number of decimal places:
#' @param na.rm A logical value indicating to remove NA values in the table or not. By Default, the value is TRUE
#' @param pct type of percentages in cross-tabulation: by default, it shows row percentages in two
#' @param plot.display logical value, indicating whether plot will be displayed or not
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
#' @seealso ibarplot, inumsum, ikdplot, iboxplot, isum
#' @keywords tabulation, frequency table, cross-tabulation, contigency table
#' @examples
#' str(infert)
#' # tabulation of one factor variable
#' itab(education, data = infert)
#' itab(parity, data = infert)
#' itab(case, data = infert)
#' itab(spontaneous, data = infert)
#' # cross-tabulation
#' itab(case, education, data = infert, plot.display = FALSE)
#' itab(case, education, data = infert)
#' itab(case, education, data = infert, pct = 'col') # change percentage type
#' itab(case, education, data = infert, pct = 'row') # default percentage type
#' itab(case, education, data = infert, pct = 'all')
#' itab(case, education, data = infert, pct = 'none')
#'
#' # changing plot types
#' itab(induced, education, data = infert)
#' itab(induced, education, data = infert, plot.type = 'f') # default faceted type
#' itab(induced, education, data = infert, plot.type = 'p') # parallel bar
#' itab(induced, education, data = infert, plot.type = 's') # stacked bar
#' itab(induced, education, data = infert, plot.type = 'fs') # full stacked bar
#' itab(induced, education, data = infert, facet.ncol = 3) # change number of facets
#' itab(induced, education, data = infert, plot.type = 'p', show.legend = FALSE) # legend on and off
#' itab(induced, education, data = infert, save.plot = TRUE) # save plot to current directory
#' itab(induced, education, data = infert, save.plot = TRUE, plot.name = "mybarplot.tiff")
#' itab(induced, education, data = infert, save.plot = TRUE, width = 10, height = 8, dpi = 300)
#' # three variables - analysis
#' infert$case <- factor(infert$case)
#' infert$induced <- factor(infert$induced)
#' itab(case, education, induced, data = infert)
#' itab(case, education, induced, data = infert, na.rm = TRUE, pct = 'col')

#' @export
itab <- function(x, y = NULL, by  = NULL, data = NULL, rnd = 1, na.rm = FALSE,
                 x.varname = NULL, y.varname = NULL, by.varname = NULL,
                 pct = "row", plot.display = TRUE,
                 main = NULL, xlab = NULL, ylab = NULL,
                 show.legend = TRUE, legend.text = NULL,
                 plot.type = "f", facet.ncol = 2,
                 save.plot = FALSE, plot.name = 'itab.tiff',
                 width = 5, height = 4, dpi = 150)
{

  # data input
  if (!is.null(data)) {
    arguments <- as.list(match.call())
    x <- eval(arguments$x, data)
    y <- eval(arguments$y, data)
    by <- eval(arguments$by, data)

    lab.x <- ifelse(is.null(x.varname), arguments$x, x.varname)
    lab.y <- ifelse(is.null(y.varname), arguments$y, y.varname)
    if (is.null(by)) {lab.by <- NULL} else {
      lab.by <- ifelse(is.null(by.varname), arguments$by, by.varname)
    }
  } else {
    lab.x <- ifelse(is.null(x.varname), deparse(substitute(x)), x.varname)
    if (!is.null(y)) lab.y <- ifelse(is.null(y.varname), deparse(substitute(y)), y.varname)
    if (!is.null(by)) lab.by <- ifelse(is.null(by.varname), deparse(substitute(by)),
                                       by.varname)
  }

  # # create data frame
  # data <- data.frame(x)
  # if (!is.null(y)) {
  #   data <- data.frame(x, y)
  # }
  # if (!is.null(by)) {
  #   data <- data.frame(x, y, by)
  # }

  # levels of useNA in table() has c("no", "ifany", "always)
  include.na <- ifelse(na.rm, "no", "ifany")

  # switch mechanisms
  type <- ifelse(is.null(y), "uni", ifelse(is.null(by), "bi", "strata"))

  ctab <- function(tab, pct, rnd) {
    tabr <- rbind(tab, Total = colSums(tab))
    tabc <- cbind(tab, Total = rowSums(tab))
    tbl <- cbind(tabr, Total = rowSums(tabr))

    rpct <- round(tbl / rowSums(tabr) * 100, rnd) # row percentage
    cpct <- round(t(t(tbl) / colSums(tabc)) * 100, rnd) # column percentage
    tpct <- round(tbl / sum(tab) * 100, rnd)

    p.value <- suppressWarnings(chisq.test(tab)$p.value)
    p.value <- c(ifelse(p.value < 0.001, "<0.001", round(p.value, 3)),
                 rep("", nrow(tbl) - 1))

    tblr <- NULL; tblc <- NULL; tblt <- NULL
    var.r <- NULL; var.c <- NULL; var.t <- NULL
    for ( i in 1:ncol(tbl)) {
      tblt <- data.frame(cbind(tblt, tbl[,i], tpct[,i]))
      var.t <- c(var.t, colnames(tbl)[i], "(t%)")
      tblr <- data.frame(cbind(tblr, tbl[,i], rpct[,i]))
      var.r <- c(var.r, colnames(tbl)[i], "(r%)")
      tblc <- data.frame(cbind(tblc, tbl[,i], cpct[,i]))
      var.c <- c(var.c, colnames(tbl)[i], "(c%)")
    }

    tblt <- cbind(tblt, p.value = p.value)
    tblr <- cbind(tblr, p.value = p.value)
    tblc <- cbind(tblc, p.value = p.value)

    colnames(tblt) <- c(var.t, "p.value")
    colnames(tblr) <- c(var.r, "p.value")
    colnames(tblc) <- c(var.c, "p.value")
    names(dimnames(tbl)) <- c(lab.x, lab.y)
    names(dimnames(tblt)) <- c(lab.x, lab.y)
    names(dimnames(tblr)) <- c(lab.x, lab.y)
    names(dimnames(tblc)) <- c(lab.x, lab.y)


    df <- list(no.percentage = tbl,
               total.percentage = tblt,
               row.percentage = tblr,
               column.percentage = tblc)
    switch(pct,
           none = tbl,
           total = tblt,
           row = tblr,
           col = tblc,
           all = df)
  }

  res <- switch(
    type,
    uni = {
      # frequency table
      tbl <- table(x, useNA = include.na)
      t.count <- c(tbl, Total = sum(tbl))
      c.count <- c(cumsum(tbl), sum(tbl))
      names(c.count) <- names(t.count)
      r.freq <- t.count / sum(tbl) * 100
      c.freq <- c.count / sum(tbl) * 100
      names(c.freq) <- names(t.count)
      df <- cbind(t.count, round(r.freq, rnd), round(c.freq, rnd))
      colnames(df) <- c("n", "(col%)", "(cum.col%)")
      names(dimnames(df)) <- c(lab.x, "")
      uni = df
    },
    bi = ctab(table(x, y, useNA = include.na), pct = pct, rnd = rnd),
    strata = {
      by.levels <- as.character(unique(na.omit(by)))
      res <- lapply(by.levels,
                    function(z)
                      ctab(table(x[by == z], y[by == z]), pct = pct, rnd = rnd))
      res <- structure(res, names = by.levels)
      if (!na.rm) {
        res[["NA"]] <- ctab(table(x[is.na(by)], y[is.na(by)]), pct = pct, rnd = rnd)
      }
      strata = res
    })

  if (plot.display) {
    main <- ifelse(is.null(main),
                   ifelse(is.null(y), paste0('Plot of ', lab.x),
                          ifelse(is.null(by), paste0('Plot of ', lab.x, ' ~ ', lab.y),
                                 paste0('Plot of ', lab.x, ' ~ ', lab.y, ', stratified by ',
                                        lab.by))),main)
    xlab <- ifelse(is.null(xlab), lab.x, xlab)
    ylab <- ifelse(is.null(ylab),
                   ifelse(plot.type == 'fs', 'Percentage (%)', 'Number'), ylab)
    if (!is.null(y)) legend.text <- ifelse(is.null(legend.text), lab.y, legend.text)
    plot(ibarplot(x = x, y = y, by = by, rnd = rnd, na.rm = na.rm,
                  main = main, xlab = xlab, ylab = ylab, show.legend = show.legend,
                  legend.text = legend.text, plot.type = plot.type,
                  facet.ncol = facet.ncol, save.plot = save.plot,
                  plot.name = plot.name, width = width, height = height, dpi = dpi))
  }
  return(res)
}
