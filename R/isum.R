#' @title A Quick Summary function with Graph Display
#'
#' @description
#' isum is a generic function to produce a quick summary results of various types of data.
#' The function invokes on particular methods which depend on the class of the first
#' argument.
#' If the data is categorical, it generates frequency, relative frequency and cummulative
#' frequency.
#' If continuous, it generates five number summaries (median, Q1, Q3, min,
#' max), mean and standard deviation.
#' if the input is data.frame, it generates a list containing number summary, frequency table and additional information for date variable.
#'
#' @param x An R object, for which summary statistics are desired
#' @param rnd Integer, indicating how many decimal places should be shown
#' @param na.rm A logical value, indicating whether NA values should be removed before the
#' analysis
#' @param plot.display A logical value, indicating whether the data will be plotted and display
#' @param plot.title A character value, specifying the name of the plot in categorical or continuous data. This argument does not work in summaring data.frame object, where the plot will be automatically mapped to the names of the data.frame.
#' @seealso ixtab, igroup
#' @keywords summarize, isum, statistics, quick summary
#' @examples
#' ?iris
#' str(iris)
#' isum(iris)
#'
#' ?infert
#' str(infert)
#' isum(infert)
#'
#' ?trees
#' str(trees)
#' isum(trees)

#' @export isum
isum <- function(x, rnd = 1, na.rm = TRUE, plot.display = TRUE, plot.title = NULL) {
  UseMethod("isum")
}

#' @rdname  isum
#' @export
isum.default <- function(...) {
  message("... Warning: 'x' is not a data type for summary statistics ...")
}

#' @rdname  isum
#' @export
isum.factor <- function(x, rnd = 1, na.rm = TRUE, plot.display = TRUE, plot.title = NULL) {
  x <- factor(x, levels = unique(x), labels = unique(x))
  # levels of useNA in table() has c("no", "ifany", "always)
  if (na.rm) {include.na = "no"} else {include.na = "ifany"}
  tbl <- table(x, useNA = include.na) # default na.rm = TRUE
  t.count <- c(tbl, Total = sum(tbl))
  c.count <- c(cumsum(tbl), sum(tbl))
  names(c.count) <- names(t.count)
  r.freq <- t.count / sum(tbl) * 100
  c.freq <- c.count / sum(tbl) * 100
  names(c.freq) <- names(t.count)
  df <- as.data.frame(cbind(Freq. = t.count,
                            Percent = round(r.freq, rnd),
                            # Cum.Count = c.count,
                            Cum. = round(c.freq, rnd)))
  if(plot.display) {
    barplot(height = tbl,
            main = paste0("Bar Plot: ", ifelse(is.null(plot.title), deparse(substitute(x)),
                                 plot.title)),
            col = rainbow(length(tbl)))
  }
  return(df)
}

#' @rdname  isum
#' @export
isum.character <- function(...) {
  isum.factor(...)
}

#' @rdname  isum
#' @export
isum.logical <- function(...) {
  isum.factor(...)
}

#' @rdname  isum
#' @export
isum.numeric <- function(x, rnd = 1, na.rm = TRUE, plot.display = TRUE, plot.title = NULL) {
  len <- length(x) # total observations
  na <- length(x[is.na(x)]) # missing observations
  if(na > 0) na.rm <- TRUE
  mu <- mean(x, na.rm = na.rm)
  std <- sd(x, na.rm)
  q <- round(quantile(x, probs = c(0, .25, .5, .75, 1), na.rm = na.rm), rnd)
  v <- round(c(mu, std, q), rnd)
  sp <- shapiro.test(x)
  df <- data.frame(Obs. = len, NA. = na,
                   Mean = v[1], Std.Dev = v[2],
                   Median = v[5], Q.25 = v[4], Q.75 = v[6],
                   Min = v[3], Max = v[7], Shapiro.pvalue = sp$p.value,
                   row.names = "")

  if(plot.display) {
    qqnorm(x, main = paste0("Normal Q-Q Plot: ", ifelse(is.null(plot.title),
                                                        deparse(substitute(x)),
                                                        plot.title)))
    qqline(x, col = "red")
    if(sp$p.value < 0.0001) {p.value <- "<0.001"} else {p.value <- round(sp$p.value, 4)}
    text(-1.8, v[7], "Shapiro-Wilk Normality Test", cex = 0.75, col = "blue")
    text(-2, v[7] - (v[7] * .05), paste("p-value =", p.value), cex = 0.75, col = "blue")
  }
  return(df)
}

#' @rdname  isum
#' @export
isum.data.frame <- function(x, rnd = 1, na.rm = TRUE, plot.display = TRUE, ...) {
  x <- data.frame(x)
  # create logical vector for date type
  v_date <- unlist(lapply(names(x), function(y)
    return(ifelse(ifelse(class(x[,y]) == "Date", TRUE, FALSE), TRUE, FALSE))))
  v_num <- unlist(lapply(names(x), function(y)
    return(ifelse(is.numeric(x[,y]), TRUE, FALSE))))
  v_freq <- !v_date & !v_num

  num <- do.call(rbind, lapply(names(x)[v_num],
                               function(y) isum(x[,y], rnd, na.rm, plot.display,
                                                plot.title = toString(y))))
  row.names(num) <- names(x)[v_num]
  freq <- lapply(names(x)[v_freq], function(y) isum(x[,y], rnd, na.rm, plot.display,
                                                    plot.title = toString(y)))
  freq <- structure(freq, names = names(x)[v_freq])
  if (any(v_freq) == FALSE) freq <- "... no frequency tables to be shown ..."
  if (any(v_date) == FALSE) {
    add.info <- "... No Additional Information ..."
  } else {add.info <- paste0("... '", ncol(x[,v_date]), "' variable(s) are of type 'Date' ...")}
  return(list(Num.Summary = num,
              Freq.Summary = freq,
              Additional.Info = add.info))
}
