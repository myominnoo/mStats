#' A Quick Summary function with Graph Display
#'
#' @description
#' isum() lets you do a quick summary on your data type.
#'
#' For "factor" data type, it tabulates and gives frequency, percentages and
#'     cummulative percentages.
#' For "integer" data type, it provides seven summary measures:
#'     mean, standard deviation, median, Q1, Q3, minimum and maximum.
#'
#' @param x An R object: it can be a dataframe or a vector.
#' @param ... Additional parameters
#' @param l.size An integer value determining whether the variable should be treated
#'     as factor
#'
#' @param rnd an integer indicating the number of decimal places:
#' @param na.rm A logical value indicating whether "NA" missing values should be
#'     removed before the computation proceeds.
#'
#' @seealso isum.factor, isum.numeric, isum.data.frame
#' @keywords summarize, isum, basic statistics, quick summary
#' @examples
#' isum()

isum <- function(x, ...) {
  UseMethod("isum", x)
}

isum.default <- function(x, ..., l.size = 10L) {
  if (is.data.frame(x)) {
    return(x)
  } else {
    if (length(unique(x)) < l.size) {
      x <- factor(x)
      UseMethod("isum", x)
    } else {
      UseMethod("isum", x)
    }
  }
}

isum.factor <- function(x, rnd = 1, na.rm = TRUE) {
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
  # Bar Chart
  barplot(height = tbl)
  return(df)
}


isum.numeric <- function(x, rnd = 1, na.rm = TRUE) {
  len <- length(x) # total observations
  na <- length(x[is.na(x)]) # missing observations
  mu <- mean(x, na.rm = na.rm)
  std <- sd(x, na.rm)
  q <- quantile(x, probs = c(0, .25, .5, .75, 1), na.rm)
  v <- round(c(mu, std, q), rnd)
  df <- data.frame(Obs. = len, NA. = na,
                   mean = v[1], sd = v[2],
                   median = v[5], Q.25 = v[4], Q.75 = v[6],
                   min = v[3], max = v[7],
                   row.names = "")
  # Normality Plot
  qqnorm(x, col = "blue")
  qqline(x, col = "red")
  sp <- shapiro.test(x)
  sp$p.value
  text(-1.8, v[7], "Shapiro-Wilk normality test", cex = 0.75, col = "red")
  text(-2, v[7] - (v[7] * .05), paste("p-value =", round(sp$p.value, 4)),
       cex = 0.75, col = "red")
  return(df)
}

