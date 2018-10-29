#' A Quick Summary function with Graph Display
#'
#' @description
#' isum() lets you do a quick summary on your data type.
#'
#' For "factor" type, it tabulates and gives frequency, percentages and
#'     cummulative percentages.
#' For "integer" type, it provides seven summary measures:
#'     mean, standard deviation, median, Q1, Q3, minimum and maximum.
#' For "data.frame" type, it gives a list of all the variables based on
#'     their data types.
#'
#' @usage isum(x, ...)
#' @usage ## Default S3 method:
#' isum(x, l.size = 10L, ...)
#' @usage ## Default S3 method for class 'factor'
#' isum(x, rnd = 1, na.rm = TRUE, ...)
#' @param ## Default S3 method for class 'numeric'
#' isum(x, rnd = 1, na.rm = TRUE, ...)
#' @param ## Default S3 method for class 'data.frame'
#' isum(x, ...)
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
#' @export
#' @examples
#' isum(iris)

isum.numeric <- function(x, rnd = 1, na.rm = TRUE, ...) {
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

