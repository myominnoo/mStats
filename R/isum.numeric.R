#' A Quick Summary function with Graph Display
#'
#' @description
#' isum() lets you do a quick summary on your data
#'
#' For the class "factor", it tabulates and gives frequency, percentages and
#'     cummulative percentages. The same is with the class "character".
#'
#' For the class "integer", it provides seven summary measures:
#'     mean, standard deviation, median, Q1, Q3, minimum and maximum.
#'
#' For the class "data.frame", it gives a list of all the variables based on
#'     their data types.
#'
#' @param x An R object: it can be a dataframe or a vector.
#' @param rnd an integer indicating the number of decimal places:
#' @param na.rm A logical value indicating whether "NA" missing values should be
#'     removed before the computation proceeds.
#'
#' @seealso isum.factor, isum.numeric, isum.data.frame
#' @keywords summarize, isum, basic statistics, quick summary
#' @export


isum.numeric <- function(x, rnd = 1, na.rm = TRUE, plot.title = NULL) {
  len <- length(x) # total observations
  na <- length(x[is.na(x)]) # missing observations
  if(na > 0) na.rm <- TRUE
  mu <- mean(x, na.rm = na.rm)
  std <- sd(x, na.rm)
  q <- quantile(x, probs = c(0, .25, .5, .75, 1), na.rm = na.rm)
  v <- round(c(mu, std, q), rnd)
  df <- data.frame(Obs. = len, NA. = na,
                   mean = v[1], sd = v[2],
                   median = v[5], Q.25 = v[4], Q.75 = v[6],
                   min = v[3], max = v[7],
                   row.names = "")
  # Normality Plot
  qqnorm(x, col = "blue",
         main = paste0("Normal Q-Q Plot of variable: '",
                       ifelse(is.null(plot.title),
                              deparse(substitute(x)),
                              plot.title), "'"))
  qqline(x, col = "red")
  sp <- shapiro.test(x)
  pvalue <- sp$p.value
  if(pvalue < 0.0001) {pvalue <- "<0.0001"} else {pvalue <- round(pvalue, 4)}
  text(-1.8, v[7], "Shapiro-Wilk normality test", cex = 0.75, col = "red")
  text(-2, v[7] - (v[7] * .05), paste("p-value =", pvalue),
       cex = 0.75, col = "red")
  return(df)
}

