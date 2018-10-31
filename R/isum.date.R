#' A Quick Summary function for Object of type "Date"
#'
#' @description
#' summ_date() lets you do a quick summary on your data
#'
#' @param x An R object: it can be a dataframe or a vector.
#' @param rnd an integer indicating the number of decimal places:
#' @param na.rm A logical value indicating whether "NA" missing values should be
#'     removed before the computation proceeds.
#'
#' @seealso isum.factor, isum.numeric, isum.data.frame
#' @keywords summarize, date, basic statistics, quick summary
#' @export
#' @examples
#' isum(iris)

summ_date <- function(x, rnd = 1, na.rm = TRUE) {
  len <- length(x) # total observations
  na <- length(x[is.na(x)]) # missing observations
  if(na > 0) na.rm <- TRUE
  mu <- mean(x, na.rm = na.rm)
  std <- sd(x, na.rm)
  q <- quantile(unclass(x), probs = c(0, .25, .5, .75, 1),
                na.rm = na.rm)
  q <- as.Date(q, origin = "1970-01-01")
  v <- c(mu, std, q)
  df <- data.frame(Obs. = len, NA. = na,
                   mean = v[1], sd = v[2],
                   median = v[5], Q.25 = v[4], Q.75 = v[6],
                   min = v[3], max = v[7],
                   row.names = "")
  plot(x, main = paste0("Plot of ", deparse(substitute(x))))
  return(df)
}
