#' A Quick Summary function
#'
#' isum() lets you do a quick summary on your data.
#' @param x An R object: it can be a dataframe or a vector.
#' @param rnd an integer indicating the number of decimal places:
#' @param na.rm A logical value indicating whether "NA" missing values should be removed before the computation proceeds.
#' @param l.size An integer value determining how many levels of
#' unqiue values a vector must have in order to display as factor.
#' @keywords summarize, isum, basic statistics, quick summary
#' @export
#' @examples
#' isum()

isum <- function(x, rnd = 1, na.rm = TRUE, l.size = 10L) {
  if (!is.data.frame(x)) {
    if (length(unique(x)) < l.size) {
      x <- factor(x)
      # levels of useNA in table() has c("no", "ifany", "always)
      if (na.rm) {include.na = "no"} else {include.na = "ifany"}
      len <- length(x) # total observations
      na <- length(x[is.na(x)]) # missing observations
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
    } else {
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
  } else {
    print("...x is a dataframe ...")
  }
}

