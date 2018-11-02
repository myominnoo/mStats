#' A Quick Summary function with Graph Display
#'
#' @description
#' igroup() lets you do a quick summary on your data
#'
#' @param x An R Object: This must be numeric
#' @param y An R Object: This must be factor.
#' @param data An Option to specify dataset.
#' @param rnd an integer indicating the number of decimal places:
#' @param na.rm A logical value indicating whether "NA" missing values should be
#'     removed before the computation proceeds.
#' @param plot.title An optional input to indicate the plottitle: This value passes
#'     through to isum() function along with the title name for the plot.
#' @seealso isum.factor, isum.numeric, isum.data.frame
#' @keywords summarize, isum, basic statistics, quick summary
#' @export
#' @examples
#' str(iris)
#' igroup(x = "Sepal.Length", y = "Species", data = iris)
#' igroup(iris$Sepal.Length, iris$Species)
#' str(infert)
#' igroup(x = "age", y = "case", data = infert)
#' igroup(infert$age, infert$case)

igroup <- function(x, y, data = NULL, rnd = 1, na.rm = TRUE, plot.title = NULL) {
  if(!is.null(data)) {num <- data[[x]]} else {num <- x}
  if(!is.null(data)) {by <- data[[y]]} else {by <- y}
  if(!is.factor(by)) {by <- factor(by)}

  df <- do.call(rbind,
                lapply(levels(by),
                       function(i) isum(num[by == i], rnd = rnd, na.rm = na.rm,
                                        plot.title = i)))
  df <- do.call(rbind,
                list(Overall = isum(num, rnd, na.rm,
                                    plot.title = deparse(substitute(x))), df))
  df <- cbind(df[,1:4],
              Std.Err = round(df[,4] / sqrt(df[,1]), rnd))
  df <- cbind(df,
              CI.95.Lower = df[,3] - (1.96 * df[,5]),
              CI.95.Upper = df[,3] + (1.96 * df[,5]))
  df <- do.call(rbind, list(df[1,], "_" = "_", df[-1,]))
  row.names(df)[-c(1,2)] <- levels(by)

  two.group <- length(levels(by)) < 3
  if(two.group) {s.test <- t.test(num[by == levels(by)[1]],
                                  num[by == levels(by)[2]])} else {
    s.test <- aov(num ~ by)
    s.test <- list(ANOVA.OneWay = summary(s.test),
                   PostHoc.Comparison.Tests = TukeyHSD(s.test))
  }

  plot(num ~ by,
       main = "Comparison of Boxplots:",
       xlab = ifelse(is.null(data), deparse(substitute(x)), x),
       ylab = ifelse(is.null(data), deparse(substitute(y)), y),
       col = rainbow(length(levels(by))))
  if(two.group) {
    result <- list(Num.Group.Summary = df, T.test.TwoSample = s.test)
  } else {
    result <- list(Num.Group.Summary = df, ANOVA.OneWay = s.test)
  }
  return(result)
}
