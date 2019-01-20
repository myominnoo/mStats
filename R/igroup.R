#' @title Summary by groups
#'
#' @description
#' igroup is a function that produces three contingency tables: one with counts only, another one with
#' counts and percentages for rows and the last one with counts and percentages for columns. In
#' addition, it automatically calculates Chi Square test and Fisher Exact test as well as displays
#' mosaic plot.
#'
#' Plot can be disabled by setting plot.display to FALSE.
#'
#' @param num An R object, continuous data
#' @param y An R object, categorical data
#' @param data an optional data frame (or object coercible by as.data.frame to a data frame)
#' containing the variables for contigency table.
#' @param rnd an integer indicating the number of decimal places:
#' @param na.rm A logical value indicating to remove NA values in the table or not.
#' By Default, the value is TRUE
#' @param plot.display A logical value, indicating whether the data will be plotted and display
#' @param plot.title A character value, specifying the name of the plot in categorical or continuous data. This argument does not work in summaring data.frame object, where the plot will be automatically mapped to the names of the data.frame.
#' @seealso isum, ixtab
#' @keywords grouped summary, categorical data, continous data
#' @examples
#' str(infert)
#' igroup(age, case, infert)
#' igroup(age, case, infert, plot.display = FALSE)
#' igroup(infert$age, infert$case)
#'
#' str(iris)
#' igroup(Petal.Length, Species, iris)
#' igroup(Petal.Length, Species, iris, plot.display = FALSE)
#' igroup(iris$Petal.Length, iris$Species)
#' igroup(iris$Petal.Length, iris$Species, plot.display = FALSE)


#' @export
igroup <- function(num, by, data = NULL, rnd = 1, na.rm = TRUE, plot.display = TRUE,
                   plot.title = NULL) {
  arguments <- as.list(match.call())
  if (is.null(data)) {
    num <- eval(arguments$num)
    by <- factor(eval(arguments$by))
    } else {
    num <- eval(arguments$num, data)
    by <- factor(eval(arguments$by, data))
  }
  df <- do.call(rbind, lapply(levels(by), function(x) isum(num[by == x], rnd = rnd, na.rm = na.rm,
                                                           plot.display = FALSE)))
  df <- do.call(rbind, list(Overall = isum(num, rnd, na.rm, plot.display = FALSE), df))
  spw.pvalue = round(df[,10], digits = 7)
  df <- cbind(df[,1:4], Std.Err = round(df[,4] / sqrt(df[,1]), rnd))
  df <- cbind(df,
              CI.Lwr = round(df[,3] - (1.96 * df[,5]), rnd + 1),
              CI.Upr = round(df[,3] + (1.96 * df[,5]), rnd + 1), Shapiro.pvalue = spw.pvalue)
  df <- do.call(rbind, list(df[1,], "_" = "_", df[-1,]))
  row.names(df)[-c(1,2)] <- levels(by)

  two.group <- length(levels(by)) < 3
  if(two.group) {s.test <- t.test(num[by == levels(by)[1]],
                                  num[by == levels(by)[2]])} else {
                                    s.test <- aov(num ~ by)
                                    s.test <- list(ANOVA.OneWay = summary(s.test),
                                                   PostHoc.Comparison.Tests = TukeyHSD(s.test))
                                  }

  if (plot.display) {
    plot(num ~ by,
         main = "Boxplots: Comparison",
         xlab = toString(arguments$by),
         ylab = toString(arguments$num),
         col = rainbow(length(levels(by)))
         )
  }

  if(two.group) {
    result <- list(Num.Group.Summary = df, T.test.TwoSample = s.test)
  } else {
    result <- list(Num.Group.Summary = df, ANOVA.OneWay = s.test)
  }
  return(result)
}


