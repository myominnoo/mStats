#' @title Number summary grouped by a factor
#' @description
#' \code{summ.by} generates seven number summary statistics and tests normality
#' on the fly grouped by a categorical variable.
#' @param x a numeric object
#' @param y a factor or character object
#' @param data a data frame object (Optional)
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#' @param na.rm A logical value to specify missing values, <NA> in the table
#' @param ... optional arguments
#' @details
#' Similar to \code{summ} output, \code{summBy}
#' reports number of observations in the dataset, missing data, seven number
#' summary statistics, coefficient of variation (CV.) and normality test.
#' Normality test is perfomed by Shapiro-Wilk Normality Test. See more at
#' \code{\link{shapiro.test}}.
#'
#' ANNOTATION
#'
#' Obs. = observation
#' NA. = missing data
#' Mean = Mean value
#' Std.Dev = Standard deviation
#' Median = Median value
#' Q1 = First quartile or percentile
#' Q3 = Third quartile or percentile
#' Min = Minimum value
#' Maximum = Maximum value
#'
#' If the second variable has two levels of category, it performs either
#' Student's t-test
#' \code{\link{t.test}} or Wilcoxon test (Mann-Whitney's test)
#' \code{\link{wilcox.test}}. If more than two levels, ANOVA
#' \code{\link{aov}} or
#' Kruskal-Wallis rank sum test \code{\link{kruskal.test}}
#' is carried out to test the difference between different groups.
#'
#' @import stats
#' @seealso \code{\link{summ}}, \code{\link{tab}}, \code{\link{xtab}}
#' @keywords number summary
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' summBy(age, education, infert)
#' summBy(spontaneous, education, infert)
#'
#' # multiple variables
#' summBy(list("age", "spontaneous", "case"), education, infert)
#'
#' # all numericals in the whole dataset
#' summBy(infert, education)
#' summBy(iris, Species)

#' @export
summBy <- function(x, y, data = NULL, rnd = 1, na.rm = FALSE)
{
  arguments <- as.list(match.call())
  if (!is.null(data)) {
    x <- eval(substitute(x), data)
    if (as.character(arguments$x)[1] %in% c("list"))
      x <- list(as.character(arguments$x)[-1])
  }
  UseMethod("summBy", x)
}

#' @rdname summBy
#' @export
summBy.default <- function(...) {
  warning(' ... Wrong Data Type ... ')
}

#' @rdname summBy
#' @export
summBy.numeric <- function(x, y, data = NULL, rnd = 1, na.rm = FALSE)
{
  arguments <- as.list(match.call())
  x.name <- deparse(substitute(x))
  y.name <- deparse(substitute(y))
  if (!is.null(data)) {
    x <- eval(substitute(x), data)
    y <- eval(substitute(y), data)
  }

  t <- table(y, useNA = "ifany")
  lvl <- names(t)
  lvl[is.na(lvl)] <- "<NA>"

  sink(tempfile())
  f <- do.call(
    rbind,
    lapply(lvl, function(z) {
      if (z == "<NA>") {summ(x[is.na(y)], rnd = rnd, na.rm = na.rm)} else
      {summ(x[y == z], rnd = rnd, na.rm = na.rm)}
    })
  )
  sink()

  # assign row name to final output
  f <- f[,c(1:7,11)]
  row.names(f) <- lvl

  if (length(lvl) > 2) {
    pvalue <- tryCatch({
      suppressWarnings(summary(aov(x ~ y))[[1]][1,5])
    }, error = function(err) {
      return(NA)})
    pvalue.name <- 'ANOVA'

    pvalue <- c(
      pvalue,
      tryCatch({
        suppressWarnings(kruskal.test(x ~ y)$p.value)
      }, error = function(err) {
        return(NA)})
    )
    pvalue.name <- c(pvalue.name, 'Kruskal-Wallis')

  } else {
    pvalue <- tryCatch({
      suppressWarnings(t.test(x ~ y)$p.value)
    }, error = function(err) {
      return(NA)
    })
    pvalue.name <- 't-test'

    pvalue <- c(
      pvalue,
      tryCatch({
        suppressWarnings(wilcox.test(x ~ y)$p.value)
      }, error = function(err) {
        return(NA)})
    )
    pvalue.name <- c(pvalue.name, 'Wilcoxon')
  }
  pvalue <- sprintf(pvalue, fmt = '%#.5f')

  f <- cbind(f, rbind(pvalue,
                      matrix(rep("", 2 * (length(lvl) - 1)), ncol = 2)))
  names(f)[9:10] <- pvalue.name

  # print formating
  output.format(f, paste0("Number Summary: ", x.name, " | ", y.name))
  invisible(f)
}

#' @rdname summBy
#' @export
summBy.list <- function(x, y, data = NULL, rnd = 1, na.rm = FALSE) {
  arguments <- as.list(match.call())
  y.name <- deparse(substitute(y))
  if (!is.null(data)) {
    y <- eval(substitute(y), data)
    vn <- as.character(arguments$x)[-1]
    df <- sapply(
      paste(deparse(substitute(data)), "$", vn, sep = ""),
      function(z) eval(parse(text = z))
    )
    data <- data.frame(unlist(df))
    names(data) <- vn
  } else {
    vn <- as.character(arguments$x)[-1]
    data <- data.frame(do.call(cbind, x))
    names(data) <- vn
  }

  sink(tempfile())
  f <- do.call(rbind,
               lapply(data, function(z) {
                 summBy.numeric(z, y, rnd = rnd, na.rm = na.rm)
               }))
  sink()

  # print formating
  output.format(f, paste0("Number Summary: ",
                          paste(vn, collapse = " + "), " | ", y.name))
  invisible(f)
}

#' @rdname summBy
#' @export
summBy.data.frame <- function(x, y, data = NULL, rnd = 1, na.rm = FALSE) {
  arguments <- as.list(match.call())
  y <- eval(substitute(y), x)
  y.name <- arguments$y

  data <- as.data.frame(x)
  var.names <- names(data)

  type.factor <- c("factor", "character", "logical")
  type.numeric <- c("integer", "double", "numeric")
  type.date <- c("Date")

  var.type <- sapply(var.names, function(z) class(unlist(x[ , z])))

  var.numeric <- names(var.type[!(var.type %in% type.factor) &
                                  !(var.type %in% type.date)])
  data <- data[, var.numeric]

  names.invalid <- grep("^([[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*$",
                        var.numeric, value = TRUE, invert = TRUE)
  var.numeric[var.numeric %in% names.invalid] <- paste0("v", names.invalid)
  names(data) <- var.numeric
  if (length(var.numeric) == 0) stop(" ... no numerical variables found ... ")

  sink(tempfile())
  if (length(var.numeric) > 1) {
    f <- do.call(rbind,
                 lapply(data, function(z) {
                   summBy.numeric(z, y, rnd = rnd, na.rm = na.rm)
                 }))
  } else {
    f <- summBy.numeric(data, y, rnd = rnd, na.rm = na.rm)
  }
  sink()

  # print formating
  output.format(f, paste0("Number Summary: ",
                          paste(names(data), collapse = " + "), " | ", y.name))
  invisible(f)
}

