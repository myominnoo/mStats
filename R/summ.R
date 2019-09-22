#' @title Number Summary for numerical data
#' @description
#' \code{summ} generates seven summary statistics for numerical data and tests
#' for normality on the fly.
#' @param x numeric object, list or data frame
#' @param data a data frame object (Optional)
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#' @param na.rm A logical value to specify missing values, <NA> in the table
#' @param ... optional arguments
#' @import stats
#' @details
#' \code{summ} reports number of observations in the dataset, missing data,
#' seven number summary statistics, coefficient of variation (CV.) and normality
#' test.
#' Normality test is perfomed by Shapiro-Wilk Normality Test. See more at
#' \code{\link{shapiro.test}}.
#'
#' CV = Standard Deviation / MEAN * 100
#'
#' ANNOTATIONS
#'
#' Obs. = observation
#' NA. = missing data
#' Mean = Mean value
#' Std.Dev = Standard deviation
#' Median = Median value
#' Q1 = First quartile or percentile
#' Q3 = Third quartile or percentile
#' Min = Minimum value
#' Max = Maximum value
#' Normal.test = test by Shapiro-Wilk Normality Test
#'
#' @seealso \code{\link{summBy}}, \code{\link{tab}}, \code{\link{xtab}}
#' @keywords number summary
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' codebook(infert)
#' summ(infert$age)
#' summ(age, data = infert)
#' # summarizing multiple numeric vectors
#' summ(list(infert$age, infert$parity, infert$induced, infert$spontaneous))
#' summ(list("age", "parity", "induced", "spontaneous"), infert)
#' # Number summary of the whole data frome
#' summ(infert)


#' @export
summ <- function(x, data = NULL, rnd = 1, na.rm = FALSE)
{
  arguments <- as.list(match.call())
  if (!is.null(data)) {
    x <- eval(substitute(x), data)
    if (as.character(arguments$x)[1] %in% c("list"))
      x <- list(as.character(arguments$x)[-1])
  }
  UseMethod("summ", x)
}

#' @rdname summ
#' @export
summ.default <- function(...) {
  warning(' ... Wrong Data Type ... ')
}

#' @rdname summ
#' @export
summ.numeric <- function(x, data = NULL, rnd = 1, na.rm = FALSE)
{
  arguments <- as.list(match.call())
  x.name <- deparse(substitute(x))
  if (!is.null(data)) {
    x <- eval(substitute(x), data)
  }

  len <- ifelse(na.rm, length(x[!is.na(x)]), length(x))
  na <- length(x[is.na(x)])
  na.rm <- TRUE
  mu <- mean(x, na.rm = na.rm)
  std <- sd(x, na.rm = na.rm)
  cv <- std / mu * 100
  q <- round(quantile(x, probs = c(0, .25, .5, .75, 1), na.rm = na.rm), rnd)
  v <- round(c(mu, std, cv, q), rnd)
  pvalue <- tryCatch({
    suppressWarnings(shapiro.test(x)$p.value)
  }, error = function(err) {
    return(NA)
  })
  pvalue <- ifelse(pvalue < 0.00001, "< 0.00001", round(pvalue, 5))
  f <- data.frame(Obs. = len, NA. = na, Mean = v[1], Std.Dev = v[2],
                  Median = v[6], Q1 = v[5], Q3 = v[7],
                  Min = v[4], Max = v[8],
                  CV. = v[3], Normal.test = pvalue,
                  stringsAsFactors = FALSE)
  row.names(f) <- x.name

  # print formating
  output.format(f, "Number Summary")

  invisible(f)
}

#' @rdname summ
#' @export
summ.list <- function(x, data = NULL, rnd = 1, na.rm = FALSE) {
  arguments <- as.list(match.call())
  if (!is.null(data)) {
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
               lapply(data, function (z)
                 summ.numeric(z, rnd = rnd, na.rm = na.rm)))
  sink()

  # print formating
  output.format(f, paste0("Number Summary"))

  invisible(f)
}

#' @rdname summ
#' @export
summ.data.frame <- function(x, data = NULL, rnd = 1, na.rm = FALSE) {
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
                 lapply(data, function (z)
                   summ.numeric(z, rnd = rnd, na.rm = na.rm)))
  } else {
    f <- summ.numeric(data, rnd = rnd, na.rm = na.rm)
  }
  sink()

  f <- data.frame(f)
  # print formating
  output.format(f, paste0("Number Summary: ", deparse(substitute(x))))

  invisible(f)
}
