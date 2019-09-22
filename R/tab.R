#' @title Tabulation
#' @description
#' \code{tab} generates frequency distribution of a categorical variable.
#' @param x a character, factor or logical object OR list OR data frame
#' @param data a data frame object (Optional)
#' @param na.rm A logical value to specify missing values, <NA> in the table
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#' @param ... optional arguments
#' @details
#' Exploring data before jumping into complex analysis is always a necessity.
#' The first step of an analysis is always to summarize and display data.
#'
#' \code{tab}
#' produce one-way table of frequencies.
#'
#' \strong{References:}
#' \enumerate{
#'   \item Essential Medical Statistics, Betty R. Kirkwood & Jonathan A.C. Sterne,
#'   Second Edition. Chapter 3
#'   \item An Introduction to MEdical Statistics, Martin Bland, Thrid Edition,
#'   Chapter 4
#' }
#'
#' @seealso \code{\link{xtab}}
#' @keywords frequency distribution, tabulation, one-way table
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' tab(infert$education)
#' tab(education, infert)
#' tab(case, infert)
#'
#' # multiple variables
#' tab(list(education, case, spontaneous), infert)
#' tab(list(infert$education, infert$case, infert$spontaneous))
#'
#' # whole dataset
#' tab(infert)
#' tab(iris)

#' @export
tab <- function(x, data = NULL, na.rm = FALSE, rnd = 1)
{
  arguments <- as.list(match.call())
  if (!is.null(data)) {
    if (as.character(arguments$x)[1] %in% c("list"))
      x <- list(as.character(arguments$x)[-1]) else
        x <- as.character(eval(arguments$x, data))
  } else {
    if (is.factor(x) | is.logical(x) | is.numeric(x))
      x <- as.character(arguments$x)
  }
  UseMethod("tab", x)
}

#' @rdname tab
#' @export
tab.default <- function(...) {
  warning(' ... Wrong Data Type ... ')
}

#' @rdname tab
#' @export
tab.character <- function(x, data = NULL, na.rm = FALSE, rnd = 1)
{
  arguments <- as.list(match.call())
  x.name <- deparse(substitute(x))
  if (!is.null(data)) {
    x <- eval(substitute(x), data)
  }
  na.rm <- ifelse(na.rm, "no", "ifany")

  t <- table(x, useNA = na.rm)
  t.cnt <- c(t, Total = sum(t))
  t.cum <- c(cumsum(t), sum(t))
  f <- cbind(Freq. = t.cnt,
             Percent. = round(t.cnt / sum(t) * 100, rnd),
             Cum.Percent. = round(t.cum / sum(t) * 100, rnd))
  names(attributes(f)$dimnames) <- c(x.name, "")
  f <- data.frame(f)

  # print formating
  output.format(f, paste0("Tabulation: ", x.name))

  invisible(f)
}

#' @rdname tab
#' @export
tab.list <- function(x, data = NULL, na.rm = FALSE, rnd = 1)
{
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
  f <- lapply(data, function(z){
    tab.character(z, na.rm = na.rm, rnd = rnd)
  })
  sink()

  for (i in 1:length(vn)) {
    t <- f[[i]]
    output.format(t, paste0("Tabulation: ", vn[i]))
  }

  invisible(f)
}

#' @rdname tab
#' @export
tab.data.frame <- function(x, data = NULL, na.rm = FALSE, rnd = 1)
{
  data <- as.data.frame(x)
  var.names <- names(data)

  type.factor <- c("factor", "character", "logical")
  type.numeric <- c("integer", "double", "numeric")
  type.date <- c("Date")

  var.type <- sapply(var.names, function(z) class(unlist(x[ , z])))
  var.factor <- names(var.type[!(var.type %in% type.numeric) &
                                 !(var.type %in% type.date)])
  data <- data[, var.factor]
  names.invalid <- grep("^([[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*$",
                        var.factor, value = TRUE, invert = TRUE)
  var.factor[var.factor %in% names.invalid] <- paste0("v", names.invalid)
  names(data) <- var.factor

  if (length(var.factor) == 0) stop(" ... no categorical variables found ... ")

  sink(tempfile())
  if (length(var.factor) > 1) {
    f <- lapply(data, function(z){
      tab.character(z, na.rm = na.rm, rnd = rnd)
    })
  } else {
    f <- tab.character(data, na.rm = na.rm, rnd = rnd)
  }
  sink()

  if (length(var.factor) > 1) {
    for (i in 1:length(var.factor)) {
      t <- f[[i]]
      output.format(t, paste0("Tabulation: ", var.factor[i]))
    }
  } else {
    output.format(f, paste0("Tabulation: ", var.factor))
  }

  invisible(f)
}
