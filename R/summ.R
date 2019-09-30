#' @title Number Summary for numerical data
#'
#' @description
#'
#' \code{summ} generates seven summary statistics for numerical data and tests
#' for normality on the fly.
#'
#' @param x numeric object, list or data frame
#' @param y a factor or character object
#' @param data a data frame object (Optional)
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#' @param na.rm A logical value to specify missing values, <NA> in the table
#'
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
#' \strong{ANNOTATIONS}
#'
#' Obs. = observation
#'
#' NA. = missing data
#'
#' Mean = Mean value
#'
#' Std.Dev = Standard deviation
#'
#' Median = Median value
#'
#' Q1 = First quartile or percentile
#'
#' Q3 = Third quartile or percentile
#'
#' Min = Minimum value
#'
#' Max = Maximum value
#'
#' Normal.test = test by Shapiro-Wilk Normality Test
#'
#' @seealso \code{\link{summBy}}, \code{\link{tab}}, \code{\link{xtab}}
#' @keywords number summary, statistics, descriptive, five number
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#' # single variable
#' summ(age, infert)
#' summ(infert$age)
#'
#' # multiple variable
#' summ(c(age, case, parity), infert)
#'
#' # whole dataframe
#' summ(infert)
#' summ(iris)
#' summ(mtcars)
#'
#' # subset: one category and one number
#' infert.new <- keep(infert, education, age)
#' summ(infert.new)
#' }


#' @export
summ <- function(x, data = NULL, rnd = 1, na.rm = FALSE)
{
  arguments <- as.list(match.call())
  x.name <- (deparse(substitute(x)))
  x.name <- unlist(strsplit(gsub("^c\\(|\\)$", "", x.name), ","))

  catch <- tryCatch(is.data.frame(x), error=function(e) {})
  if (!is.null(data))
    x <- eval(substitute(x), data)
  if (is.null(catch)) catch <- FALSE
  if (catch) x <- data.frame()
  if (length(x.name) > 1) x <- list()
  UseMethod("summ", x)
}


#' @rdname summ
#' @export
summ.default <- function(x, data = NULL, rnd = 1, na.rm = FALSE)
{
  stop("... Wrong data type ...")
}

#' @rdname summ
#' @export
summ.numeric <- function(x, data = NULL, rnd = 1, na.rm = FALSE)
{
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
  pvalue <- sprintf(pvalue, fmt = '%#.5f')

  f <- data.frame(Obs. = len, NA. = na, Mean = v[1], Std.Dev = v[2],
                  Median = v[6], Q1 = v[5], Q3 = v[7],
                  Min = v[4], Max = v[8],
                  CV. = v[3], Normal.test = pvalue,
                  stringsAsFactors = FALSE)
  row.names(f) <- x.name

  x.lbl <- attr(x, "label")
  x.lbl <- ifelse(is.null(x.lbl), "NULL", x.lbl)
  texts <- paste("Number Summary: ", x.name, "\n",
                 "label: ", paste0(x.lbl), sep = "", collapse = "")

  printText(f, texts, "label: ")

  invisible(f)
}


#' @rdname summ
#' @export
summ.list <- function(x, data = NULL, rnd = 1, na.rm = FALSE)
{
  arguments <- as.list(match.call())
  x.name <- deparse(substitute(x))
  x.name <- unlist(strsplit(gsub("^c\\(|\\)$", "", x.name), ","))
  x.name <- gsub(" ", "", x.name)

  data.list <- NULL
  if (is.null(data)) {
    data <- lapply(x.name, function(z) eval(parse(text = z)))
    data <- as.data.frame(data)
    names(data) <- x.name
  } else {
    data <- data[, x.name]
    names(data) <- x.name
  }

  sink(tempfile())
  f <- do.call(rbind, lapply(data, function(z){
    summ.numeric(z, na.rm = na.rm, rnd = rnd) }))
  sink()

  texts <- paste0("Number Summary: ", paste(x.name, collapse = " | "), collapse = "")
  printText(f, texts)

  x.lbl <- (sapply(x.name, function(z) attr(data[, z], "label")))
  if (!is.null(unlist(x.lbl))) {
    printMsg(paste0("labels: "))
    for (i in 1:length(x.name)) {
      printMsg(paste0(x.name[i], ": ", x.lbl[i], collapse = ""))
    }
  }

  invisible(f)
}

#' @rdname summ
#' @export
summ.data.frame <- function(x, data = NULL, rnd = 1, na.rm = FALSE)
{
  data <- x
  vars <- names(x)
  type.numeric <- c("int", "double", "numeric")

  vars.type <- sapply(vars, function(z) class(unlist(x[ , z])))
  vars.names <- vars[(vars.type %in% type.numeric)]
  data <- data[, vars.names]

  if (is.data.frame(data)) {
    if (ncol(data) == 0)
      stop("... no numerical variables found ...")
    names.invalid <- grep("^([[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*$",
                          vars.names, value = TRUE, invert = TRUE)
    if (length(names.invalid) > 0) {
      vars.names[vars.names %in% names.invalid] <- paste0("v", names.invalid)
      names(data) <- vars.names
    }
  }

  sink(tempfile())
  if (is.data.frame(data)) {
    f <- do.call(rbind, lapply(data, function(z){
      summ.numeric(z, na.rm = na.rm, rnd = rnd) }))
    x.lbl <- lapply(data, function(z) attr(z, "label"))
  } else {
    f <- summ.numeric(data, na.rm = na.rm, rnd = rnd)
    row.names(f) <- vars.names
    x.lbl <- attr(data, "label")
  }
  sink()

  if (is.data.frame(data)) {
    texts <- paste0("Number Summary: ",
                    paste(vars.names, collapse = " | "),
                    collapse = "")
    printText(f, texts)

    x.lbl <- (sapply(vars.names, function(z) attr(data[, z], "label")))
    if (!is.null(unlist(x.lbl))) {
      printMsg(paste0("labels: "))
      for (i in 1:length(vars.names)) {
        printMsg(paste0(vars.names[i], ": ", x.lbl[i], collapse = ""))
      }
    }

  } else {
    x.lbl <- ifelse(is.null(x.lbl), "NULL", x.lbl)
    texts <- paste0("Number Summary: ", vars.names, "\n",
                    "label: ", paste0(x.lbl), collapse = "")
    printText(f, texts, "label: ")
  }

  invisible(f)
}
