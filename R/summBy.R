#' @title Number summary grouped by a factor
#' @description
#' \code{summ.by} generates seven number summary statistics and tests normality
#' on the fly grouped by a categorical variable.
#' @param x a numeric object
#' @param y a factor or character object
#' @param data a data frame object (Optional)
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#' @param na.rm A logical value to specify missing values, <NA> in the table
#' @details
#' Similar to \code{summ} output, \code{summBy}
#' reports number of observations in the dataset, missing data, seven number
#' summary statistics, coefficient of variation (CV.) and normality test.
#' Normality test is perfomed by Shapiro-Wilk Normality Test. See more at
#' \code{\link{shapiro.test}}.
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
#' @keywords number summary, statistics, descriptive, five number
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#' # single x variable
#' summBy(age, education, infert)
#' summBy(infert$age, infert$education)
#'
#' # multiple variables
#' summBy(c(age, parity, stratum), education, infert)
#'
#' # whole dataframe
#' summBy(infert, education)
#' summBy(iris, Species)
#'
#' # variables labels
#' infert.new <- labelVars(infert,
#'                 c(age, case, spontaneous, education),
#'                 c("age of patient",
#'                   "case (yes or no)",
#'                   "spontaneous labor",
#'                   "education of patient"))
#' infert.new <- keep(infert.new, age, case, spontaneous, education)
#' codebook(infert.new)
#' summBy(infert.new, education)
#' }



#' @export
summBy <- function(x, y, data = NULL, rnd = 1, na.rm = FALSE)
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
  UseMethod("summBy", x)
}


#' @rdname summ
#' @export
summBy.default <- function(x, y, data = NULL, rnd = 1, na.rm = FALSE)
{
  stop("... Wrong data type ...")
}

#' @rdname summ
#' @export
summBy.numeric <- function(x, y, data = NULL, rnd = 1, na.rm = FALSE)
{
  x.name <- deparse(substitute(x))
  y.name <- deparse(substitute(y))
  if (!is.null(data)) {
    x <- eval(substitute(x), data)
    y <- eval(substitute(y), data)
    data <- data[, c(x.name, y.name)]
  } else {
    data <- data.frame(x, y)
    names(data) <- c(x.name, y.name)
  }

  na.rm <- ifelse(na.rm, "no", "ifany")
  t <- table(y, useNA = na.rm)
  lvl <- names(t)
  lvl[is.na(lvl)] <- "<NA>"

  sink(tempfile())
  f <- do.call(rbind, lapply(lvl, function(z) {
    if (z == "<NA>") d <- data[is.na(y), x.name] else
      d <- data[y == z, x.name]
    suppressWarnings(summ(d))
  }))
  sink()

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
    pvalue.name <- c(pvalue.name, 'K-Wallis')

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

  texts <- paste0("Number Summary: ", x.name, "\nby categories of: ",
                  y.name, collapse = "")
  printText(f, texts)

  invisible(f)
}


#' @rdname summBy
#' @export
summBy.list <- function(x, y, data = NULL, rnd = 1, na.rm = FALSE)
{
  arguments <- as.list(match.call())
  y.name <- deparse(substitute(y))

  y <- eval(substitute(y), data)
  x.names <- as.character(arguments$x)[-1]
  x.name <- gsub(" ", "", x.name)
  data <- data[, x.names]

  sink(tempfile())
  f <- do.call(rbind,
               lapply(data, function(z) {
                 t <- rbind(summBy.numeric(z, y, na.rm = na.rm, rnd = rnd),
                            rep("", 10))
                 row.names(t)[nrow(t)] <- ""
                 t
               }))
  sink()

  texts <- paste0("Number Summary: ",
                  paste0(x.name, collapse = " | "),
                  "\nby categories of: ", y.name, collapse = "")
  printText(f, texts)

  x.lbl <- sapply(data, function(z) attr(z, "label"))
  y.lbl <- attr(y, "label")

  for (i in 1:length(x.names)) {
    if (!is.null(unlist(x.lbl[i]))) {
      printMsg("labels:")
      printMsg(paste0(x.name[i], ": ", x.lbl[i], collapse = ""))
    }
    if (!is.null(y.lbl))
      printMsg(paste0(y.name, ": ", y.lbl, collapse = ""))
  }

  invisible(f)
}


#' @rdname summBy
#' @export
summBy.data.frame <- function(x, y, data = NULL, rnd = 1, na.rm = FALSE)
{
  arguments <- as.list(match.call())
  y.name <- arguments$y
  y <- eval(substitute(y), x)
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
    f <- do.call(rbind, lapply(data, function(z) {
      t <- rbind(summBy.numeric(z, y, na.rm = na.rm, rnd = rnd),
                 rep("", 10))
      row.names(t)[nrow(t)] <- ""
      t }))
    x.lbl <- lapply(data, function(z) attr(z, "label"))
  } else {
    f <- summBy.numeric(data, y, na.rm = na.rm, rnd = rnd)
    row.names(f) <- vars.names
    x.lbl <- attr(data, "label")
  }
  sink()

  x.lbl <- sapply(data, function(z) attr(z, "label"))
  y.lbl <- attr(y, "label")

  texts <- paste0("Number Summary: ",
                  paste0(vars.names, collapse = " | "),
                  "\nby categories of: ", y.name, collapse = "")
  printText(f, texts, "by categories of: ")

  if (!all(is.null(unlist(x.lbl))))
    printMsg("labels: ")
  for (i in 1:length(x.lbl)) {
    if (!is.null(unlist(x.lbl[i]))) {
      printMsg(paste0(vars.names[i], ": ", x.lbl[i], collapse = ""))
    }
  }
  if (!is.null(y.lbl))
    printMsg(paste0(y.name, ": ", y.lbl, collapse = ""))

  invisible(f)
}
