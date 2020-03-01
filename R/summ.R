#' @title Number Summary for numerical data
#'
#' @description
#'
#' \code{summ()} generates summary statistics for numerical data
#'
#' @param data dataset (optional)
#' @param ... a variable or variables. Colon separator \code{:} can be
#' used for multiple variables. See details.
#' @param x exposure variable
#' @param by outcome variable for cross-tabulation
#' @param na.rm A logical value to specify missing values,
#' <NA> in the table
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#' @param print.table logical value to display formatted outputs
#'
#' @details
#'
#' \code{summ()} reports number of observations in the dataset,
#' missing data,
#' seven number summary statistics, coefficient of
#' variation (CV) and normality
#' test.
#'
#' Normality test is perfomed by Shapiro-Wilk Normality Test. See more at
#' \code{\link{shapiro.test}}.
#'
#' \strong{ANNOTATIONS}
#'
#' \code{Obs.} = observation
#'
#' \code{NA.} = missing data
#'
#' \code{Mean} = Mean value
#'
#' \code{Std.Dev} = Standard deviation
#'
#' \code{Median} = Median value
#'
#' \code{Q1} = First quartile or percentile
#'
#' \code{Q3} = Third quartile or percentile
#'
#' \code{Min} = Minimum value
#'
#' \code{Max} = Maximum value
#'
#' \code{Normality} = P-value from Shapiro-Wilk Normality Test
#'
#' @note
#'
#' In case of using the whole dataset for grouped summary measures,
#' the dataset must be \code{data.frame}.
#'
#' @seealso
#'
#' \code{\link{summBy}}, \code{\link{tab}}, \code{\link{xtab}}
#'
#' @import stats
#'
#' @keywords
#'
#' number summary, statistics, descriptive, five number
#'
#' @author
#'
#' For any feedback, please contact \code{Myo Minn Oo} via:
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#' \dontrun{
#' ## Using infert dataset
#' # one variable
#' summ(infert, age)
#'
#' # multiple variable
#' summ(infert, age, induced, case)
#'
#' # multiple variable using colon separator
#' summ(infert, age:case)
#' summ(infert, age, parity:pooled.stratum)
#'
#' # the whole dataset
#' summ(infert)
#' summ(iris)
#'
#' # without using data argument
#' induced <- infert$induced
#' case <- infert$case
#' parity <- infert$parity
#' summ(NULL, induced, case, parity)
#'
#'
#'
#' ## IDRE UCLA Example 1
#' path <- "https://stats.idre.ucla.edu/stat/data/patient_pt1_stata_dm.dta"
#' hosp <- haven::read_dta(path)
#' codebook(hosp)
#'
#' # one variable
#' summ(hosp, rbc)
#'
#' # multiple variables
#' summ(hosp, tumorsize:age, lengthofstay, rbc:test2)
#'
#' # whole dataset
#' summ(hosp)
#'
#'
#'
#'
#' ## IDRE UCLA Example 2
#' path <- "https://stats.idre.ucla.edu/stat/stata/modules/kids.dta"
#' kids <- haven::read_dta(path)
#'
#' # display codebook
#' codebook(kids)
#'
#' # one variable
#' summ(kids, age)
#'
#' # multiple variables
#' listView(kids, age, wt, birth)
#' summ(kids, age, wt, birth)
#'
#' # the whole dataset
#' summ(kids)
#'
#'
#'
#'
#'
#' ### GROUPED SUMMARY MEASURES
#' # using infert dataset
#' # one variable
#' summ(infert, age, by = education)
#'
#' # multiple variables
#' summ(infert, age, induced, case, by = education)
#' summ(infert, age:pooled.stratum, by = education)
#'
#' # the whole dataset
#' summ(infert, by = education)
#'
#'
#'
#'
#' ## IDRE UCLA Example 1
#' path <- "https://stats.idre.ucla.edu/stat/data/patient_pt1_stata_dm.dta"
#' hosp <- haven::read_dta(path)
#' codebook(hosp)
#'
#' # one variable
#' summ(hosp, rbc, by = cancerstage)
#'
#' # multiple variables
#' summ(hosp, tumorsize:age, lengthofstay, rbc:test2, by = cancerstage)
#'
#' # whole dataset
#' summ(hosp, by = cancerstage) ## Error
#'
#' ## change to data.frame and then summ
#' library(magrittr)
#' hosp %>% data.frame %>% summ(by = cancerstage)
#'
#'
#'
#' ## IDRE UCLA Example 2
#' path <- "https://stats.idre.ucla.edu/stat/stata/modules/kids.dta"
#' kids <- haven::read_dta(path)
#'
#' # display codebook
#' codebook(kids)
#'
#' # one variable
#' summ(kids, age, by = sex)
#'
#' # multiple variables
#' summ(kids, age, wt, by = sex)
#'
#' # the whole dataset
#' summ(kids, by = sex) # Error
#'
#' library(magrittr)
#' kids %>% data.frame %>% summ(by = sex)
#' }


#' @export
summ <- function(data = NULL, ... , by = NULL,
                 na.rm = FALSE, rnd = 1,
                 print.table = TRUE)
{
  arguments <- as.list(match.call())[-1]
  if (!is.null(data)) {
    by <- eval(substitute(by), data)
  }
  nonX <- c("data", "by", "row.pct", "na.rm", "rnd", "print.table")
  x <- as.character(arguments[!(names(arguments) %in% nonX)])

  if (length(x) > 1) {
    x <- list()
  } else if (length(x) == 1 ) {
    if (any(grepl(":", x))) {
      x <- list()
    } else {
      x <- numeric()
    }
  } else {
    x <- data.frame()
  }

  if (is.null(by)) {
    UseMethod("summ", x)
  } else {
    UseMethod("summBy", x)
  }
}


#' @rdname summ
#' @export
summ.default <- function( ... )
{
  stop(" >>> Data type is not suppored. <<< ")
}

#' @rdname summ
#' @export
summ.numeric <- function(data = NULL, x, by = NULL,
                         na.rm = FALSE, rnd = 1,
                         print.table = TRUE)
{
  arguments <- as.list(match.call())[-1]
  if (!is.null(data)) {
    x <- eval(substitute(x), data)
  }
  x.name <- arguments$x

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
  pvalue <- sprintf(pvalue, fmt = '%#.3f')

  f <- data.frame(Obs. = len, NA. = na, Mean = v[1], Std.Dev = v[2],
                  Median = v[6], Q1 = v[5], Q3 = v[7],
                  Min = v[4], Max = v[8],
                  Normality = pvalue,
                  stringsAsFactors = FALSE)
  row.names(f) <- x.name

  if (print.table) {
    texts <- paste("Number Summary: ", x.name, collapse = "")
    printText(f, texts)
    x.lbl <- attr(x, "label")
    if (!is.null(x.lbl)) {
      printMsg("Labels:")
      printMsg(paste0(x.name, ": ", x.lbl, collapse = ""))
    }
  }

  invisible(f)
}


#' @rdname summ
#' @export
summ.list <- function(data = NULL, ... , by = NULL,
                      na.rm = FALSE, rnd = 1,
                      print.table = TRUE)
{
  arguments <- as.list(match.call())[-1]
  nonX <- c("data", "by", "row.pct", "na.rm", "rnd", "print.table")
  x.names <- arguments[!names(arguments) %in% nonX]

  hasColon <- grepl(":", as.character(x.names))

  if (any(hasColon) & !is.null(data)) {
    data <- data.frame(data)
    x.names <- do.call(
      c,
      lapply(as.character(x.names), function(z) {
        hasColon <- grepl(":", z)
        if (hasColon) {
          varsColonSplit(data, z, hasColon)
        } else {
          z
        }
      })
    )
    x.names <- lapply(x.names, function(z) z)
  }

  if (!is.null(data)) {
    data <- data[, as.character(x.names)]
  } else {
    data <- do.call(
      cbind,
      lapply(x.names, function(z) {
        eval(z)
      })
    )
    data <- data.frame(data, stringsAsFactors = FALSE)
    names(data) <- x.names
  }

  # one way tabulation for multiple variables
  f <- do.call(rbind, lapply(data, function(z){
    summ.numeric(NULL, z, na.rm = na.rm, rnd = rnd,
                 print.table = FALSE) }))

  if (print.table) {
    texts <- paste0("Number Summary: ",
                    paste(x.names, collapse = ", "), collapse = "")
    printText(f, texts)

    x.names <- as.character(x.names)
    x.lbl <- (sapply(x.names, function(z)
      attr(data[, z], "label")))
    if (!is.null(unlist(x.lbl))) {
      printMsg(paste0("labels:"))
      for (i in 1:length(x.names)) {
        if (x.lbl[i] != "NULL") {
          printMsg(paste0(x.names[i], ": ", x.lbl[i],
                          collapse = ""))
        }
      }
    }
  }

  invisible(f)
}

#' @rdname summ
#' @export
summ.data.frame <- function(data = NULL, ... , by = NULL,
                            na.rm = FALSE, rnd = 1,
                            print.table = TRUE)
{
  vars <- names(data)
  type.numeric <- c("int", "double", "numeric")

  vars.type <- sapply(data, function(z)
    paste0(class(unlist(z)), collapse = ""))
  vars.names <- vars[(vars.type %in% type.numeric)]
  data <- data[, vars.names]

  if (is.data.frame(data)) {
    if (ncol(data) == 0)
      stop(" >>> no numeric variables found <<< ")

    names.invalid <- grep("^([[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*$",
                          vars.names, value = TRUE, invert = TRUE)
    if (length(names.invalid) > 0) {
      vars.names[vars.names %in% names.invalid] <- paste0("v", names.invalid)
      names(data) <- vars.names
    }
  }

  if (is.data.frame(data)) {
    f <- do.call(rbind, lapply(data, function(z){
      summ.numeric(NULL, z, na.rm = na.rm, rnd = rnd,
                   print.table = FALSE) }))
    x.lbl <- lapply(data, function(z) attr(z, "label"))
  } else {
    f <- summ.numeric(NULL, data, na.rm = na.rm, rnd = rnd,
                      print.table = FALSE)
    row.names(f) <- vars.names
    x.lbl <- attr(data, "label")
  }

  if (print.table) {
    if (is.data.frame(data)) {
      texts <- paste0("Number Summary: ",
                      paste(vars.names, collapse = ", "),
                      collapse = "")
      printText(f, texts)

      x.lbl <- (sapply(vars.names, function(z) attr(data[, z], "label")))
      if (!is.null(unlist(x.lbl))) {
        printMsg(paste0("labels:"))
        for (i in 1:length(vars.names)) {
          if (x.lbl[i] != "NULL") {
            printMsg(paste0(vars.names[i], ": ", x.lbl[i], collapse = ""))
          }
        }
      }

    } else {
      texts <- paste0("Number Summary: ", vars.names, collapse = "")
      printText(f, texts, "label:")
      if (x.lbl != "NULL") {
        printMsg(paste0(vars.names, ": ", x.lbl, collapse = ""))
      }

    }
  }

  invisible(f)
}
