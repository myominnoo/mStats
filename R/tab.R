#' @title Tabulation
#'
#' @description
#'
#' \code{tab()} generates one-way or two-way tabulation of
#' a categorical variable.
#'
#' @param data dataset (optional)
#' @param ... a variable or variables. Colon separator \code{:} can be
#' used for multiple variables. See details.
#' @param x exposure variable
#' @param by outcome variable for cross-tabulation
#' @param row.pct TRUE or FALSE: If \code{TRUE}, row percentages
#' are shown and if \code{FALSE}, column percentages.
#' if \code{NULL}, a table without any row or column percentages
#' is produced.
#' @param na.rm A logical value to specify missing values,
#' <NA> in the table
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#' @param print.table logical value to display formatted outputs
#'
#' @details
#'
#' \code{tab()} produces one-way or \code{2x2} tables.
#'
#' \strong{One-way tabulation}
#'
#' If \code{by} is not specified, \code{tab} generates one-way tabulation of
#' a variable or multiple variables. There is no argument for \code{x}.
#' Instead, \code{...} accepts multiple variables and produces
#' corresponding tabulations.
#'
#' Tabulation is displayed in \code{Freq.} (frequency), \code{Percent.}
#' (Relative Frequency) and \code{Cum.Percent.} (Cumulative frequency).
#'
#'
#' \strong{Data type}
#'
#' For tabulating the whole dataset, variables with either of
#' four data types (character,
#' factor, ordered factor and logical) are tabulated.
#'
#' \preformatted{tab(data)}
#'
#' For tabulating
#' variables with other data types, variables have to be called separately.
#'
#' \preformatted{tab(data, var1, var2, etc)}
#'
#'
#' \strong{Multiple variables}
#'
#' \code{tab()} accepts multiple variables. They can be put after
#' \code{data} arguments as many as possible.
#' Colon separator \code{:} can be used to indicate sequence of variables.
#'
#' \preformatted{tab(data, var1, var2, var3:var6, var7, var10:var15)}
#'
#' \strong{Two-way or \code{2x2} tabulation}
#'
#' \code{2x2} tables can also be produced by \code{\link{xtab}}. This is
#' called explicit calling of \code{xtab}. Otherwise, the same tabulation
#' can be implicitly generated in \code{tab} function by specifying
#' outcome variable, \code{by}.
#'
#' See details in \code{\link{xtab}}.
#'
#'
#' @return
#'
#' \enumerate{
#'    \item \code{vector} or \code{list}: corresponding unformatted table(s)
#'    \item output: formatted texts
#' }
#'
#' @references
#'
#' \enumerate{
#'   \item Essential Medical Statistics, Betty R. Kirkwood & Jonathan
#'   A.C. Sterne,
#'   Second Edition.
#'   \item An Introduction to MEdical Statistics, Martin Bland,
#'   Thrid Edition,
#'   \item STATA DATA MANAGEMENT. UCLA: Statistical Consulting Group.
#'    from https://stats.idre.ucla.edu/stata/seminars/stata-data-management/
#'    (accessed Febrary 25, 2020).
#' }
#'
#' @seealso
#'
#' \code{\link{xtab}}, \code{\link{summ}}
#'
#' @keywords frequency distribution, tabulation, one-way table,
#' statistics, descriptive
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
#' tab(infert, education)
#'
#' # multiple variable
#' tab(infert, education, parity, case)
#'
#' # multiple variable using colon separator
#' tab(infert, case:parity)
#' tab(infert, education, case:parity, age:case)
#'
#' # the whole dataset
#' tab(infert)
#' tab(esoph)
#'
#' # without using data argument
#' education <- infert$education
#' case <- infert$case
#' parity <- infert$parity
#' tab(NULL, education, case, parity)
#'
#'
#'
#' ## IDRE UCLA Example 1
#' path <- "https://stats.idre.ucla.edu/stat/data/patient_pt1_stata_dm.dta"
#' hosp <- haven::read_dta(path)
#' codebook(hosp)
#'
#' # one variable
#' tab(hosp, sex)
#'
#' # multiple variables
#' tab(hosp, hospital, wound, remission, married:lengthofstay)
#'
#' # whole dataset
#' tab(hosp)
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
#' tab(kids, famid)
#'
#' # multiple variables
#' tab(kids, famid, birth:sex)
#'
#' # the whole dataset
#' tab(kids)
#'
#'
#'
#'
#' ## Cross-tabulation
#' ## using infert dataset
#' # one variable
#' tab(infert, education, by = case)
#'
#' # multiple variables
#' tab(infert, education, age:induced, spontaneous, by = case)
#'
#' # the whole dataset
#' tab(infert, by = case)
#'
#'
#'
#'
#' ## using hosp dataset
#' # one variable
#' tab(hosp, sex, by = remission)
#'
#' # multiple variables
#' tab(hosp, hospital, married:lengthofstay, by = remission)
#'
#' # the whole dataset
#' tab(hosp, by = remission)
#' }


#' @export
tab <- function(data = NULL, ... , by = NULL,
                row.pct = TRUE,
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
      x <- character()
    }
  } else {
    x <- data.frame()
  }

  if (is.null(by)) {
    UseMethod("tab", x)
  } else {
    UseMethod("xtab", x)
  }
}



#' @rdname tab
#' @export
tab.default <- function( ... )
{
  stop(" >>> Data type is not suppored. <<< ")
}

#' @rdname tab
#' @export
tab.character <- function(data = NULL, x, by = NULL,
                          row.pct = TRUE,
                          na.rm = FALSE, rnd = 1,
                          print.table = TRUE)
{
  arguments <- as.list(match.call())[-1]
  if (!is.null(data)) {
    x <- eval(substitute(x), data)
  }
  x.name <- arguments$x
  na.rm <- ifelse(na.rm, "no", "ifany")

  t <- table(x, useNA = na.rm)
  t.cnt <- c(t, Total = sum(t))
  t.cum <- c(cumsum(t), sum(t))
  f <- cbind(Freq. = t.cnt,
             Percent. = round(t.cnt / sum(t) * 100, rnd),
             Cum.Percent. = round(t.cum / sum(t) * 100, rnd))
  names(attributes(f)$dimnames) <- c(x.name, "")
  f <- data.frame(f)

  if (print.table) {
    # restructure label of x
    if (any(grepl("\\$", x.name))) {
      v <- as.character(x.name)
      x.name <- paste0(x.name[2], x.name[1], x.name[3], collapse = "")
    }
    texts <- paste0("Tabulation: ", paste0(x.name), collapse = "")
    printText(f, texts)
    if (!is.null(attr(x, "label"))) {
      printMsg("Labels:")
      printMsg(paste0(x.name, ": ", attr(x, "label"), collapse = ""))
    }
  }
  invisible(f)
}


#' @rdname tab
#' @export
tab.list <- function(data = NULL, ... , by = NULL,
                     row.pct = TRUE,
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
  f <- lapply(names(data), function(z) {
    t <- data[, z]
    t <- tab.character(NULL, t, na.rm = na.rm, rnd = rnd,
                       print.table = FALSE)
  })

  if (print.table) {
    x.lbl <- sapply(data, function(z) attr(z, "label"))
    for (i in 1:length(x.names)) {
      texts <- paste0("Tabulation: ", x.names[i], collapse = "")
      printText(f[[i]], texts)
      if (!is.null(unlist(x.lbl[i]))) {
        printMsg("Labels:")
        printMsg(paste0(x.names[i], ": ", x.lbl[i], collapse = ""))
      }
    }
  }
  invisible(f)
}

#' @rdname tab
#' @export
tab.data.frame <- function(data = NULL, ... , by = NULL,
                           row.pct = TRUE,
                           na.rm = FALSE, rnd = 1,
                           print.table = TRUE)
{
  vars <- names(data)
  type.character <- c("factor", "character", "orderedfactor")
  type.logical <- c("logical")

  vars.type <- sapply(vars, function(z)
    paste0(class(unlist(data[ , z])), collapse = ""))
  vars.names <- vars[(vars.type %in% type.character) |
                       (vars.type %in% type.logical)]
  data <- data[, vars.names]

  if (is.data.frame(data)) {
    if (ncol(data) == 0)
      stop(" >>> no categorical variables found <<< ")

    names.invalid <- grep("^([[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*$",
                          vars.names, value = TRUE, invert = TRUE)
    if (length(names.invalid) > 0) {
      vars.names[vars.names %in% names.invalid] <- paste0("v", names.invalid)
      names(data) <- vars.names
    }
  }

  if (is.data.frame(data)) {
    f <- lapply(data, function(z)
      tab.character(NULL, z, na.rm = na.rm, rnd = rnd,
                    print.table = FALSE))
    x.lbl <- sapply(data, function(z) attr(z, "label"))
  } else {
    f <- tab.character(NULL, data, na.rm = na.rm, rnd = rnd,
                       print.table = FALSE)
    x.lbl <- attr(data, "label")
  }

  if (is.data.frame(data)) {
    if (print.table) {
      for (i in 1:length(vars.names)) {
        texts <- paste0("Tabulation: ", vars.names[i], collapse = "")
        printText(f[[i]], texts)
        if (!is.null(unlist(x.lbl[i]))) {
          printMsg("Labels:")
          printMsg(paste0(vars.names[i], ": ", x.lbl[i], collapse = ""))
        }
      }
    }
  } else {
    if (print.table) {
      texts <- paste0("Tabulation: ", vars.names, collapse = "")
      printText(f, texts)
      if (!is.null(x.lbl)) {
        printMsg("Labels:")
        printMsg(paste0(vars.names, ": ", x.lbl, collapse = ""))
      }
    }
  }

  invisible(f)
}
