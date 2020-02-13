#' @title Tabulation
#' @description
#' \code{tab} generates frequency distribution of a categorical variable.
#' @param x a character, factor or logical object OR list OR data frame
#' @param data a data frame object (Optional)
#' @param na.rm A logical value to specify missing values, <NA> in the table
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#' @param print.table logical value to display formatted outputs
#' @param ... optional arguments
#' @details
#'
#' \code{tab}
#'
#' produces one-way table of frequencies.
#' Exploring data before jumping into complex analysis is always a necessity.
#' The first step of an analysis is always to summarize and display data.
#'
#' If variables are specified, all data types are accepted, except for dataframe,
#' it tabulates all variables with three data types: 1) character, 2) factor, and
#' 3) logical.
#'
#' \strong{References:}
#' \enumerate{
#'   \item Essential Medical Statistics, Betty R. Kirkwood & Jonathan A.C. Sterne,
#'   Second Edition. Chapter 3
#'   \item An Introduction to MEdical Statistics, Martin Bland, Thrid Edition,
#'   Chapter 4
#' }
#'
#' @seealso \code{\link{xtab}}, \code{\link{summ}}, \code{\link{summBy}}
#' @keywords frequency distribution, tabulation, one-way table,
#' statistics, descriptive
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#' # vectors
#' x <- rep(c("M", "F"), c(40, 60))
#' tab(x)
#'
#' # factors
#' tab(infert$education)
#' tab(iris$Species)
#' tab(Species, iris)
#' tab(agegp, esoph)
#'
#' # numeric as categorical
#' tab(case, infert)
#' tab()
#'
#' # multiple variables of mixed types
#' tab(c(infert$case, infert$education, infert$induced))
#' tab(c(case, parity, induced), infert)
#'
#' # dataframe
#' tab(infert)
#' tab(iris)
#'
#' # errors: no categorical variables
#' tab(mtcars)
#' }


#' @export
tab <- function(x, data = NULL, na.rm = FALSE, rnd = 1,
                print.table = TRUE)
{
  arguments <- as.list(match.call())
  x.name <- (deparse(substitute(x)))
  x.name <- unlist(strsplit(gsub("^c\\(|\\)$", "", x.name), ","))

  catch <- tryCatch(is.data.frame(x), error=function(e) {})
  x <- as.character()
  if (is.null(catch)) catch <- FALSE
  if (catch) x <- data.frame()
  if (length(x.name) > 1) x <- list()
  UseMethod("tab", x)
}


#' @rdname tab
#' @export
tab.default <- function(...) {
  stop(' ... Wrong Data Type ... ')
}

#' @rdname tab
#' @export
tab.character <- function(x, data = NULL, na.rm = FALSE, rnd = 1,
                          print.table = TRUE)
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

  x.lbl <- attr(x, "label")
  x.lbl <- ifelse(is.null(x.lbl), "NULL", x.lbl)
  texts <- paste0("Tabulation: ", x.name, "\n",
                 "label: ", paste0(x.lbl), collapse = "")

  if (print.table) printText(f, texts, "label: ")
  invisible(f)
}


#' @rdname tab
#' @export
tab.list <- function(x, data = NULL, na.rm = FALSE, rnd = 1,
                     print.table = TRUE)
{
  arguments <- as.list(match.call())
  x.name <- deparse(substitute(x))
  x.name <- unlist(strsplit(gsub("^c\\(|\\)$", "", x.name), ","))
  x.name <- gsub(" ", "", x.name)

  data.list <- NULL
  if (is.null(data)) {
    data <- lapply(x.name, function(z) eval(parse(text = z)))
    names(data) <- x.name
  } else {
    data <- lapply(x.name, function(z)
      eval(parse(text = paste0("data$", z))))
    names(data) <- x.name
  }

  f <- lapply(data, function(z){
    tab.character(z, na.rm = na.rm, rnd = rnd, print.table = FALSE)
  })

  x.lbl <- lapply(data, function(z) attr(z, "label"))

  for (i in 1:length(x.name)) {
    x.lbl[i] <- ifelse(is.null(x.lbl[i]), "NULL", x.lbl[i])
    texts <- paste0("Tabulation: ", x.name[i], "\n",
                   "label: ", paste0(x.lbl[i]), collapse = "")

    if (print.table) printText(f[[i]], texts, "label: ")
  }

  invisible(f)
}

#' @rdname tab
#' @export
tab.data.frame <- function(x, data = NULL, na.rm = FALSE, rnd = 1,
                           print.table = TRUE)
{
  data <- x
  vars <- names(x)
  type.character <- c("factor", "character", "orderedfactor")
  type.logical <- c("logical")

  vars.type <- sapply(vars, function(z) paste0(class(unlist(x[ , z])), collapse = ""))
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
      tab.character(z, na.rm = na.rm, rnd = rnd, print.table = FALSE))
    x.lbl <- lapply(data, function(z) attr(z, "label"))
  } else {
    f <- tab.character(data, na.rm = na.rm, rnd = rnd, print.table = FALSE)
    x.lbl <- attr(data, "label")
  }

  if (is.data.frame(data)) {
    for (i in 1:length(vars.names)) {
      x.lbl[i] <- ifelse(is.null(x.lbl[i]), "NULL", x.lbl[i])
      texts <- paste0("Tabulation: ", vars.names[i], "\n",
                      "label: ", paste0(x.lbl[i]), collapse = "")

      if (print.table) printText(f[[i]], texts, "label: ")
    }
  } else {
    x.lbl <- ifelse(is.null(x.lbl), "NULL", x.lbl)
    texts <- paste0("Tabulation: ", vars.names, "\n",
                    "label: ", paste0(x.lbl), collapse = "")
    if (print.table) printText(f, texts, "label: ")
  }

  invisible(f)
}

