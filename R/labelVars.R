#' @title Manipulate labels: Label variables or dataframe
#'
#' @description
#'
#' \code{labelVars} names \code{variables}.
#'
#' \code{labelData} names \code{Dataset}.
#'
#' @param data dataset
#' @param vars one variable or variables
#' @param lbl label or labels
#'
#' @details
#'
#' Labels are useful to provide more detailed information about
#' variables or dataset. Many functions in \code{mStats} package
#' extract label information and display them as footnote.
#'
#' \strong{single or multiple variables}
#'
#' Single or multiple variables can be labelled.
#'
#' Example: single variable \preformatted{labelVars(data, var, lble)}
#'
#' Example: multiple variable
#'
#' \preformatted{labelVars(data,
#'       c(var1, var2, var3),
#'       c(lbl1, lbl2, lbl3))}
#'
#' \strong{Labelling Dataset}
#'
#' Dataset can also be labelled and displayed in \code{codebook}.
#'
#' \preformatted{labelData(data, lbl)}
#'
#' @seealso
#'
#' \code{\link{codebook}}, \code{\link{labelData}},
#'
#' \code{\link{labelValues}}
#'
#' @keywords
#'
#' data.frame label, data label, data frame, label
#'
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
#' # piping function from magrittr package
#' library(magrittr)
#'
#' # label variables
#' infert %>%
#'     labelVars(age, "AGE OF PATIENT") %>%
#'     codebook()
#'
#' infert %>%
#'     labelVars(c(age, parity, induced, case),
#'               c("Patient's AGE", "No of pregancy",
#'                 "Induced Abortion", "case: Yes or no")) %>%
#'     codebook()
#'
#' # label dataframe
#' infert %>%
#'     labelData("Infertility after Spontaneous and Induced Abortion") %>%
#'     codebook()
#' }


#' @export
labelVars <- function(data = NULL, vars, lbl)
{
  arguments <- as.list(match.call())
  vars.x <- as.character(arguments$vars)
  catch <- tryCatch((vars), error=function(e) {})

  if (is.null(catch)) {
    if (length(vars.x) > 1)
      vars <- list(vars.x) else
        vars <- as.character(vars.x)
  } else {
    if (length(vars.x) > 1)
      stop("... use only one variable at a time ...") else
        vars <- as.character(vars.x)
  }
  UseMethod("labelVars", vars)
}


#' @rdname labelVars
#' @export
labelVars.default <- function(data = NULL, vars, lbl)
{
  stop("... Wrong data type ...")
}


#' @rdname labelVars
#' @export
labelVars.character <- function(data = NULL, vars, lbl)
{
  arguments <- as.list(match.call())
  vars.name <- deparse(substitute(vars))

  if (is.null(data)) {
    attr(vars, "label") <- lbl
    f <- vars
  } else {
    attr(data[, vars.name], "label") <- lbl
    f <- data
  }

  printMsg(paste0("Variable '", vars.name,
                  "' labelled  as '", lbl, "'"))
  return(f)
}


#' @rdname labelVars
#' @export
labelVars.list <- function(data = NULL, vars, lbl)
{
  arguments <- as.list(match.call())
  lbl <- as.character(arguments$lbl)[-1]

  vars.name <- as.character(arguments$vars)[-1]

  if (length(lbl) != length(vars.name))
    stop("... vars and lbl must have the same length ...")

  for (n in 1:length(vars.name)) {
    attr(data[, vars.name[n]], "label") <- lbl[n]
    printMsg(paste0("Variable '", vars.name[n],
                    "' labelled as '", lbl[n], "'"))
  }

  return(data)
}


#' @rdname labelVars
#' @export
labelData <- function(data, lbl)
{
  if (!is.data.frame(data)) stop(" ... data must be a dataframe ... ")
  data.name <- deparse(substitute(data))
  attr(data, "label") <- lbl
  printMsg(paste0("Dataset '", data.name,
                  "' labelled as '", lbl, "'"))
  return(data)
}
