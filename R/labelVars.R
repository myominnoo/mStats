#' @title Manipulate labels: Label variables or dataframe
#' @description
#' \code{labelVars} attaches a label to variables or a dataframe
#' @param data a dataframe object (Optional)
#' @param vars vectors or list
#' @param lbl specify a string
#' @details
#'
#' Labels are displayed when you \code{\link{codebook}}
#' the dataframe. See the examples below.
#'
#' This function works only on data.frame type. Consider changing to this type
#' by using as.data.frame().
#'
#' @seealso \code{\link{codebook}}, \code{\link{labelData}}, \code{\link{labelValues}}
#' @keywords dataframe label, data frame, label
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#' # label variables
#' codebook(labelVars(infert, age, "AGE"))
#' codebook(labelVars(infert, c(age, parity), c("AGE", "PARITY")))
#'
#' # label dataframe
#' codebook(labelData(infert, "INFERT"))
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

  printMsg(paste0("Variable: labelled '", vars.name,
                  "' as '", lbl, "'"))
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
    printMsg(paste0("Variable: labelled '", vars.name[n],
                    "' as '", lbl[n], "'"))
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
  printMsg(paste0("Dataframe: labelled '", data.name,
                  "' as '", lbl, "'"))
  return(data)
}
