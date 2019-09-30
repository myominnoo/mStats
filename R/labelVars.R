#' @title Manipulate labels: Label variables or dataframe
#' @description
#' \code{labelVars} attaches a label to variables or a dataframe
#' @param var vectors or list
#' @param lbl specify a string
#' @param data a dataframe object (Optional)
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
labelVars <- function(data = NULL, var, lbl)
{
  arguments <- as.list(match.call())
  var.x <- as.character(arguments$var)
  catch <- tryCatch((var), error=function(e) {})

  if (is.null(catch)) {
    if (length(var.x) > 1)
      var <- list(var.x) else
        var <- as.character(var.x)
  } else {
    if (length(var.x) > 1)
      stop("... use only one variable at a time ...") else
        var <- as.character(var.x)
  }
  UseMethod("labelVars", var)
}


#' @rdname labelVars
#' @export
labelVars.default <- function(data = NULL, var, lbl)
{
  stop("... Wrong data type ...")
}


#' @rdname labelVars
#' @export
labelVars.character <- function(data = NULL, var, lbl)
{
  arguments <- as.list(match.call())
  var.name <- deparse(substitute(var))

  if (is.null(data)) {
    attr(var, "label") <- lbl
    f <- var
  } else {
    attr(data[, var.name], "label") <- lbl
    f <- data
  }

  printMsg(paste0("Variable: labelled '", var.name,
                  "' as '", lbl, "'"))
  return(f)
}


#' @rdname labelVars
#' @export
labelVars.list <- function(data = NULL, var, lbl)
{
  arguments <- as.list(match.call())
  lbl <- as.character(arguments$lbl)[-1]

  var.name <- as.character(arguments$var)[-1]

  if (length(lbl) != length(var.name))
    stop("... var and lbl must have the same length ...")

  for (n in 1:length(var.name)) {
    attr(data[, var.name[n]], "label") <- lbl[n]
    printMsg(paste0("Variable: labelled '", var.name[n],
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
