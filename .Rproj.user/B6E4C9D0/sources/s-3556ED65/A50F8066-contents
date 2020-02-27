#' @title Manipulate labels of a variable or variables
#'
#' @description
#' \code{labelValues()} attach value labels to variables and
#' convert to factor
#'
#' @param data dataset (optional)
#' @param var vectors or lists
#' @param lbl specify a string
#' @param useNA force to use NA as level, if any
#' @details
#'
#' \code{labelValues()} basically recodes the values of variables
#' as factor. Hence, factored variables can also be used.
#'
#' \strong{Order of labels}
#'
#' The order of the labels appearing is the same as the output from
#' \code{\link{tab}} or \code{table} function.
#'
#' \strong{Multiple variables and labels}
#'
#' Multiple variables and labels must be specified as \code{list()}.
#'
#' \preformatted{
#' labelValues(data,
#'             list(var1, var2, var3),
#'             list(c(lbl1, lbl2, lbl3),
#'                  c(lbl1, lbl2, lbl3),
#'                  c(lbl1, lbl2, lbl3)))
#' }
#'
#'
#' @return
#'
#' \code{data.frame} if \code{data} is specified.
#'
#' \code{vector} if \code{data} is not specified.
#'
#' @seealso
#'
#' \code{\link{labelVars}}, \code{\link{labelData}}
#'
#' @keywords
#'
#' value label, change levels, label
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
#' # one variable
#' infert %>% tab(case)
#' infert %>%
#'     labelValues(case, c("no", "yes")) %>%
#'     tab(case)
#'
#' # multiple variables
#' infert %>% tab(induced)
#' labelValues(infert,
#'             list(case, induced),
#'             list(c("no", "yes"), c("none", "one", "two"))) %>%
#' tab(case, by = induced)
#'
#'
#'
#'
#' ## labelling factored variables
#' infert %>% tab(education)
#' infert %>%
#'     labelValues(education, c("low", "mid", "high")) %>%
#'     tab(education)
#'
#' infert %>% tab(education, by = induced)
#' infert %>%
#'     labelValues(list(education, induced),
#'                 list(c("low", "mid", "high"),
#'                      c("none", "one", "two"))) %>%
#'     tab(education, by = induced)
#'
#'
#'
#'
#' # vector
#' case.new <- infert$case
#' labelValues(NULL, case.new, c("no", "yes"))
#' }

#' @export
labelValues <- function(data = NULL, var, lbl, useNA = FALSE)
{
  arguments <- as.list(match.call())
  if (is.null(data)) {
    if (is.list(var))
      stop("... use only one variable at a time ...")
  } else {
    var <- eval(substitute(var), data)
    if (as.character(arguments$var)[1] %in% c("list"))
      var <- list(as.character(arguments$var)[-1])
  }
  UseMethod("labelValues", var)
}


#' @rdname labelValues
#' @export
labelValues.default <- function(data = NULL, var, lbl, useNA = FALSE)
{
  arguments <- as.list(match.call())
  var.name <- deparse(substitute(var))

  if (!is.null(data)) {
    var <- eval(substitute(var), data)
  }

  lvl <- names(table(var, useNA = "ifany"))
  if ((any(is.na(lvl)) & (length(lvl) == length(lbl))) | useNA)
  {exclude <- NULL} else {
    exclude <- NA
    lvl <- lvl[!is.na(lvl)]
  }

  f <- factor(var, labels = lbl, exclude = exclude, ordered = FALSE)

  for (i in 1:length(lvl)) {
    printMsg(paste0("Variable '", var.name, "': value '", lvl[i],
                    "' labelled as '", lbl[i], "'"))
  }

  if (!is.null(data)) {
    data[, var.name] <- f
    f <- data
  }
  return(f)
}


#' @rdname labelValues
#' @export
labelValues.list <- function(data = NULL, var, lbl, useNA = FALSE)
{
  arguments <- as.list(match.call())
  var.name <- as.character(arguments$var)[-1]
  if (length(lbl) != length(var.name))
    stop("... var and lbl must have the same length ...")

  for (i in 1:length(var.name)) {
    var <- data[, var.name[i]]
    lvl <- names(table(var, useNA = "ifany"))
    if ((any(is.na(lvl)) & (length(lvl) == length(lbl))) | useNA)
    {exclude <- NULL} else {
      exclude <- NA
      lvl <- lvl[!is.na(lvl)]
    }
    data[, var.name[i]] <- factor(var, labels = unlist(lbl[i]),
                                  exclude = exclude, ordered = FALSE)
    lbl.unlist <- unlist(lbl[i])
    for (j in 1:length(lbl.unlist)) {
      printMsg(paste0("Variable '", var.name[i], "': value '",
                      lvl[j], "' labelled as '", lbl.unlist[j], "'"))
    }
  }
  return(data)
}
