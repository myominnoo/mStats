#' @title Manipulate labels
#' @description
#' \code{labelValues} attach value labels to variables
#' @param var vectors or lists
#' @param lbl specify a string
#' @param data a dataframe object (Optional)
#' @param useNA force to use NA as level, if any
#' @details
#'
#' The order of the labels appearing is the same as output from
#' \code{\link{tab}} function.
#'
#' If data is specified, it returns the whole dataframe with recoded variables.
#' Otherwise, only one vector can be used to label values.
#'
#' @seealso \code{\link{labelVars}}, \code{\link{labelData}}
#' @keywords value label, levels, label
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#' str(infert)
#' infert.new <- labelValues(infert, case, c("no", "yes"))
#' table(infert.new$case)
#'
#' infert.new <- labelValues(infert,
#'             list(case, induced),
#'             list(c("no", "yes"), c("none", "one", "two")))
#' table(infert.new$case)
#' table(infert.new$induced)
#'
#' summary(labelValues(infert, education, c("low", "mid", "high")))
#' summary(labelValues(infert,
#'                 list(education, induced),
#'                 list(c("low", "mid", "high"),
#'                      c("none", "one", "two"))))
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
    printMsg(paste0("Values: labelled in '", var.name, "' | '",
                    lvl[i], "' => '", lbl[i], "'"))
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
      printMsg(paste0("Values: labelled in '", var.name[i], "' | '",
                      lvl[j], "' => '", lbl.unlist[j], "'"))
    }
  }
  return(data)
}
