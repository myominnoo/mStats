#' @title Manipulate labels
#' @description
#' \code{labelValue} attach value labels to variables
#' @param x a variable or a list or a dataframe
#' @param data a dataframe object (Optional)
#' @param lbl specify a string
#' @param useNA force to use NA as level, if any
#' @details
#'
#' The order of the labels appearing is the same as output from
#' \code{\link{tab}} function.
#'
#' If data is specified, it returns the whole dataframe with recoded variables.
#'
#' @seealso \code{\link{labelVars}}, \code{\link{labelData}}
#' @keywords value label, levels, label
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#' str(infert)
#' # one variable at a time
#' case <- labelValue(case, infert)
#' str(case)
#' case <- labelValue(case, infert, c("no", "yes"))
#' str(case)
#' case <- labelValue(infert$case)
#' str(case)
#'
#' multiple variables
#' infert1 <- labelValue(list(case, induced), infert,
#'              list(c("no", "yes"), c("none", "1", "2")))
#' str(infert1)
#'
#' # for data frame, try labelData()
#' labelValue(infert)
#' }

#' @export
labelValue <- function(x, data = NULL, lbl = NULL, useNA = FALSE)
{
  arguments <- as.list(match.call())
  if (!is.null(data)) {
    x <- eval(substitute(x), data)
    if (as.character(arguments$x)[1] %in% c("list"))
      x <- list(as.character(arguments$x)[-1])
  }
  UseMethod("labelValue", x)
}

#' @rdname labelValue
#' @export
labelValue.default <- function(x, data = NULL, lbl = NULL, useNA = FALSE)
{
  arguments <- as.list(match.call())
  x.name <- deparse(substitute(x))
  if (!is.null(data)) {
    x <- eval(substitute(x), data)
  }

  lvl <- names(table(x, useNA = "ifany"))
  if ((any(is.na(lvl)) & (length(lvl) == length(lbl))) | useNA)
  {exclude <- NULL} else {
    exclude <- NA
    lvl <- lvl[!is.na(lvl)]
  }
  if (is.null(lbl)) lbl <- lvl

  f <- factor(x, labels = lbl, exclude = exclude, ordered = FALSE)

  cat(paste0("\tValues: labelled in '", x.name, "' as ",
             paste(lbl, collapse = " "), " ... \n"))
  invisible(f)
}


#' @rdname labelValue
#' @export
labelValue.list <- function(x, data = NULL, lbl = NULL, useNA = FALSE)
{
  arguments <- as.list(match.call())

  if (!is.null(data)) {
    vn <- as.character(arguments$x)[-1]
  } else {
    vn <- as.character(arguments$x)[-1]
    data <- data.frame(x)
    names(data) <- vn
  }

  if (!is.null(lbl)) {
    lbl <- (lbl)
  }

  for (i in 1:length(vn)) {
    sink(tempfile())
    data[, vn[i]] <- labelValue(data[, vn[i]], lbl = unlist(lbl[i]), useNA = useNA)
    sink()
    if (is.null(lbl)) {
      cat(paste0("\tValues: labelled in '", vn[i], "' ... \n"))
    } else {
      cat(paste0("\tValues: labelled in '", vn[i], "' as ",
                 paste(unlist(lbl[i]), collapse = " "), " ... \n"))
    }
  }

  invisible(data)
}


#' @rdname labelValue
#' @export
labelValue.data.frame <- function(x, data = NULL, lbl = NULL, useNA = FALSE)
{
  warning(paste0(" ... Value labels cannot be used in data frame ... \n",
                 "   ... Try labelData() ... "))
}

