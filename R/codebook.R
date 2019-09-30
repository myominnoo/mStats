#' @title Codebook
#' @description
#' \code{codebook} displays structure of a data frame
#' @param x dataframe
#' @details
#' \code{codebook}
#'
#' generates the report of data structure with names, data lables, types,
#' number of observations, number of observations with missing values and
#' percentage of observations with missing values.
#'
#' ANNOTATIONS:
#'
#' VARS_NAME - Names of variables
#' LABEL     - Labels of variables
#' TYPE      - Types of variables
#' OBS_COUNT - Counts of valid observations
#' NA_COUNT  - Counts of observations with missing value
#' NA_(%)    - Percentage of observations with missing value
#'
#' @seealso \code{\link{ilog}}
#' @keywords codebook, summary, structure, layout
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#' codebook(infert)
#' codebook(iris)
#'
#' # if something else
#' codebook(iris$Species)
#' }

#' @export
codebook <- function(x) {
  UseMethod("codebook")
}


#' @rdname codebook
#' @export
codebook.default <- function(x) {
  printMsg("... Try codebook(iris) ...")
  f <- summary(x)
  print(f)
  invisible(f)
}

#' @rdname codebook
#' @export
codebook.data.frame <- function(x) {
  vars <- names(x)
  type.numeric <- c("integer", "double", "numeric")
  type.factor <- c("factor", "character")
  type.logical <- c("logical")
  type.date <- c("Date")

  vars.type <- sapply(vars, function(z) class(unlist(x[ , z])))
  vars.lbl <- paste(sapply(vars, function(z) {
    lbl.attr <- attr(x[[z]], "label")
    lbl.attr <- ifelse(is.null(lbl.attr), "NULL", lbl.attr)
    lbl.attr <- strtrim(lbl.attr, 40)
  }))

  na.counts <- sapply(vars, function(z)
    sum(as.numeric(is.na(x[, z])), na.rm = TRUE))
  obs.counts <- sapply(vars, function(z)
    sum(as.numeric(!is.na(x[, z])), na.rm = TRUE))
  obs.nrow <- nrow(x)

  f <- data.frame(names(vars.type), vars.lbl, vars.type, obs.counts, na.counts,
                  paste(round(na.counts / nrow(x) * 100, 1), "%"),
                  row.names = NULL)
  names(f) <- c("VARS_NAME", "LABEL", "DATA_TYPE",
                "OBS_COUNT", "NA_COUNT", "NA_(%)")

  x.lbl <- attr(x, "label")
  x.lbl <- ifelse(is.null(x.lbl), "NULL", x.lbl)
  texts <- paste("Codebook: ", deparse(substitute(x)), "\n",
                 "   label: ", x.lbl, sep = "", collapse = "")

  printText(f, texts)
  invisible(f)
}
