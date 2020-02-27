#' @title Codebook: Detailed information about variables
#'
#' @description
#' \code{codebook()} provides detailed information about each variable
#' within the dataset.
#'
#' @param data dataframe
#'
#' @details
#' \code{codebook} generates the report of data structure with names,
#' data lables, types,
#' number of observations, number of observations with missing values and
#' percentage of observations with missing values.
#'
#' \strong{ANNOTATIONS}:
#'
#' VARS_NAME - Names of variables
#'
#' LABEL     - Labels of variables
#'
#' TYPE      - Types of
#'
#' OBS_COUNT - Counts of valid observations
#'
#' NA_COUNT  - Counts of observations with missing value
#'
#' NA_(\%)    - Percentage of observations with missing value
#'
#' @references
#'
#' STATA DATA MANAGEMENT. UCLA: Statistical Consulting Group.
#' from https://stats.idre.ucla.edu/stata/seminars/stata-data-management/
#' (accessed Febrary 25, 2020).
#'
#' @seealso
#'
#' \code{\link{ilog}}, \code{\link{listView}}
#'
#' @keywords codebook, summary, structure, layout
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
#' codebook(infert)
#' codebook(iris)
#'
#' # if something else
#' codebook(iris$Species)
#'
#'
#'
#' # Example from IDRE UCLA
#' path <- "https://stats.idre.ucla.edu/stat/data/patient_pt1_stata_dm.dta"
#' hosp <- haven::read_dta(path)
#' codebook(hosp)
#'
#' library(magrittr)
#' hosp %>% labelData("Hospital Dataset Example") %>% codebook()
#' }

#' @export
codebook <- function(data)
{
  UseMethod("codebook")
}


#' @rdname codebook
#' @export
codebook.default <- function(data)
{
  printWarning("Try codebook(iris)")
}

#' @rdname codebook
#' @export
codebook.data.frame <- function(data)
{
  vars <- names(data)
  type.numeric <- c("integer", "double", "numeric")
  type.factor <- c("factor", "character")
  type.logical <- c("logical")
  type.date <- c("Date")

  vars.type <- sapply(data, function(z) class(unlist(z)))
  vars.lbl <- paste(sapply(vars, function(z) {
    lbl.attr <- attr(data[[z]], "label")
    lbl.attr <- ifelse(is.null(lbl.attr), "<NA>", lbl.attr)
    lbl.attr <- strtrim(lbl.attr, 40)
  }))

  na.counts <- sapply(vars, function(z)
    sum(as.numeric(is.na(data[, z])), na.rm = TRUE))
  obs.counts <- sapply(vars, function(z)
    sum(as.numeric(!is.na(data[, z])), na.rm = TRUE))
  obs.nrow <- nrow(data)

  f <- data.frame(names(vars.type), vars.lbl, vars.type, obs.counts,
                  na.counts,
                  paste(round(na.counts / nrow(data) * 100, 1), "%"),
                  row.names = NULL)
  names(f) <- c("VARS_NAME", "LABEL", "DATA_TYPE",
                "OBS_COUNT", "NA_COUNT", "NA_(%)")

  texts <- paste("Codebook: ", deparse(substitute(data)),
                 collapse = "")
  printText(f, texts)
  data.lbl <- attr(data, "label")
  if (!is.null(data.lbl)) {
    printMsg("Dataset label:")
    printMsg(data.lbl)
  }

  invisible(f)
}
