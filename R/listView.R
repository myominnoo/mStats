#' @title List and view observations of \code{data.frame} in R Console
#'
#' @description
#' \code{listView()} displays a list of observations of specified or all
#' variables in R Console.
#'
#' @param data a data frame object
#' @param ... Names of variables to be subsetted.
#'
#' @details
#'
#' If \code{data} is specified without any variables, the whole dataset
#' will be displayed using \code{print.data.frame}. If variables are
#' specified, a corresponding subset will be printed in R Console.
#'
#' If specified variables contain duplicated variables, they will all be
#' printed with numbered suffix. Example, if \code{age} is duplicated
#' three times, then \code{age.1},  \code{age.2}, and
#'  \code{age.3} are printed in corresponding order.
#'
#' Maximum number is set at \code{1e5}.
#'
#' @references
#'
#' STATA DATA MANAGEMENT. UCLA: Statistical Consulting Group.
#' from https://stats.idre.ucla.edu/stata/seminars/stata-data-management/
#' (accessed Febrary 25, 2020).
#'
#' @import utils
#'
#' @seealso \code{\link{codebook}}
#'
#' @keywords list, display, view, head, tail
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
#' # list infert dataset
#' listView(infert, education)
#'
#' listView(infert, age, case, stratum)
#' listView(infert, education:stratum)
#'
#' # list several columns in different order
#' listView(infert, education, induced:pooled.stratum)
#' listView(infert, age, education, case:age, age:parity)
#'
#' # list the whole dataset
#' listView(infert)
#'
#'
#'
#' # Example from IDRE UCLA
#' path <- "https://stats.idre.ucla.edu/stat/data/patient_pt1_stata_dm.dta"
#' hosp <- haven::read_dta(path)
#' listView(hosp, hospital:pain)
#' }


#' @export
listView <- function(data, ... )
{
    arguments <- as.list(match.call())
    vars <- arguments[c(-1, -2)]

    if (!is.data.frame(data)) {
        x <- vector()
    } else {
        if (length(vars) == 0) {
            x <- data.frame()
        } else if (length(vars) == 1) {
            if (any(grepl(":", vars))) {
                x <- list()
            } else {
                x <- character()
            }
        } else {
            x <- list()
        }
    }
    UseMethod("listView", x)
}


#' @rdname listView
#' @export
listView.default <- function(...)
{
    printWarning("Try listView(infert)")
}


#' @rdname listView
#' @export
listView.character <- function(data, ... )
{
    arguments <- as.list(match.call())
    vars <- as.character(arguments[c(-1, -2)])
    data <- data.frame(data[, vars])
    names(data) <- vars
    print.data.frame(data, max = 1e5)
}


#' @param vars variable's names
#' @param hasColon Logical indicating where colon is present or not
#' @rdname listView
#' @export
varsColonSplit <- function(data, vars, hasColon)
{
    vars <- unlist(strsplit(vars[hasColon], split = ":"))
    vars <- paste0("^", vars, "$")
    vars <- names(data)[grep(vars[1], names(data)):
                            grep(vars[2], names(data))]
    return(vars)
}


#' @rdname listView
#' @export
listView.list <- function(data, ... )
{
    arguments <- as.list(match.call())
    vars <- as.character(arguments[c(-1, -2)])
    nvars <- length(vars)
    vars <- as.character(vars)
    data <- data.frame(data)

    hasColon <- grepl(":", vars)
    if (any(hasColon)) {
        vars <- do.call(
            c,
            lapply(vars, function(z) {
                hasColon <- grepl(":", z)
                if (hasColon) {
                    varsColonSplit(data, z, hasColon)
                } else {
                    z
                }
            })
        )
        data <- data[, vars]
    } else {
        data <- data[, vars]
    }
    print.data.frame(data, max = 1e5)
}


#' @rdname listView
#' @export
listView.data.frame <- function(data, ... )
{
    data <- data.frame(data)
    print.data.frame(data, max = 1e5)
}
