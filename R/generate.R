#' @title Create a new variable
#'
#' @description
#' \code{generate()} creates a new variable within the data frame
#'
#' @param data Dataset
#' @param var New variable
#' @param .expr value or Expression for simple arithmetic or logical operations:
#' See examples.
#'
#' @details
#'
#' If the data is specified, it returns modified dataset with
#' newly created variable.
#' The value of the variable are specified by \code{expr} argument.
#'
#'
#' @concept new variable generate produce create transform
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
#'
#'
#' # Example from IDRE UCLA
#' path <- "https://stats.idre.ucla.edu/stat/data/patient_pt1_stata_dm.dta"
#' hosp <- haven::read_dta(path)
#' codebook(hosp)
#'
#'
#' ## to use piping function
#' library(magrittr)
#'
#' ## generate variable with NA
#' generate(hosp, average, NA) %>%
#'     keep(average)
#'
#'
#' ## generate variable with other variables
#' generate(hosp, average, pain) %>%
#'     keep(pain, average)
#'
#' generate(hosp, average, (test1 + test2) / 2) %>%
#'     keep(test1, test2, average)
#'
#'
#' ## existing variable cann't be generated.
#' generate(hosp, pain)
#' }
#'
#' @export
generate <- function(data, var, .expr = NULL)
{
    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    ## assign data into .data for further evaluation
    .data <- data



    ## get variables' and arguments' names
    .vars.names <- names(data)
    var <- as.character(.args$var)
    .expr <- deparse(substitute(.expr))

    ## if variable already exisits, then stop
    if (any(.vars.names %in% var)) {
        stop(paste0(" ... '", var, "' already exisits! ... "))
    }



    ## formulate text syntax, evaluate and assign to .data
    .expr.txt <- paste0("within(.data, ", var, " <- ", .expr, ")")

    tryCatch({
        .data <- eval(parse(text = .expr.txt))
    }, error = function(cnd) {
        stop(" ... Expression cannot be evaluated! ... ")
    })


    ## Display message to nofity changes
    # printMsg(paste0("Expression used to generate: '", .expr.txt, "'"))
    .var <- is.na(.data[, var])
    if (any(.var)) {
        printMsg(paste0(nrow(.data[!.var, var]), " real values generated | ",
                        nrow(.data[.var, var]), " missing values generated"))
    } else {
        printMsg(paste0(nrow(.data[, var]), " real values generated"))
    }

    return(.data)
}
