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
#' @author
#'
#' For any feedback, please contact \code{Myo Minn Oo} via:
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' ## use infert data
#' data(infert)
#' codebook(infert)
#'
#' ## create a new variable induced2 with 0 and 1+2
#' infert.new <- generate(infert, induced2, as.numeric(induced == 0))
#' tab(infert.new, induced2)
#'
#' ## create a new variable with values the same as education
#' infert.new <- generate(infert, edu, education)
#' tab(infert.new, edu)
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
        printMsg(paste0(length(.data[[var]]), " real values generated"))
    }

    return(.data)
}
