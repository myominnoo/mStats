#' @title Create a new variable
#'
#' @description
#' \code{generate()} creates a new variable within the data frame
#'
#' @param data Dataset
#' @param new_var Name of new variable without double quote
#' @param expr value or Expression for simple arithmetic or logical operations:
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
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' ## use infert data
#' data(infert)
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
generate <- function(data, new_var, expr = NULL )
{
    ## match call arguments
    .args <- as.list(match.call())

    ## copy data to .data
    .data <- data

    ## get names of dataset and headings
    .data_name <- deparse(substitute(data))
    .vars_names <- names(.data)

    ## if input is not a data.frame, stop
    if (!is.data.frame(.data)) {
        stop("`.data` must be a data.frame", call. = FALSE)
    }

    ## get variable name
    new_var <- as.character(.args$new_var)

    ## if variable already exisits, then stop
    if (any(.vars_names %in% new_var)) {
        stop(paste0("'", new_var, "' already exisits in the dataset"),
             call. = FALSE)
    }

    ## create expression text
    ## if value is more than one vector, then collapse it
    expr <- paste0(deparse(substitute(expr)), collapse = "")
    .expr <- paste0("with(.data, ", expr, ")")


    ## add new var to the dataset
    ## add label
    tryCatch({
        .data$new_var <- eval(parse(text = .expr))
    }, error = function(cnd) {
        stop(cnd)
    })
    attr(.data$new_var, "label") <- paste0(new_var, ": ", expr)

    ## get missing value and total number
    ## and print
    .var_miss <- sum(is.na(.data$new_var))
    .var_nrow <- length(.data$new_var)
    if (any(.var_miss)) {
        printText(paste0("'", new_var, "' generated ",
                         .var_nrow - .var_miss, " valid + ",
                         .var_miss, " <NA> values" ))
    } else {
        printText(paste0("'", new_var, "' generated ",
                         .var_nrow - .var_miss, " valid values"))
    }

    names(.data)[ncol(.data)] <- new_var


    return(.data)
}
