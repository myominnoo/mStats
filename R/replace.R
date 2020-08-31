#' @title Change contents of an existing variable
#'
#' @description
#'
#' \code{replace()} alters the values of a variable when specified
#' conditions are met.
#'
#' `AND` conditions
#' \preformatted{replace(data, var, value, condition1, condition2, etc)}
#'
#' `OR` conditions
#' \preformatted{replace(data, var, value, condition1 | condition2, etc)}
#'
#' If conditions are not specified, \code{replace()} changes the whole
#' variable with specified value.
#'
#' \preformatted{replace(data, var, value)}
#'
#' @param data Dataset
#' @param var Variable
#' @param value Replacement value
#' @param ... `if` conditions.
#'
#' @details
#'
#' It is used when multiple conditions have to be met to change a value.
#' The function first checks whether specified value is a variable of the dataset.
#' If yes, then the values are replaced with those of that variables
#' with the conditions.
#'
#'
#' @return
#' Modified Dataset
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
#' ## use infert dataset
#' data(infert)
#'
#' ## replace parity == NA if parity > 4
#' infert.new <- replace(infert, parity, NA, parity > 4)
#' tab(infert.new, parity)
#'
#' ## replace education as character
#' infert.new <- replace(infert, education, as.character(education))
#' codebook(infert.new)
#'
#' @export

replace <- function(data, var, value, ... )
{
    ## match call arguments
    .args <- as.list(match.call())

    ## copy data to .data
    .data <- data

    ## get names of dataset and headings
    .data_name <- deparse(substitute(data))
    var <- deparse(substitute(var))
    value <- deparse(substitute(value))

    ## if input is not a data.frame, stop
    if (!is.data.frame(.data)) {
        stop(paste0("`", .data_name, "` must be a data.frame"),
             call. = FALSE)
    }

    ## get var label
    .var.lbl <- attr(.data[[var]], "label")

    ## get expression from three dots
    .expr <- enquotes(.args, c("data", "var", "value"))
    ## if more than one expression, combine with & operator
    if (length(.expr) >= 1) {
        .expr <- paste0("(", .expr, ")", collapse = " & ")
        .expr_txt <- paste0("[", .expr, "]")
    } else if (length(.expr) == 0) {
        .expr_txt <- NULL
    }

    # if var is a factor, remove corresponding levels
    if (is.factor(.data[[var]])) {
        .data[[var]] <- as.character(.data[[var]])
    }

    ## if value is more than one vector, then collapse it
    value <- paste0(value, collapse = "")

    ## if expression is empty, then replace the whole variable
    ## if not, repress using expression
    ## evaluate the whole expression
    tryCatch({
        .df <- eval(parse(
            text = paste0("within(.data, ", var, .expr_txt, " <- ", value, ")")
        ))
    }, error = function(cnd) {
        stop(cnd, call. = FALSE)
    })

    # if var is a factor, convert to factor again corresponding levels
    if (is.factor(.data[[var]])) {
        .df[[var]] <- as.factor(.df[[var]])
    }

    ## assign label back to the var
    attr(.df[[var]], "label") <- .var.lbl

    ## record number of rows changed that will be made
    if (length(.expr) > 0) {
        tryCatch({
            .var_mod <- eval(parse(
                text = paste0(".data[with(.data, which(", .expr, ")), ]")
            ))
        }, error = function(cnd) {
            stop(cnd, call. = FALSE)
        })
        .var_mod <- nrow(.var_mod)
    } else {
        .var_mod <- nrow(.data)
    }

    ## Display message to nofity changes
    printText(
        paste0("'", var, "' replaced with '", value, "'",
               ifelse(length(.expr) > 0, paste0(" when ", .expr), ""),
               ": ", .var_mod, " Obs changed")
    )

    return(.df)
}
