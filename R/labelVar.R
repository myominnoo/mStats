#' @title Add or replace labels of variables and dataset
#'
#' @description
#'
#' \code{labelVar} labels \code{variables}.
#'
#' \code{labelData} labels \code{Dataset}.
#'
#' @param data dataset
#' @param var one variable or variables
#' @param lbl to specify labels. If not specified, label is removed
#'
#' @details
#'
#' Labels are useful to provide more detailed information about
#' variables or dataset. Many functions in \code{mStats} package
#' extract label information and display them as footnote.
#'
#' \strong{Label single or multiple variables}
#'
#' Single or multiple variables can be labelled.
#'
#' Example: single variable
#'
#' \preformatted{labelVar(data, var, lbl)}
#'
#' Example: multiple variable
#'
#' \preformatted{labelVar(data,
#'       c(var1, var2, var3),
#'       c(lbl1, lbl2, lbl3))}
#'
#' \strong{Label Dataset}
#'
#' Dataset can also be labelled and displayed in \code{codebook}.
#'
#' \preformatted{labelData(data, lbl)}
#'
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
#'
#' ## label education
#' infert.new <- labelVar(infert, education, "patient's education")
#' codebook(infert.new)
#'
#' ## label multiple variables
#' infert.new <- labelVar(infert, c(education, age, case),
#'                        c("patient's education", "age in years", "case status"))
#' codebook(infert.new)
#'
#'
#' ## label dataset
#' infert.new <- labelData(infert.new,
#'                         "Infertility after Spontaneous and Induced Abortion")
#' codebook(infert.new)
#'
#' @export
labelVar <- function(data, var, lbl = NULL)
{

    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    ## assign data into .data for further evaluation
    .data <- data


    ## numbers of vars and lbls
    .var <- as.character(.args$var)
    .var.len <- length(.var)
    if (.var.len > 1) {
        .var <- .var[-1]
        .var.len <- length(.var)
    }
    ## Check if colon is there.
    ## if present, retrieve variables between the two variables
    if (any(grepl(":", .var))) {
        .var <- do.call(
            c,
            lapply(.var, function(z) {
                .colon <- grepl(":", z)
                if (.colon) {
                    splitByColon(data, z, .colon)
                } else {
                    z
                }
            })
        )
        .var.len <- length(.var)
    }


    ## check labels
    .lbl <- lbl
    .lbl.len <- ifelse(is.null(lbl), 1, length(lbl))

    if (.var.len != .lbl.len) {
        if (is.null(lbl)) {
            .lbl <- rep(NULL, .var.len)
        } else {
            stop(" ... Variable and its label specify different length! ... ")
        }
    }


    ## loop and assign each variable to corresponding labels
    for (i in 1:.var.len) {
        .txt <- paste0(".data[, '", .var[i], "']")
        attr(.data[[.var[i]]], "label") <- .lbl[i]
        printMsg(paste0("Variable '", .var[i],
                        "' labelled as '",
                        ifelse(is.null(.lbl[i]), "<NA>", .lbl[i]), "'"))
    }

    return(.data)
}




#' @rdname labelVar
#' @export
labelData <- function(data, lbl = NULL)
{

    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    attr(data, "label") <- lbl
    printMsg(paste0("Dataset '", .args$data,
                    "' labelled as '",
                    ifelse(is.null(lbl), "<NA>", lbl), "'"))
    return(data)
}
