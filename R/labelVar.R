#' @title ADD, REPLACE or REMOVE labels of variables and dataset
#'
#' @description
#'
#' \code{labelVar()} labels \code{variables}.
#'
#' \code{labelData()} labels \code{Dataset}.
#'
#' @param data dataset
#' @param ... variable = "label"
#' @param lbl label for dataset
#'
#' @details
#'
#' Labels are useful to provide more detailed information about
#' variables or dataset. Many functions in \code{mStats} package
#' extract label information and display them as footnote.
#' Single or multiple variables can be labelled.
#'
#' \strong{Label single variables}
#'
#'
#' \preformatted{labelVar(data, var = "lbl")}
#'
#' \strong{Label multiple variables}
#'
#' \preformatted{labelVar(data,
#'       var1 = "lbl1", var2 = "lbl2", var3 = "lbl3", etc)}
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
#'
#' ## add labels
#' infert.new <- labelVar(infert,
#'                        education = "EDUCATION",
#'                        age = "AGE IN YEARS",
#'                        parity = "count",
#'                        case = "case status",
#'                        induced = "# of prior induced abortions",
#'                        spontaneous = "# of prior spon. abortions",
#'                        stratum = "Matched set number",
#'                        pooled.stratum = "Stratum number")
#'
#' infert.new <- labelData(infert.new,
#'     "Infertility after Spontaneous and Induced Abortion")
#' codebook(infert.new)
#'
#'
#' @export
labelVar <- function(data, ... )
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

    ## get the names within three dots
    .vars <- .args[-c(1:2)]

    ## check if variables are valid
    sapply(1:length(.vars), function(z) {
        .var_name <- names(.vars[z])
        ## check if all variables are in the dataset
        if (!(.var_name %in% .vars_names)) {
            stop(paste0("Variable '", .var_name, "' not found in the dataset"),
                 call. = FALSE)
        }

        .lbl <- as.character(.vars[z])
        attr(.data[[.var_name]], "label") <<- .lbl
        printText(
            paste0("'", .var_name, "' labelled as '",
                   .lbl, "'"))
    })

    return(.data)
}



#' @rdname labelVar
#' @export
labelData <- function(data, lbl = NULL)
{
    ## match call arguments
    .args <- as.list(match.call())

    ## copy data to .data
    .data <- data

    ## if input is not a data.frame, stop
    if (!is.data.frame(.data)) {
        stop("`.data` must be a data.frame", call. = FALSE)
    }

    ## get label
    attr(.data, "label") <- lbl
    if (is.null(lbl)) {
        printText(paste0("Dataset '", .args$data, "': label removed"))
    } else {
        printText(paste0("Dataset '", .args$data, "': labelled as '",
                         lbl, "'"))
    }

    return(.data)
}
