#' @title Create lagged variables
#'
#' @description
#'
#' \code{lagRows()} creates lagged version of existing variables
#'
#' @param data Dataset
#' @param var Existing variable
#' @param by If specified, lagged variable is created grouped by this variable.
#' If not specified, it is created taking the whole variable.
#' @param lag_var name for lagged variable
#'
#'
#' @details
#'
#' This is often encountered in time-related analysis.
#' In a lagged variable, values from earlier points in time are placed in later
#' rows of dataset.
#'
#' @note
#'
#' Before using \code{lagRows}, the dataset needs to be sorted by a id variable
#' or similar variable.
#'
#' @return
#' Modified Dataset
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
#'
#' ## create a dataset with dates
#' df <- data.frame(
#'     hospid = 1:100,
#'     docid = round(runif(100, 1, 10)),
#'     dis_date = formatDate(runif(100, 42700, 42800))
#' )
#'
#' ## lagged dis_date, not specifed "by"
#' lagRows(df, dis_date)
#'
#' ## lagged dis_date by docid
#' ## first we need to sort
#' df1 <- arrange(df, docid)
#' df1
#' lagRows(df1, dis_date, by = docid, lag_var = "lag_date")
#'
#'
#' @export
lagRows <- function(data, var, by = NULL, lag_var = NULL)
{

    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    ## assign .data for further use
    .data <- data
    .var <- eval(substitute(var), .data)
    .by <- .args$by

    if (is.null(.by)) {
        .lag <- c(.var[1], .var[-length(.var)])
        .lag[1] <- NA
    } else {
        .by <- eval(substitute(by), .data)
        ## get var by each levels of by
        .lvl <- unique(.by)
        .lag <- do.call(
            c,
            lapply(.lvl, function(z) {
                .equal <- .by == z
                .lag <- .var[.equal]
                .lag <- c(.lag[1], .lag[-length(.lag)])
                .lag[1] <- NA
                .lag
            })
        )
    }

    .data$lag_var <- .lag

    ## print
    printMsg(paste0(length(.data$lag_var), " lagged values generated"))

    if (!is.null(lag_var)) {
        names(.data)[ncol(.data)] <- lag_var
    }

    return(.data)
}
