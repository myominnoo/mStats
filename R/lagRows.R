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
#' @param new_var name for lagged variable
#' @param last_obs `TRUE`, retrieve the last observation per group.
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
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' set.seed(100)
#' ## create a dataset with dates
#' df1 <- data.frame(
#'     hospid = 1:100,
#'     docid = round(runif(100, 1, 10)),
#'     dis_date = formatDate(runif(100, 42700, 42800))
#' )
#'
#' ## lagged dis_date, not specifed "by"
#' lagRows(df1, dis_date)
#'
#' ## lagged dis_date by docid
#' ## first we need to sort
#' df2 <- df1[order(df1$docid), ]
#' df2
#'
#' ## lag dates within groups
#' lagRows(df2, dis_date, by = docid, new_var = lag_date)
#' lagRows(df2, dis_date, by = docid, lag_date, TRUE)
#'
#'
#' @export
lagRows <- function(data, var, by = NULL, new_var = NULL, last_obs = FALSE)
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
        stop(paste0("`", .data_name, "` must be a data.frame"),
             call. = FALSE)
    }

    ## assign var and by
    var <- eval(substitute(var), .data)
    by <- .args$by

    ## create lagged variables
    if (is.null(by)) {
        .lag <- c(var[1], var[-length(var)])
        .lag[1] <- NA
    } else {
        ## if by is specified, then assign by
        ## then create levels
        by <- .data[[as.character(by)]]
        .lvl <- unique(by)
        .lag <- do.call(
            c,
            lapply(.lvl, function(z) {
                if (is.na(z)) {
                    .equal <- is.na(by)
                } else {
                    .equal <- which(by == z)
                }
                .lag <- var[.equal]
                .lag <- c(.lag[1], .lag[-length(.lag)])
                .lag[1] <- NA
                if (last_obs) {
                    .lag <- var[.equal]
                    .lag <- rep(.lag[length(.lag)], length(.lag))
                }
                .lag
            })
        )
    }

    ## add lagged variable to dataset
    ## add label and create name
    tryCatch({
        .data$new_var <- .lag
    }, error = function(cnd) {
        stop(cnd, call. = FALSE)
    })

    attr(.data$new_var, "label") <-
        paste0("<SYS.GEN: Lagged '", .args$var, "'>")
    new_var <- ifelse(is.null(.args$new_var),
                      paste0(.args$var, "_lag"),
                      as.character(.args$new_var))

    names(.data)[ncol(.data)] <- new_var


    ## print changes
    printText(paste0("'", .args$var, "' lagged into '", new_var, "'"))

    return(.data)
}
