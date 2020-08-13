#' @title APPEND datasets
#'
#' @description
#'
#' \code{append()} row-combines multiple datasets of the same column names.
#'
#' @param data Base dataset
#' @param ... Datasets to be appended
#'
#' @details
#'
#' Single or multiple datasets can be appended.
#' At least one matching variable must be in the appending datasets.
#' If arrangement or order of variables are not the same, they are
#' automatically rematched to the MASTER dataset and appended.
#'
#'
#' \preformatted{append(data, data1, data2, data3, etc)}
#'
#' `data` = MASTER DATASET
#' Remaining datasets: data1, data2, data3, etc = APPENDING DATASETS
#'
#'
#'
#' @return
#' Modified dataset
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
#' codebook(infert)
#'
#' infert1 <- infert[-c(10:20), -2]
#' infert2 <- infert[-c(40:60), -4]
#'
#' infert.new <- append(infert, infert1, infert2)
#' codebook(infert.new)
#'
#' @export
append <- function(data, ... )
{
    ## match call arguments
    .args <- as.list(match.call())

    ## copy data to .data
    .data <- data

    ## get names of dataset and headings
    .data_name <- .args$data
    .ds_names <- enquotes(.args, "data")
    .vars_names <- names(.data)


    ## get dataset names within three dots
    .ds_list <- list(...)

    ## get logical vectors to indicate
    ## variables names that are not in the appending datasets
    .vars_miss <- lapply(.ds_list, function(z) {
        .ds_names <- names(z)
        .contain <- .vars_names %in% names(z)

        ## if none match with the master dataset, stop
        if (!any(.contain)) {
            stop("At least one variable must match to append.",
                 call. = FALSE)
        }
        ## retun logical vectors
        .contain
    })

    ## process final datasets to be appended
    ## 1) match the variables' names
    ## 2) if some variables are not there in the datasets,
    ##      create them on the fly.
    .ds_list <- lapply(1:length(.ds_list), function(z) {
        .ds <- .ds_list[[z]]
        .ds[, .vars_names[!.vars_miss[[z]]]] <- NA
        .ds[, .vars_names]
    })

    ## append to the master dataset
    for (i in 1:length(.ds_list)) {
        .data <- rbind(.data, .ds_list[[i]])
        printText(paste0("[", i, "] Appended '", .ds_names[[i]], "' to '",
                         .data_name, "'"))
    }

    ## return
    return(.data)
}
