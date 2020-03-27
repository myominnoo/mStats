#' @title Join two or more datasets
#'
#' @description
#' \code{leftJoin()} merges two or multiple datasets sharing common variables
#' and keeping all rows from x or master.
#'
#' @param data master dataset
#' @param ... mergers or datasets to merge into master dataset
#' @param by common variables
#'
#' @details
#'
#' The join keeps all rows or observations in master dataset with matched
#' observations from mergers. It adds one more variable `merge_` to the resulting
#' dataset. The value `1` of `merge_` indicates the rows are from master dataset and
#' for 2, rows from merger dataset and 3 is matched observations. In leftJoin,
#' there can be both 1 or 3 in the return dataset.
#'
#'
#' # Displaying notes
#'
#' The notes are displayed in a fashion to inform the user what has been joined or
#' not joined. This provides useful insights into one's own data. This is inspired
#' by `STATA`.
#'
#' @note
#'
#' For `tibble` data format, the return dataset from Join operation results in
#'  `data.frame`since the function is based on \code{\link{merge}}.
#'
#' @return
#' Modified dataset in `data.frame`
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
#'
#' ## set seed
#' set.seed(123)
#' ## first, create a patient dataset
#' patient <- data.frame(
#'     hospid = 1:100,
#'     docid = round(runif(100, 1, 15)),
#'     sex = runif(100, 1, 2),
#'     age = runif(100, 30, 60)
#' )
#'
#' ## now create a doctor dataset
#' doc <- data.frame(
#'    docid = c(1:10, 21:25),
#'    rating = round(runif(15, 1, 5))
#' )
#'
#' ## left join the two dataset
#' leftJoin(patient, doc, by = "docid")
#'
#' ## there are 36 records not matched, 31 not matched from master dataset,
#' ## 5 not matched from merger dataset. 69 Final matched records
#'
#'
#' @export
leftJoin <- function(data, ... , by)
{

    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    ## get variable names within three dots to search for duplicates
    .data.names <- as.character(enquos(.args, c("data", "by")))

    ## get all data within three dots
    .data2 <- list(...)


    ## Global data
    .data <- data
    lapply(1:length(.data.names), function(z) {

        ## assign to .y for easy coding
        .y <- .data2[[z]]

        ## add dummy x to data
        ## add index 1 as mater, 2 as merger, 3 as matched.
        .data$dummy_x <- 1


        ## add dummy y for comparison
        .y$dummy_y <- 2

        ## merging
        .data <- merge(.data, .y, by = by, all.x = TRUE, all.y = TRUE)

        ## check missing values in dummy_x and _y
        .index.x <- is.na(.data$dummy_x)
        .index.y <- is.na(.data$dummy_y)

        ## remove dummy variables
        .data <- .data[, !(names(.data) %in% c("dummy_x", "dummy_y"))]


        .data$merge_ <- ifelse(.index.y, 1,
                               ifelse(.index.x, 2, 3))

        ## assign back to global .data
        .data <<- .data[.data$merge_ != 2, ]

        ## Displaying notes
        .dis <- data.frame(
            Result = c("not matched", "    master", "    merger", "", "matched"),
            Frequency = c(sum(.index.y) + sum(.index.x),
                          sum(.index.y),
                          sum(.index.x),
                          "",
                          sum(!.index.x & !.index.y)),
            merge_ = c("", "1", "2", "", "3")
        )

        printText(.dis, paste0("Notes on Joining '", .args$data, "'"),
                  .printDF = TRUE)
        printMsg(paste0("master: ", .args$data))
        printMsg(paste0("merger: ", .data.names[z]))

    })

    return(.data)
}
