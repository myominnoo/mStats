#' @title Count from N to N
#'
#' @description
#' \code{countBy()} produces serial counting numbers or
#' total number of observations per each observation.
#'
#' @param data Dataset
#' @param ... Variables to find duplications.
#' @param var_name name of variable for serial counting
#' @param N if `TRUE`, it generates total number of observations for
#' each observation. If `FALSE`, it produces sets of serial numbers
#' from 1 to total number of observations per group.
#'
#'
#' @details
#'
#' If variables are not specified within `...` arguments, all variables
#' are used to order first and create unique identifier for each
#' observation.
#'
#' If variables are specified, unique identifiers are generated based
#' on the combination of variables, and grouped. Serial numbers are then
#' produced based on each group.
#'
#'
#' If `N` is set to `TRUE`, total number of observations is generated
#' per each observation for the whole dataset or per group. A new variable
#' `.N_` is created in the dataset.
#'
#' If `FALSE`, serial numbers are generated per each observation for
#' the whole dataset or per group. A new variable `.n_` is created in the
#' dataset.
#'
#' Variable's name can be set by specifying `var_name`.
#'
#' @return
#'
#' Modified ordered dataset with one additional variable
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' ## Example from UCLA Website
#' ## https://stats.idre.ucla.edu/stata/seminars/notes/counting-from-_n-to-_n/
#' df <- data.frame(matrix(
#'     c(72, 1, 84, 2, 76, 1, 89, 3, 82, 2, 90, 1, 85, 1),
#'     byrow = TRUE, ncol = 2))
#' names(df) <- c("score", "group")
#'
#' countBy(df)
#' countBy(df, N = TRUE)
#'
#' ## count per group
#' countBy(df, group)
#' countBy(df, group, var_name = "n1")
#'
#' ## count per group (total observation)
#' countBy(df, group, N = TRUE)
#' countBy(df, group, var_name = "n2", N = TRUE)
#'
#' @export
countBy <- function(data, ... , var_name = NULL, N = FALSE)
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

    ## get variable names within three dots to search for duplicates
    .vars_dup <- enquotes(.args, c("data", "var_name", "N"))
    # If length is 0, then this is equal to all variables
    .vars_dup_len <- length(.vars_dup)

    ## check vars to find duplicates. If none, use all variables.
    if (.vars_dup_len == 0) {
        .vars_dup <- .vars_names
    }

    ## create expression to order data
    .data <- eval(parse(
        text = paste0(".data[with(.data, order(",
                      paste0(.vars_dup, collapse = ", "),
                      ")), ]")
    ))

    ## create identifiers to check duplications
    .id <- apply(.data[.vars_dup], 1, paste, collapse = " ")

    ## create seiral id with ave function
    .id_num <- ave(.id, .id, FUN = seq_along)

    ## get the last number of serial number
    .last_obs <- sapply(.id, function(z) {
        .dup_id <- .id_num[.id == z]
        .dup_id[length(.dup_id)]
    })

    ## if no variables specified, then use all variables
    if (.vars_dup_len == 0) {
        .id_num <- ave(.id_num, .id_num, FUN = seq_along)
        .last_obs <- sapply(1:length(.id_num),
                            function(z) max(.id_num, na.rm = TRUE))
    }

    ## add count data to the dataset
    if (N) {
        .data$.N_ <- .last_obs
        attr(.data$.N_, "label") <- "<Sys.Gen: Last Observation>"
    } else {
        .data$.n_ <- .id_num
        attr(.data$.n_, "label") <- "<Sys.Gen: Grouped Serial Number>"
    }

    ## process variable name
    if (!is.null(var_name)) {
        names(.data)[ncol(.data)] <- var_name
    }

    return(.data)
}
