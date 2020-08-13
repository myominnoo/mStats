#' @title Duplicate observations within a dataframe
#'
#' @description
#'
#' \code{expand2} generates duplicated observations within a dataframe.
#'
#' @param data a data frame object
#' @param n_n index or indexes specifying row numbers
#' @param copies desired number of copies
#' @param original a logical indicating whether to keep the original dataframe
#'
#' @details
#'
#' \code{expand2} appends observations from the dataframe
#' with n copies of the observations with
#' specified indexes of observations or all data.
#'
#' @return
#'
#' \code{data.frame}
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
#' ## create duplicates
#' infert.new <- expand2(infert, 1:5, copies = 2)
#' codebook(infert.new)
#'
#'
#' ## check duplicates report and rmeove dup
#' infert.dupremove <- duplicates(infert.new, make_unique = TRUE)
#' codebook(infert.dupremove)
#'
#' ## remove only copy no. 3
#' infert.3copies <- duplicates(infert.new)
#' tab(infert.3copies, .dup_id)
#'
#' infert.3copies <- infert.3copies[infert.3copies$.dup_id != 2, ]
#' codebook(infert.3copies)
#'
#' @export
expand2 <- function(data, n_n = NULL, copies = 2, original = TRUE)
{
    data.lbl <- attr(data, "label")
    data <- data.frame(data)
    vars.lbl <- sapply(data, function(z) {
        lbl <- attr(z, "label")
        if (is.null(lbl)) {
            lbl <- "<NA>"
        } else {
            lbl <- paste(attr(z, "label"), collapse = " ")
        }
        lbl
    })
    #### if n_n is empty, put number of all rows to n_n
    if (is.null(n_n)) {
        n_n <- nrow(data)
    }
    #### if there are more than one values in n_n, take the last value
    if (length(n_n) == 1) {
        n_n <- 1:n_n
    }
    t <- data[n_n, ]

    if (original) {
        f <- data
    } else {
        f <- NULL
    }
    for (i in 1:(copies)) {
        f <- rbind(f, t)
    }
    attr(f, "label") <- data.lbl
    for (i in 1:ncol(f)) {
        attr(f[, i], "label") <- vars.lbl[i]
    }

    return(f)
}
