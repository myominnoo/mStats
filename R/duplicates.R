#' @title Report and tag duplicated observations
#'
#' @description
#' \code{duplicates()} reports duplications and creates indexes.
#'
#' @param data Dataset
#' @param ... Variables to find duplications.
#' @param drop if `TRUE`, delete all duplicated records, keeping all unique.
#' If not specified, all variables are used.
#'
#'
#' @details
#'
#' Specified variables are used to search for duplications. If not
#' specified, all variables are used.
#'
#'
#' Then they are pasted as a character vector for speedy operation,
#' extract duplication data and make a report.
#'
#' The return dataset is added a new variable called \code{dup}
#' for further use.
#'
#'
#' \strong{ANNOTATIONS}:
#'
#' \code{Copies}       - nth Copies
#'
#' \code{Observations} - Number of corresponding observations
#'
#' \code{Surplus}  - Number of surplus observations
#'
#' \code{dup} - indicates copies within the dataset:
#'
#' 0 = unique observations
#'
#' 2 = duplicated two times
#'
#' 3 = duplicated three times and so on ...
#'
#'
#' @return
#'
#' Modified dataset with additional variable \code{dup}
#'
#'
#' @import stats
#'
#'
#' @concept duplicates report distinct unique
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
#' \dontrun{
#'
#'
#' # Example from IDRE UCLA
#' path <- "https://stats.idre.ucla.edu/stat/data/patient_pt1_stata_dm.dta"
#' hosp <- haven::read_dta(path)
#' codebook(hosp)
#'
#'
#' ## to use piping function
#' library(magrittr)
#'
#'
#' ## find duplicated records using all variables
#' duplicates(hosp) %>%
#'     codebook
#'
#'
#'
#' ## find duplicated records using one variables
#' duplicates(hosp, hospid) %>%
#'     codebook
#'
#'
#'
#' ## find duplicated records using two variables
#' duplicates(hosp, hospid, docid) %>%
#'     codebook
#'
#'
#'
#' ## removing duplicated records
#' duplicates(hosp, drop = TRUE) %>%
#'     codebook
#'
#' duplicates(hosp, hospid, drop = TRUE) %>%
#'     codebook
#'
#' duplicates(hosp, hospid, docid, drop = TRUE) %>%
#'     codebook
#' }
#'
#' @export
duplicates <- function(data, ... , drop = FALSE)
{

    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    ## assign data into .data for further evaluation
    .data <- data
    .vars.names <- names(data)
    .data.nrow <- nrow(.data)


    ## get variable names within three dots to search for duplicates
    .dup.vars <- as.character(enquos(.args, c("data", "drop")))
    # If length is 0, then this is equal to all variables
    .dup.vars.len <- length(.dup.vars)

    ## check vars to find duplicates. If none, use all variables.
    if (.dup.vars.len == 0) {
        .dup.vars <- .vars.names
    }


    ## create expression to order data
    .expr.txt <- paste0(".data[with(.data, order(",
                        paste0(.dup.vars, collapse = ", "),
                        ")), ]")
    .data <- eval(parse(text = .expr.txt))

    ## create unique id. if variables not specified, then all variables are
    # used.
    .dup.id <- sapply(1:.data.nrow, function(z) {
        if (.dup.vars.len == 0) {
            paste(.data[z, ], collapse = "")
        } else {
            paste(.data[z, .dup.vars], collapse = "")
        }
    })

    # Create serial id for ave function
    ## this is later to create another function like _n or _N
    .dup.ave <- as.numeric(ave(.dup.id, .dup.id, FUN = seq_along))
    ## get the last number of serial number
    .dup.obs <- sapply(.dup.id, function(z) {
        .dup <- .dup.ave[.dup.id == z]
        .dup[length(.dup)]
    })


    ## create table and use the categories to calculate surplus number
    .dup.obs.tbl <- table(.dup.obs)
    .dup.obs.tbl.names <- as.numeric(names(.dup.obs.tbl))
    .non.dup <- sapply(.dup.obs.tbl.names, function(z) {
        .id <- .dup.id[.dup.obs == z]
        length(.id[!duplicated(.id)])
    })


    ## create final table for report
    .tbl <- data.frame(
        cbind("|", .dup.obs.tbl.names, "|",
              .dup.obs.tbl, "|",
              .dup.obs.tbl - .non.dup, "|")
    )
    names(.tbl) <- c("+", "Copies", "+", "Observations", "+",
                     "Surplus", "+")

    ## display report
    printText(.tbl,
              paste0("Duplicates in terms of ",
                     ifelse(.dup.vars.len == 0, "all variables",
                            paste0(.dup.vars, collapse = " + "))),
              .printDF = TRUE)
    printMsg(paste0("Number of Observation: ", nrow(.data)))


    ## create a dup variable for indication
    .data$dup <- .dup.obs - 1



    ## if drop is TRUE, then drop all duplications and keep all unique.
    if (drop) {
        .dup.drop <- .dup.ave == 1
        .data <- .data[.dup.drop, ]
        printMsg(paste0(sum(!.dup.drop * 1), " observations deleted"))
    }

    return(.data)
}
