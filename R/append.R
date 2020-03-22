#' @title Append multiple datasets
#'
#' @description
#'
#' \code{append()} joins multiple datasets of the same column names.
#'
#' @param data Base dataset
#' @param ... Datasets to be appended
#'
#' @details
#'
#' Multiple datasets can be appended only if they have the same variables in the
#' same order.
#'
#'
#' \preformatted{append(data, data1, data2, data3, etc)}
#'
#'
#'
#' @return
#' Modified Dataset
#'
#' @concept
#'
#' append, join rows combine rbind
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
#'
#' ## use piping function
#' library(magrittr)
#'
#'
#' ## appending three times
#' append(hosp, hosp, hosp, hosp) %>%
#'     codebook
#'
#'
#' ## appending directly from web
#' append(hosp, haven::read_dta(path)) %>%
#'     codebook
#' }
#'
#' @export
append <- function(data, ... )
{
    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    ## get column names from data
    .vars.names <- names(data)


    ## get dataset names within three dots
    .data.names <- as.character(enquos(.args, "data"))

    ## check if column names are identicals
    .vars.names.equal <- sapply(.data.names, function(z) {
        .data <- eval(parse(text = z))
        identical(.vars.names, names(.data))
    })

    if (any(!.vars.names.equal)) {
        stop(" ... variables are not consistent! ... ")
    }

    ## global declaration of .data
    .data <- data
    for (i in 1:length(.data.names)) {
        .data <- rbind(.data, eval(parse(text = .data.names[i])))
    }

    ## Display message to nofity changes
    printMsg(paste0(length(.data.names), " datasets appended to '",
                    .args$data, "': ",
                    paste0(.data.names, collapse = ", ")))

    return(.data)
}
