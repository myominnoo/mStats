#' @title Convert \code{2x2 tables} to \code{data.frame}
#'
#' @description
#'
#' \code{expandTables()} generates a data frame and supports two levels.
#'
#' @param ... vectors of 2x2 tables
#' @param exp_name Name of \code{exp} Variable
#' @param exp_lvl Names of two categories in the order of
#' Exposed and non-exposed
#' @param case_name Name of \code{Case} variable
#' @param case_lvl names of two categories in the order of
#' @param strata_name Name of stratified variable
#'
#' @details
#'
#' \strong{expandTables}
#'
#' uses the vectors of \code{2x2} tables and
#' generates a data frame of at least two columns:
#' exp and case.
#'
#' \preformatted{expandTables(c(100, 200, 100, 200))}
#'
#' \code{Strata}
#'
#' Multiple tables can be used to construct a dataset by specifying
#' \code{strata_name} as follow. Strata can be included
#' using multiple named vectors.
#'
#' \preformatted{
#' expandTables(
#'              strata1 = c(100, 200, 100, 200),
#'              strata2 = c(100, 200, 100, 200),
#'              strata3 = c(100, 200, 100, 200),
#'              exp_name = "exp",
#'              exp_lvl = c("exposed", "unexposed"),
#'              case_name = "case",
#'              case_lvl = c("case", "control"),
#'              strata_name = "Strata"
#' )
#' }
#'
#' \code{Labels for variables}
#'
#' If names or lavels of variables are not specified, the followings are
#' applied.
#'
#' \enumerate{
#'     \item exp Name: \code{exp}
#'     \item exp levels: \code{exposed} and \code{unexposed}
#'     \item case Name: \code{case}
#'     \item case levels: \code{cases} and \code{controls}
#'     \item Strata Name: \code{strata}
#'     \item Note: Strata levels are not considered as vectors must
#'     be named.
#' }
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
#'
#' ## Asthma Example from Essential Medical Statistics
#' ## page 160
#'
#' asthma <- expandTables(c(81, 995, 57, 867),
#'               exp_name = "sex",
#'               exp_lvl = c("woman", "man"),
#'               case_name = "asthma",
#'               case_lvl = c("yes", "no"))
#'
#' ## label variable and dataset
#' asthma <- labelData(asthma, "Hypothetical Data of Asthma Prevalence")
#' asthma <- labelVar(asthma, sex = "Man or Woman",
#'                            asthma = "Asthma or No Asthma")
#'
#' ## Checking codebook
#' codebook(asthma)
#'
#'
#' ## simple tabulation
#' tab(asthma)
#'
#' ## cross-tabulation
#' tab(asthma, sex, by = asthma)
#'
#'
#' @export
expandTables <- function( ... ,
                          exp_name = "exp",
                          exp_lvl = c("exposed", "unexposed"),
                          case_name = "case",
                          case_lvl = c("cases", "controls"),
                          strata_name = "strata")
{
    # get vectors within three dots
    .vec <- list(...)
    .vec_len <- length(.vec)

    ## calculate strata
    .strata_names <- sapply(1:length(.vec), function(z) names(.vec[z]))
    .strata_times <- sapply(.vec, function(z) sum(z))

    ## create data.frame

    .exp <- rep(exp_lvl, each = 2)
    .case <- rep(case_lvl, times = 2)

    ## create data frame
    tryCatch({
        .df <- do.call(
            rbind,
            lapply(1:.vec_len, function(z) {
                .times <- unlist(.vec[z])
                .vec.exp <- rep(.exp, .times)
                .vec.case <- rep(.case, .times)
                .vec_strata <- rep(names(.vec[z]), sum(.times))

                if (is.null(.vec_strata)) {
                    .df <- data.frame(.vec.exp, .vec.case)
                    names(.df) <- c(exp_name, case_name)
                } else {
                    .df <- data.frame(.vec.exp, .vec.case, .vec_strata)
                    names(.df) <- c(exp_name, case_name, strata_name)
                }
                .df
            })
        )

        ## print message
        printText(paste0("Expanded into a dataset"))

    }, error = function(cnd) {
        stop(cnd, call. = FALSE)
    })


    return(.df)
}


#' @describeIn expandTables
#'
#' \code{expandFreq()} expands a frequency-weighted table into a dataset.
#' The table must be in dataset format.
#' It preserves the original dataset format.
#'
#' @param data frequency table in dataset format
#' @param freq variable for weighted frequency
#'
#' @details
#'
#' \strong{expandFreq()} uses the data.frame and construct the dataset
#' based on the
#' frequency weight. The frequency weight must be specified by
#'  \code{freq} argument.
#'
#'
#' \preformatted{
#' expandFreq(data = "data_name", freq = "freq")
#' }
#'
#' @examples
#'
#' ## Example for expandFreq  <<<<<--------->>>>>
#'
#' ## Example from UCLA website
#' ## you can download the dataset here:
#' ## https://stats.idre.ucla.edu/stat/stata/examples/icda/afterlife.dta
#'
#' al <- data.frame(gender = c(1, 1, 0, 0),
#'                  aftlife = c(1, 0, 1, 0),
#'                  freq = c(435, 147, 375, 134))
#' al.exp <- expandFreq(al, freq)
#'
#' ## check the numbers by tabulation
#' ## tab(al.exp, gender, by = aftlife)
#'
#' @export
expandFreq <- function(data, freq)
{
    ## match call arguments
    .args <- as.list(match.call())

    ## copy data to .data
    .data <- data

    ## if input is not a data.frame, stop
    if (!is.data.frame(.data)) {
        stop(paste0("`", .data_name, "` must be a data.frame"),
             call. = FALSE)
    }

    ## get names of dataset and headings
    .data_name <- deparse(substitute(data))
    .vars_names <- names(.data)
    freq <- .args$freq

    ## get where the frequency column is and get the freq
    .freqi <-.vars_names %in% as.character(freq)
    .freq <- .data[[freq]]

    ## if freq is not numbers, stop
    if (!is.numeric(.freq)) {
        stop(paste0("`", .freq, "` must be a number."),
             call. = FALSE)
    }

    ## repeat other columns per freq
    .t <- apply(.data[, !.freqi], 2, function(z) rep(z, .freq))

    ## put the dataset back to the original data frame
    ## this preserves the data structure of original data
    .df <- .data[0, !.freqi]
    .df[1:nrow(.t), ] <- .t

    ## print message
    printText(paste0("'", .data_name, "' expanded into dataset."))

    return(.df)
}
