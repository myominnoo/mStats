#' @title Convert \code{2x2 tables} to \code{data.frame}
#'
#' @description
#'
#' \code{expandTables()} generates a data frame and supports two levels.
#'
#' @param ... vectors of 2x2 tables
#' @param exp_name Name of \code{Exposure} Variable
#' @param exp_lvl Names of two categories in the order of
#' Exposed and non-exposed
#' @param case_name Name of \code{Case} variable
#' @param case_lvl names of two categories in the order of
#' @param strata_name Name of stratified variable
#' @param stringsAsFactors \code{TRUE} or \code{FALSE}
#'
#' If \code{TRUE}, character vector is converted to a factor when
#' \code{data.frame} is constructed.
#'
#' @details
#'
#' \code{expandTables} uses the vectors of \code{2x2} tables and
#' generates a data frame of at least two columns:
#' Exposure and Outcome.
#'
#' \preformatted{expandTables(c(100, 200, 100, 200))}
#'
#' \strong{Strata}
#'
#' Strata can be included using multiple named vectors. The
#' generated tables can be used for further calculation such as
#' Mantel Haenszel methods.
#'
#' \preformatted{
#' expandTables(
#'              strata1 = c(100, 200, 100, 200),
#'              strata2 = c(100, 200, 100, 200),
#'              strata3 = c(100, 200, 100, 200),
#'              exp_name = "Exposure",
#'              exp_lvl = c("exposed", "unexposed"),
#'              case_name = "Outcome",
#'              case_lvl = c("case", "control"),
#'              strata_name = "Strata"
#' )
#' }
#'
#' \strong{Labels}
#'
#' If variables' names or levels are not specified, the followings are
#' applied.
#'
#' \enumerate{
#'     \item Exposure Name: \code{exposure}
#'     \item Exposure levels: \code{exposed} and \code{unexposed}
#'     \item Outcome Name: \code{outcome}
#'     \item Outcome levels: \code{disease} and \code{healthy}
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
#' For any feedback, please contact \code{Myo Minn Oo} via:
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
#' asthma <- labelVar(asthma, c(sex, asthma), c("Man or Woman", "Asthma or No Asthma"))
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
#'
#' \dontrun{
#'
#' ### Simpson's Paradox: Example from Burt Gerstman's Epidemiology
#' ### Chapter 14, Page 326
#'
#' library(magrittr)
#' simpson <- expandTables(
#'     "1" = c(1000, 9000, 50, 950),
#'     "2" = c(95, 5, 5000, 5000),
#'     exp_name = "treatment",
#'     exp_lvl = c("new", "standard"),
#'     case_name = "outcome",
#'     case_lvl = c("alive", "dead"),
#'     strata_name = "clinic") %>%
#'     labelVar(c(treatment, outcome, clinic),
#'               c("Treatment: new or standard",
#'                 "Outcome: alive or dead", "clinic: 1 or 2")) %>%
#'     labelData("Example of Simpson's Paradox")
#'
#' ## checking structure
#' codebook(simpson)
#'
#' ## tabulate each variables
#' tab(simpson)
#'
#' ## cross tabulate
#' tab(simpson, treatment, by = outcome)
#'
#' ## stratified tabulation
#' # clinic 1
#' simpson %>%
#'     filter(clinic == 1) %>%
#'     tab(treatment, by = outcome)
#'
#' # clinic 2
#' simpson %>%
#'     filter(clinic == 2) %>%
#'     tab(treatment, by = outcome)
#'
#'
#'
#'
#'
#'
#'
#'
#' ### Example from Essential Medical Statistics
#' # Page 178, Chapter 18: Controlling for confounding: Stratification
#'
#' lepto <- expandTables(
#'     male = c(36, 14, 50, 50), female = c(24, 126, 10, 90),
#'     exp_name = "area", exp_lvl = c("Rural", "Urban"),
#'     case_name = "ab", case_lvl = c("Yes", "No"),
#'     strata_name = "gender"
#' ) %>%
#'     labelData("Prevalence survey of leptospirosis in West Indies") %>%
#'     labelVar(c(area, ab, gender),
#'               c("Type of area", "Leptospirosis Antibodies",
#'                 "Gender: Male or female"))
#'
#' ## checking structure
#' codebook(lepto)
#'
#' ## tabulate area and ab
#' tab(lepto, area, by = ab)
#'
#' ## stratified analysis
#' lepto %>%
#'     filter(gender == "male") %>%
#'     tab(area, by = ab)
#'
#' lepto %>%
#'     filter(gender == "female") %>%
#'     tab(area, by = ab)
#' }
#'
#' @export
expandTables <- function( ... ,
                          exp_name = "exposure",
                          exp_lvl = c("exposed", "unexposed"),
                          case_name = "outcome",
                          case_lvl = c("disease", "healthy"),
                          strata_name = "strata",
                          stringsAsFactors = FALSE)
{
    # get vectors within three dots
    .vec <- list(...)
    .vec.len <- length(.vec)

    ## calculate strata
    .strata.names <- sapply(1:length(.vec), function(z) names(.vec[z]))
    .strata.times <- sapply(.vec, function(z) sum(z))

    ## create data.frame

    .exp <- rep(exp_lvl, each = 2)
    .case <- rep(case_lvl, times = 2)

    ## create data frame
    .data <- do.call(
        rbind,
        lapply(1:length(.vec), function(z) {
            .times <- unlist(.vec[z])
            .vec.exp <- rep(.exp, .times)
            .vec.case <- rep(.case, .times)
            .vec.strata <- rep(names(.vec[z]), sum(.times))

            if (is.null(.vec.strata)) {
                .df <- data.frame(.vec.exp, .vec.case,
                                  stringsAsFactors = stringsAsFactors)
                names(.df) <- c(exp_name, case_name)
            } else {
                .df <- data.frame(.vec.exp, .vec.case, .vec.strata,
                                  stringsAsFactors = stringsAsFactors)
                names(.df) <- c(exp_name, case_name, strata_name)
            }
            .df
        })
    )

    ## print
    printMsg("Converted to Dataset")
    # codebook(.data)

    return(.data)
}
