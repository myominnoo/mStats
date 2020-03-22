#' @title Add or replace labels of variables and dataset
#'
#' @description
#'
#' \code{labelVar} labels \code{variables}.
#'
#' \code{labelData} labels \code{Dataset}.
#'
#' @param data dataset
#' @param var one variable or variables
#' @param lbl to specify labels. If not specified, label is removed
#'
#' @details
#'
#' Labels are useful to provide more detailed information about
#' variables or dataset. Many functions in \code{mStats} package
#' extract label information and display them as footnote.
#'
#' \strong{Label single or multiple variables}
#'
#' Single or multiple variables can be labelled.
#'
#' Example: single variable
#'
#' \preformatted{labelVar(data, var, lbl)}
#'
#' Example: multiple variable
#'
#' \preformatted{labelVar(data,
#'       c(var1, var2, var3),
#'       c(lbl1, lbl2, lbl3))}
#'
#' \strong{Label Dataset}
#'
#' Dataset can also be labelled and displayed in \code{codebook}.
#'
#' \preformatted{labelData(data, lbl)}
#'
#'
#' @references
#'
#' LABELING DATA | STATA LEARNING MODULES. UCLA: Statistical Consulting Group.
#'
#' from https://stats.idre.ucla.edu/stata/modules/labeling-data/
#' (accessed March 19, 2016).
#'
#' @concept
#'
#' data.frame label data label data frame label
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
#' \dontrun{
#'
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
#' ## rename test1 to il6 and label
#' hosp %>%
#'     rename(test1, il6) %>%
#'     labelVar(il6, "Concentration of interleukin 6") %>%
#'     codebook
#'
#'
#' ## multiple variables
#' # rename test1 and test2 to il6 and crp and label
#' hosp %>%
#'     rename(test1, il6) %>%
#'     rename(test2, crp) %>%
#'     labelVar(c(il6, crp),
#'              c("Concentration of interleukin 6", "C-Reactive Protein (CRP)")
#'     ) %>%
#'     codebook
#'
#'
#' ## remove label
#' hosp %>%
#'     labelVar(c(age, married, familyhx, smokinghx, sex)) %>%
#'     codebook
#'
#'
#' ## remove labels using colon separators
#' hosp %>%
#'     labelVar(c(age:sex)) %>%
#'     codebook
#'
#'
#' ## label dataset
#' labelData(hosp, "Fake cancer patient data for demonstration") %>%
#'     codebook
#'
#'
#'
#' # Example from IDRE UCLA
#' path <- "https://stats.idre.ucla.edu/stat/stata/modules/autolab.dta"
#' auto <- haven::read_dta(path)
#' codebook(auto)
#'
#'
#' ## to use piping function
#' library(magrittr)
#'
#' ## label dataset
#' labelData(auto, "This file contains auto data for the year 1978") %>%
#'     codebook
#'
#'
#' ## label variables
#' auto %>%
#'     labelVar(rep78, "the repair record from 1978") %>%
#'     codebook
#'
#'
#' auto %>%
#'     ## add labels
#'     labelVar(c(rep78, price, mpg, foreign),
#'              c("the repair record from 1978", "the price of the car in 1978",
#'                "the miles per gallon for the car",
#'                "the origin of the car, foreign or domestic")) %>%
#'     ## remove labels
#'     labelVar(c(headroom, trunk, weight, length, turn, displacement,
#'                gear_ratio)) %>%
#'     codebook
#' }
#'
#' @export
labelVar <- function(data, var, lbl = NULL)
{

    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    ## assign data into .data for further evaluation
    .data <- data


    ## numbers of vars and lbls
    .var <- as.character(.args$var)
    .var.len <- length(.var)
    if (.var.len > 1) {
        .var <- .var[-1]
        .var.len <- length(.var)
    }
    ## Check if colon is there.
    ## if present, retrieve variables between the two variables
    if (any(grepl(":", .var))) {
        .var <- do.call(
            c,
            lapply(.var, function(z) {
                .colon <- grepl(":", z)
                if (.colon) {
                    splitByColon(data, z, .colon)
                } else {
                    z
                }
            })
        )
        .var.len <- length(.var)
    }


    ## check labels
    .lbl <- lbl
    .lbl.len <- ifelse(is.null(lbl), 1, length(lbl))

    if (.var.len != .lbl.len) {
        if (is.null(lbl)) {
            .lbl <- rep(NULL, .var.len)
        } else {
            stop(" ... Variable and its label specify different length! ... ")
        }
    }


    ## loop and assign each variable to corresponding labels
    for (i in 1:.var.len) {
        .txt <- paste0(".data[, '", .var[i], "']")
        attr(.data[[.var[i]]], "label") <- .lbl[i]
        printMsg(paste0("Variable '", .var[i],
                        "' labelled as '",
                        ifelse(is.null(.lbl[i]), "<NA>", .lbl[i]), "'"))
    }

    return(.data)
}




#' @rdname labelVar
#' @export
labelData <- function(data, lbl = NULL)
{

    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    attr(data, "label") <- lbl
    printMsg(paste0("Dataset '", .args$data,
                    "' labelled as '",
                    ifelse(is.null(lbl), "<NA>", lbl), "'"))
    return(data)
}
