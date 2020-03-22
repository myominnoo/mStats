#' @title An extension to \code{generate}
#'
#' @description
#' \code{egen()} transforms a numeric vector to a factor vector.
#'
#' @param data Dataset
#' @param var Exisitng variable
#' @param cut either a single number or a numeric vector.
#' @param new_var name of new variable to be generated
#' @param lbl labels to specify
#'
#' @details
#' \code{egen} allows easy conversion of a numerical variable to a categorical
#' variable.
#'
#' \strong{Cut-off Intervals}
#'
#'
#' If the interval is not specified, it is cut at an interval of 10,
#' starting from the minimum value. Otherwise,
#' it is divided into corresponding intervals by specified cut-off points.
#'
#' \strong{Automatic Labelling}
#'
#'
#' If \code{lbl} is not specified, labels are constructed in
#' this format: \code{lbl[##-##]}
#'
#'
#' @concept
#'
#' new variable generate produce create transform conversion transform
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
#' ## generate bmi.cat without cut points
#' egen(hosp, bmi) %>%
#'     tab(bmi.cat)
#'
#'
#' ## generate bmi.cat with cut points
#' egen(hosp, bmi, c(0, 15, 16, 18.5, 25, 30, 35, 40, 500)) %>%
#'     tab(bmi.cat)
#'
#'
#' ## generate bmi.cat with cut points
#' egen(hosp, bmi, c(0, 15, 16, 18.5, 25, 30, 35, 40, 500),
#'      lbl = c("<18.5", "18.5-24", "25-29", "30-34", "35-39", "40+"),
#'      new_var = BMI) %>%
#'     tab(BMI)
#'
#' }
#'
#' @export
egen <- function(data, var, cut = NULL, new_var = NULL, lbl = NULL)
{
    ## NA should not be removed because var is from a dataset

    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    ## assign data into .data for further evaluation
    .data <- data


    ## create new var's name
    .data.names <- names(.data)
    new_var <- .args$new_var
    new_var <- ifelse(is.null(new_var), paste0(.args$var, ".cat"), paste(new_var))
    if (any(.data.names %in% new_var)) {
        stop(paste0(" ... '", new_var, "' already exisits! ... "))
    }

    ## assign var to data
    var <- eval(substitute(var), data)
    ## if cut is not specified, cut <- 10
    if (is.null(cut)) {
        cut <- 10L
    }

    ## create break / cut-off labels
    if (length(cut) > 1) {
        .brk <- cut
    } else {
        .brk <- seq(min(var, na.rm = TRUE),
                    max(var, na.rm = TRUE), cut)
        .brk <- c(.brk[1],
                  .brk[2:(length(.brk)-1)] - 1,
                  .brk[length(.brk)] - 1)
    }


    ## get minimum value, check and add to the cut sequence
    .brk.min <- min(var, na.rm = TRUE)
    .brk <- .brk[.brk.min < .brk]
    if (.brk.min != .brk[1]) {
        .brk <- c(.brk.min, .brk)
    }

    ## get minimum value, check and add to the cut sequence
    .brk.max <- max(var, na.rm = TRUE)
    .brk <- .brk[.brk.max > .brk]
    if (.brk.max != .brk[length(.brk)]) {
        .brk <- c(.brk, ceiling(.brk.max))
    }


    ## check decimals
    .decimal <- checkDecimals(cut)

    ## change numbers to decimal values
    .brk <- round(.brk, .decimal)


    ## construct labels
    if (is.null(lbl)) {
        .last.sec.pos <- length(.brk) - 1
        .lbl <- paste(
            "lbl[", .brk[1:.last.sec.pos], "-",
            c(.brk[2:.last.sec.pos] - (1 / (10 ^ .decimal)),
              .brk[length(.brk)]), "]",
            sep = ""
        )
    } else {
        .lbl <- lbl
    }



    ## create new var
    .var <- cut(var, breaks = as.numeric(.brk),
                labels = .lbl, right = FALSE,
                include.lowest = TRUE)

    ## assign .var back to data and change the names
    .data$new_var <- .var
    names(.data)[ncol(.data)] <- new_var


    ## Display message to nofity changes
    .var.na <- is.na(.var)
    if (any(.var.na)) {
        printMsg(paste0(length(.var[!.var.na]), " real values generated | ",
                        length(.var[.var.na]), " missing values generated"))
    } else {
        printMsg(paste0(length(.var[!.var.na]), " real values generated"))
    }

    return(.data)
}



# Helpers -----------------------------------------------------------------

checkDecimals <- function(x)
{
    ## check if there are any decimal values
    .decimal <- grepl("\\.", x)
    if (any(.decimal)) {
        .decimal <- strsplit(as.character(x[.decimal][1]), "\\.")[[1]][2]
        .decimal <- nchar(.decimal)
    } else {
        .decimal <- 0
    }
    .decimal
}
