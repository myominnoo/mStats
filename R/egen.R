#' @title An extension to \code{generate}
#'
#' @description
#' \code{egen()} transforms a numeric vector to a factor vector.
#'
#' @param data Dataset
#' @param var Exisitng variable
#' @param cut either a single number or a numeric vector.
#' @param new_var Name of new variable without double quote
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
#'
#' \strong{Automatic naming new variable}
#'
#'
#' If \code{new_var} is not specified, new names will be automatically
#' created in this format: \code{VAR_cat}
#'
#' \strong{Automatic Labelling}
#'
#'
#' If \code{lbl} is not specified, labels are constructed in
#' this format: \code{lbl[##-##]}
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
#'
#' data(infert)
#'
#' ## transform continuous to category
#' infert.new <- egen(infert, age, c(20, 30, 40, 50))
#' tab(infert.new, age_cat)
#'
#' ## specifiy labels and name
#' infert.new <- egen(infert, age, c(20, 30, 40),
#'     age_grp, c("<30", "30-39", "40+"))
#' tab(infert.new, age_grp)
#'
#'
#' @export
egen <- function(data, var, cut = NULL, new_var = NULL, lbl = NULL)
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
        stop("`.data` must be a data.frame", call. = FALSE)
    }


    ## create new var's name
    .vars_names <- names(.data)
    new_var <- .args$new_var
    new_var <- ifelse(is.null(new_var),
                      paste0(.args$var, "_cat"),
                      paste(new_var))

    ## If variable is already in the dataset, stop
    if (any(.vars_names %in% new_var)) {
        stop(paste0("'", new_var, "' already exisits."),
             call. = TRUE)
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

    ## remove duplicated category
    .brk <- .brk[!duplicated(.brk)]

    ## check decimals
    .decimal <- checkDecimals(cut)

    ## change numbers to decimal values
    .brk <- c(floor(.brk[1]),
              round(.brk[-c(1, length(.brk))], .decimal),
              ceiling(.brk[length(.brk)]))

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

    ## assign .var back to data, add label and change the names
    .data$new_var <- .var
    attr(.data$new_var, "label") <- paste0(.args$var, " categories")
    names(.data)[ncol(.data)] <- new_var


    ## Display message to nofity changes
    .var.na <- is.na(.var)
    .txt <- paste0(length(.var[!.var.na]), " valid")
    if (any(.var.na)) {
        printText(paste0("'", new_var, "' generated with ",
                         .txt, " + ",
                         length(.var[.var.na]), " <NA> values"))
    } else {
        printText(paste0("'", new_var, "' generated ", .txt,
                         " values"))
    }

    return(.data)
}

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

