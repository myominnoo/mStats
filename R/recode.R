#' @title Recode contents of a variable
#' @description
#' \code{recode} easily manipulates contents of a new variable
#' of a data.frame
#'
#' @param data dataset
#' @param var name of a new variable
#' @param ... recode like this: "old value"-"new value" or 1-2 or NA-"Missing"
#' or "Missing"-NA.
#' @details
#'
#' \code{recode}
#' changes the values of variables including categorical variables
#' according to the rules specified below.
#'
#'
#' \preformatted{labelVar(data, "old value"-"new value", 1-0, NA-"Missing",
#'                        "Missing"-NA)}
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
#' ## use infert data
#' data(infert)
#'
#' ## tabulate induced to check values
#' tab(infert, induced)
#'
#' ## recode induced: 2 into 1
#' infert.new <- recode(infert, induced, 2-1)
#'
#' ## tabulate to check
#' tab(infert.new, induced)
#'
#'
#' ## recode induced: 0 to 3, 1 to 7, 2 to 10
#' infert.new <- recode(infert, induced, 0-3, 1-7, 2-10)
#'
#' ## tabulate to check
#' tab(infert.new, induced)
#'
#'
#' @export
recode <- function(data, var, ... )
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

    ## get variable name and data
    .var_name <- .args$var

    ## check if variable is in the dataset
    if (!(as.character(.var_name) %in% .vars_names)) {
        stop(paste0("Variable '", .var_name, "' not found in the dataset"),
             call. = FALSE)
    }

    ## get variable data
    .var <- .data[[.var_name]]
    .lbl <- attr(.var, "label")
    ## change double to numeric
    if (is.double(.var)) {
        .var <- as.numeric(.var)
    }

    ## get the names within three dots
    .values <- .args[-c(1:3)]

    ## change old values to new values
    lapply(1:length(.values), function(z) {
        .v <- as.character(.values[[z]])

        ## assign old and new values: check missing
        .old <- .v[2]
        .old <- ifelse(.old == "NA", NA, .old)

        .new <- .v[3]
        .new <- ifelse(.new == "NA", NA, ifelse(.new == "NULL", NULL, .new))

        ## check if values are valid
        if (is.na(.old)) {
            .check <- is.na(.var)
        } else {
            .check <- .var == .old
        }

        ## change the values
        if (is.na(.old)) {
            .var[is.na(.var)] <<- .new
        } else {
            .var[.var == .old] <<- .new
        }


        ## fix labels
        tryCatch({
            attr(.var, "labels") -> .attr
            if (is.na(.old)) {
                attr(.var, "labels") <<- c(attr(.var, "labels"), "NA" = .new)
            } else {
                attr(.var, "labels")[.attr == .old] <<- .new
            }

        }, error = function(cnd) {
            # return(NULL)
        })

        ## print text notification
        printText(paste0(sum(.check, na.rm = TRUE), " values of '", .var_name,
                         "' recoded from '", .old, "' into '", .new, "'"))
    })

    ## reassign back to dataset
    attr(.var, "label") <- .lbl
    .data[[.var_name]] <- .var

    return(.data)
}
