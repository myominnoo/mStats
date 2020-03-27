#' @title Recode contents of a variable
#' @description
#' \code{recode} easily manipulates contents of a new variable
#' of a data.frame
#'
#' @param data dataset
#' @param var name of a new variable
#' @param old_values vector
#' @param new_values vector (length of 1 or same with values_old)
#' @details
#'
#' \code{recode}
#' changes the values of variables including categorical variables
#' according to the rules specified below.
#'
#' In case of factor, \code{recode} first converts the vector into character,
#' recodes and then revert back to factor.
#'
#' If data is specified, it returns the whole dataframe with recoded variables.
#'
#' \strong{Sample Inputs for conversion}:
#'
#' Old.value    to    New.value
#'
#'    #        >>>>       #
#'
#' c(#, #)     >>>>    c(#, #)
#'
#' c(#, #)     >>>>       #
#'
#'    #:#      >>>>       #
#'
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
#' ## use infert data
#' data(infert)
#'
#' ## tabulate induced to check values
#' tab(infert, induced)
#'
#' ## recode induced: 1 and 2 into 1
#' infert.new <- recode(infert, induced, c(1, 2), 1)
#'
#' ## tabulate to check
#' tab(infert.new, induced)
#'
#' @export
recode <- function(data, var, old_values, new_values)
{
    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())
    ## get var's name
    .var.name <- as.character(.args$var)


    if (!is.null(data)) {
        var <- eval(substitute(var), data)
    }


    ## if var is factor, say YES to .is.var.factor and convert to character.
    if (is.factor(var)) {
        .is.var.factor <- TRUE
        var <- as.character(var)
    } else {
        .is.var.factor <- FALSE
    }

    .old.values.len <- length(old_values)
    .new.values.len <- length(new_values)

    # if old value == 1, new value should be 1
    # if old value > 1, then new value can be 1 or same as old value
    if (.old.values.len != .new.values.len) {
        if (.new.values.len == 1) {
            new_values <- rep(new_values, .old.values.len)
        } else {
            stop(" ... Inconsistent old and new values ... ")
        }
    }



    ## if var is labelled num, removed labels
    attr(var, "names") <- NULL
    attr(var, "labels") <- NULL

    ## log how many values are being recoded by categories
    changed.values.count <- NULL
    for (i in 1:.old.values.len) {
        if (is.na(old_values[i])) {
            changed.values.count <- c(changed.values.count, length(var[is.na(var)]))
            var[is.na(var)] <- new_values[i]
        } else {
            changed.values.count <- c(changed.values.count,
                                      length(var[var == old_values[i]]))
            var[var == old_values[i]] <- new_values[i]
        }
        printMsg(paste0(changed.values.count[i], " values of '", .var.name,
                        "' recoded from '",
                        old_values[i], "' into '",
                        new_values[i], "'"))
    }

    ## change back to factor
    if (.is.var.factor) {
        var <- factor(var)
    }

    ## asign back to dataset
    data[, .var.name] <- var

    return(data)
}
