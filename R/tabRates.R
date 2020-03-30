#' @title Tabulate Incidence Rates from time-to-event data
#'
#' @description
#' \code{strate()} calculates incidence rates and Corresponding 95\% CI.
#'
#' @param data Dataset
#' @param time person-time variable
#' @param status outcome variable: preferrably 1 for event, 0 for censored
#' @param ... Variable or multiple variables
#' Colon separator \code{:} can be used to specify multiple variables.
#' @param fail Specify failure event
#' @param per units to be used in reported rates
#' @param rnd Rounding of numbers
#' @param plot logical value to display plots of rates across a categorical
#' variable
#'
#'
#' @details
#' Rates of event occurrences, known as incidence rates are outcome measures in
#' longitudinal studies. In most longitudinal studies, follow-up times vary due
#' to logistic resasons, different periods of recruitment, delay enrolment into
#' the study, lost-to-follow-up, immigration or emigration and death.
#'
#' \strong{Follow-up time in longitudinal studies}
#'
#' Period of observation (called as follow-up time) starts when individuals join
#' the study and ends when they either have an outcome of interest, are lost-to-
#' follow-up or the follow-up period ends, whichever happens first. This period is
#' called \strong{person-year-at-risk}. This is denoted by \emph{PY} in \code{strate}
#' function's output and numer of event by \emph{D}.
#'
#' \strong{Rate}
#'
#' is calcluated using the following formula:
#' \deqn{\lambda = D / PY}
#'
#' \strong{Confidence interval of rate}
#'
#' is derived using the following formula:
#'
#' \deqn{95\% CI (rate) = rate x Error Factor}
#' \deqn{Error Factor (rate) = exp(1.96 / \sqrt{D})}
#'
#'
#' \code{plot}, if \code{TRUE}, produces a graph of the rates against
#' the numerical code used for categories of \code{by}.
#'
#'
#' @references
#'
#'
#' Betty R. Kirkwood, Jonathan A.C. Sterne (2006, ISBN:978–0–86542–871–3)
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
#'
#' @export
strate <- function(data, time, status, ... , fail = NULL,
                     per = 1, plot = TRUE, rnd = 3)
{

    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    ## assign data into .data for further evaluation
    .data <- data

    .time <- eval(substitute(time), .data)
    .status <- eval(substitute(status), .data)
    .by <- eval(substitute(by), .data)

    ## variable names


    ## get variable names within three dots to search for duplicates
    .vars <- as.character(enquos(.args, c("data", "time", "status", "fail",
                                          "per", "plot", "rnd")))

    # check colon
    if (any(grepl(":", .vars))) {
        .vars <- do.call(
            c,
            lapply(.vars, function(z) {
                .colon <- grepl(":", z)
                if (.colon) {
                    splitByColon(.data, z, .colon)
                } else {
                    z
                }
            })
        )
    }

    ## process status levels and fail
    if (is.null(fail)) {
        .lvl <- sort(unique(.status))
        .lvl <- .lvl[!is.na(.lvl)]
        .fail <- .lvl[1]
    } else {
        .fail <- fail
    }

    if (length(.vars) == 0) {
        .df <- calcRate(.time, .status, .fail, per, rnd, .args$status)

        .df <- data.frame(.fail, "|", .df, stringsAsFactors = FALSE)
        names(.df) <- c(.args$status, "|", "Events", "Person-times", "Inc. Rate",
                        "[95% Conf.", "Interval]")

        ## add dash lines
        .df <- addDashLines(.df, .vLine = 2)
        .df <- list(.df)
    } else {
        .df <- lapply(.vars, function(z) {
            .temp <- .data[[z]]
            .by.lvl <- sort(unique(.temp))
            .df <- do.call(
                rbind,
                lapply(.by.lvl, function(y) {
                    .equal <- .temp == y
                    .df <- calcRate(.time[.equal], .status[.equal], .fail,
                                    per, rnd, .args$status)
                    data.frame(as.character(y), "|", .df, stringsAsFactors = FALSE)
                })
            )
            names(.df) <- c(z, "|", "Events", "Person-times", "Inc. Rate",
                            "[95% Conf.", "Interval]")

            ## add dash lines
            addDashLines(.df, .vLine = 2)
        })
    }


    ## constructs labels
    if (length(.vars) == 0) {
        .vars <- 1:length(.df)
    }
    sapply(1:length(.vars), function(z) {
        printText2(.df[[z]],
                   paste0("Estimates of Incidence Rate of '", .args$status, "'"),
                   .printDF = TRUE)
        getnPrintLabel(.data, .args$time)
        getnPrintLabel(.data, .args$status)
        getnPrintLabel(.data, .vars[z])
    })

    invisible(.df)
}


# Helpers -----------------------------------------------------------------


## Essential medical Statistics page 238
calcRate <- function(.time, .status, .fail, .per, .rnd, .status.name)
{
    ## number of events
    .d <- length(.status[.status == .fail])

    ## person-times at risk
    .pt <- sum(.time, na.rm = TRUE)

    ## calculate incidence rate
    .rate <- .d / .pt

    ## error factor
    .ef <- exp(1.96 * 1 / sqrt(.d))
    .ll <- .rate / .ef
    .ul <- .rate * .ef
    .df <- data.frame(.d,
                      sprintf(.pt / .per, fmt = paste0('%#.', .rnd,'f' )),
                      sprintf(.rate * .per, fmt = paste0('%#.', .rnd,'f' )),
                      sprintf(.ll * .per, fmt = paste0('%#.', .rnd,'f' )),
                      sprintf(.ul * .per, fmt = paste0('%#.', .rnd,'f' )),
                      stringsAsFactors = FALSE)
    names(.df) <- c("Events", "Person-times", "Inc. Rate",
                    "[95% Conf.", "Interval]")
    .df
}

