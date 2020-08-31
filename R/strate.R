#' @title Calculate Incidence Rates from time-to-event data
#'
#' @description
#' \code{strate()} calculates incidence rates and Corresponding 95\% CI.
#'
#' @param data Dataset
#' @param time person-time variable
#' @param fail outcome variable: preferably 1 for event, 0 for censored
#' @param ... variables for stratified analysis
#' @param fail_value a value or values to specify failure event
#' @param per units to be used in reported rates
#' @param rnd Rounding of numbers
#'
#' @details
#' Rates of event occurrences, known as incidence rates are outcome measures in
#' longitudinal studies. In most longitudinal studies, follow-up times vary due
#' to logistic reasons, different periods of recruitment, delay enrollment into
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
#' Betty R. Kirkwood, Jonathan A.C. Sterne (2006, ISBN:978–0–86542–871–3)
#'
#' @import stats
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
#' ### Using the diet data (Clayton and Hills 1993) described in STATA manual
#' ## import diet data: require haven package to read dta format.
#' ## magrittr package for piping operation
#' # diet <- haven::read_dta("https://www.stata-press.com/data/r16/diet.dta")
#' # diet <- diet %>%
#' #    generate(time, (dox - doe) / 365.25) %>%
#' #    replace(time, as.numeric(time)) %>%
#' #      generate(age, as.numeric(doe - dob) / 365.25) %>%
#' #      egen(age, c(41, 51, 61, 71), ageband)
#' #
#' #  ## check dataset
#' #  codebook(diet)
#' #
#' #  ## calculate overall rates and 95% Confidence intervals
#' #  strate(d, time, fail, fail_value = c(1, 3, 13))
#' #  ## per 100 unit
#' #  strate(d, time, fail, fail_value = c(1, 3, 13), per = 100)
#' #
#' #  ## calculate Stratified rates and 95% Confidence Intervals
#' #  strate(d, time, fail, fail_value = c(1, 3, 13), job)
#' #  strate(d, time, fail, fail_value = c(1, 3, 13), job, ageband)
#' #  ## per 100 unit
#' #  strate(d, time, fail, fail_value = c(1, 3, 13), job, ageband, per = 100)
#'
#'
#' @export
strate <- function(data, time, fail, ... , fail_value = NULL, per = 1, rnd = 4)
{
  ## match call arguments
  .args <- as.list(match.call())

  ## copy data to .data
  .data <- data

  ## get names of dataset and headings
  .data_name <- deparse(substitute(data))
  .vars_names <- names(.data)
  time <- .args$time
  fail <- .args$fail

  ## if input is not a data.frame, stop
  if (!is.data.frame(.data)) {
    stop(paste0("`", .data_name, "` must be a data.frame"),
         call. = FALSE)
  }

  ## get variable names within three dots to search for duplicates
  .vars <- enquotes(.args, c("data", "time", "fail", "fail_value",
                             "per", "rnd"))

  ## check colon, and check data types if the whole dataset
  .vars <- checkEnquos(.data, .vars)

  ## check variables in the dataset
  sapply(c(time, fail, .vars), function(z) {
    if (!any(as.character(z) %in% .vars_names)) {
      stop(paste0("`", z, "` not found."),
           call. = FALSE)
    }
  })

  ## get individual data
  .time <- .data[[time]]
  .fail <- .data[[fail]]


  ## check failure value
  .fail_lvl <- unique(.fail)
  ## if failure value is not specified, the first value is used
  if (is.null(fail_value)) {
    fail_value <- .fail_lvl[1]
  }
  ## check failure values are correct
  sapply(fail_value, function(z) {
    if (!any(z %in% .fail_lvl)) {
      stop(paste0("Failure value `", z, "` not found"),
           call. = FALSE)
    }
  })

  ## if no variable, then overall strate is calculated
  if (length(.vars) == 0) {
    .t <- calcRate(.time, .fail, fail_value, per, rnd)
    .df <- data.frame(rbind(.t))
    .df <- cbind(var = as.character(fail), .df)
    row.names(.df) <- NULL

    ## change headings and add dash lines
    names(.df) <- c("Variables", "Failure",
                    "Person-Years", "Inc. Rate",
                    "[95% Conf.", "Interval]")
    .df <- addDashLines(.df, .vline = 2)
  } else {
    ## calculate stratified rates
    .t <- do.call(
      rbind,
      lapply(.vars, function(z) {
        .var <- .data[[z]]
        .t <- calcSRate(.time, .fail, .var, fail_value, per, rnd)
        .t <- cbind(var = c(z, rep("", nrow(.t) - 1)), .t)
        .t <- addDashLines(.t)
        .t <- .t[-nrow(.t), ]
        .t
      })
    )
    # remove first row
    .df <- .t[-1, ]

    ## change headings and add dash lines
    names(.df) <- c("Variables", "Category", "Failure",
                    "Person-Years", "Inc. Rate",
                    "[95% Conf.", "Interval]")
    .df <- addDashLines(.df, .vline = 3)
  }


  ## print message
  printDF(.df,
          paste0("          Estimated Incidence Rates",
                 " and 95% Confidence Intervals"))

  .obs <- length(.fail)
  printText(paste0(.obs, " records included in the analysis."))


  ## constructs labels
  printLabel(.data, .args$time)
  printLabel(.data, .args$fail)
  .lbl <- sapply(.vars, function(z) attr(.data[[z]], "label"))
  if (length(.lbl) > 0) {
    sapply(1:length(.vars), function(z) {
      printLabel(.data, .vars[z])
    })
  }

  invisible(.df)
}




# helpers -----------------------------------------------------------------


#' @describeIn strate
#'
#' @description
#'
#' \code{calcRate} calculates incidence rate and its
#' confidence intervals.
#'
#' @inheritDotParams strate
#'
#' @export
calcRate <- function(time, fail, fail_value, per, rnd)
{
  ## calculate statistics
  .d <- sum(fail %in% fail_value, na.rm = TRUE)
  .py <- sum(time, na.rm = TRUE)
  .ir <- .d / .py
  .ef <- exp(1.96 * (1/ sqrt(.d)))
  .ir.confint <- c(.ir / .ef, .ir * .ef)
  .df <- sprintf(c(.py / per, c(.ir, .ir.confint) * per),
                 fmt = paste0("%#.", rnd, "f" ))

  .df <- c(.d, .df)
  names(.df) <- c("D", "Y", "R", "LL", "UL")

  return(.df)
}


#' @describeIn strate
#'
#' @description
#'
#' \code{calcSRate} calculates Stratified incidence rate and its
#' confidence intervals.
#'
#' @inheritDotParams strate
#' @param by a variable to specify rates
#'
#' @export
calcSRate <- function(time, fail, by, fail_value, per, rnd)
{
  ## get levels of var, if NA, remove
  .lvl <- unique(by)
  .lvl <- .lvl[!is.na(.lvl)]

  ## calculate rates and statistics
  .df <- do.call(
    rbind,
    lapply(.lvl, function(z) {
      time <- time[by == z]
      fail <- fail[by == z]
      calcRate(time, fail, fail_value, per, rnd)
    })
  )

  ## add levels to df
  .df <- cbind(lvl = .lvl, .df)
  .df <- data.frame(.df)

  return(.df)
}

