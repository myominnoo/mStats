#' @title Calculate incidence rates from time-to-event data
#'
#' @description
#' \code{strate} calculates incidence rates of the cohort.
#'
#' @param data an optional data frame
#' @param time specify the timer interval when the subject is at risk
#' @param status event of interest to calculate incidence: 1 for event, 0 for censored
#' @param by specify variable to calculate stratified rates
#' @param fail failure event
#' @param per units to be used in reported rates
#' @param rnd Rounding of numbers
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
#' \strong{References:}
#' \enumerate{
#'   \item Essential Medical Statistics, Betty R. Kirkwood & Jonathan A.C. Sterne,
#'   Second Edition. Chapter 22, page 229 & 239
#' }
#' @seealso \code{\link{summ}}, \code{\link{summBy}}, \code{\link{tab}},
#' \code{\link{xtab}}
#' @keywords incidence rate
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#' library(survival)
#' data(lung)
#' str(lung)
#'
#' ## incidence rates
#' strate(lung, time, status)
#' strate(lung, time, status, fail = 2)
#'
#' ## stratified incidence rates
#' strate(lung, time, status, by = sex)
#' strate(lung, time, status, by = sex, fail = 2)
#' strate(lung, time, status, by = sex, fail = 2, per = 1000)
#' strate(lung, time, status, by = sex, fail = 2, per = 100000, rnd = 1)
#' strate(time, status, data = lung, per = 100000)
#'
#' data(pbc)
#' str(pbc)
#' strate(pbc, time, status)
#' strate(pbc, time, status, trt)
#' strate(pbc, time, status, sex)
#' strate(pbc, time, status, stage, per = 100000)
#' }


#' @export
strate <- function(data = NULL, time, status, by = NULL, fail = NULL,
                   per = 1, rnd = 5)
{
  arguments <- as.list(match.call())
  if (!is.null(data)) {
    time <- eval(substitute(time), data)
    status <- eval(substitute(status), data)
    by <- eval(substitute(by), data)
  }
  status.name <- as.character(arguments$status)
  by.name <- as.character(arguments$by)

  if (is.null(fail)) {
    lvl <- unique(status)
    lvl <- lvl[!is.na(lvl)]
    fail <- lvl[length(lvl)]
  }

  if (is.null(by)) {
    f <- strate.calc(time, status, fail, per, rnd, status.name)
    names(dimnames(f)) <- c("", "")
    by.txt <- NULL
  } else {
    by.lvl <- as.character(sort(unique(by)))
    f <- do.call(
      rbind,
      lapply(by.lvl, function(z)
        strate.calc(time[by == z], status[by == z], fail, per, rnd, z))
    )
    names(dimnames(f)) <- c(by.name, "")
    by.txt <- paste0("\nby categories of: ", by.name)
  }

  names(f) <- c("Event", "Person Year", "Inc Rate (IR)", "Lower CI", "Upper CI")

  texts <- paste0("Tabulation of failure rates\n", "failure: ",
                  paste0(status.name, collapse = ""), " == ", fail,
                  by.txt)
  printText(f, texts, "failure: ")
  printMsg("Notes:")
  if (!is.null(by)) {
    printMsg(paste0("categories of ", by.name, ": ",
                    paste0(by.lvl, collapse = " | ")))
  }
  printMsg(paste0("Estimated per ", per, "person-years-at-risk",
                  collapse = ""))
  printMsg(paste0(length(status), " records included in the analysis",
                  collapse = ""))

  invisible(f)
}


strate.calc <- function(t, s, f, p, r, v)
{
  d <- length(s[s == f])
  py <- sum(t, na.rm = TRUE)
  ir <- d / py
  ef <- exp(1.96 * 1 / sqrt(d))
  df <- c(ir * p, (ir / ef) * p, (ir * ef) * p)
  df <- sprintf(df, fmt = paste0('%#.', r,'f' ))
  df <- as.data.frame(matrix(c(d, py / p, df), ncol = 5, byrow = TRUE))
  return(df)
}
