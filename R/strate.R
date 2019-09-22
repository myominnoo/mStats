#' @title Calculate incidence rates from time-to-event data
#'
#' @description
#' \code{strate} calculates incidence rates of the cohort.
#'
#' @param time specify the timer interval when the subject is at risk
#' @param status event of interest to calculate incidence: 1 for event, 0 for censored
#' @param data an optional data frame
#' @param strata specify variable to calculate stratified rates
#' @param fail failure event
#' @param perPY units to be used in reported rates
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
#' data(lung)
#' str(lung)
#'
#' ## incidence rates
#' strate(time, status, data = lung)
#' strate(time, status, data = lung, per = 100000)
#'
#' ## stratified incidence rates
#' lung$sex <- factor(lung$sex, levels = c(1:2), labels = c('male', 'female'))
#' strate(time, status, sex, lung, per = 100000)
#' strate(time, status, inst, lung, per = 100000)
#' }

#' @export
strate <- function(time, status, strata = NULL, data = NULL, fail = NULL,
                     perPY = 1, rnd = 4)
{
  if (!is.null(data)) {
    arguments <- as.list(match.call())
    time <- eval(substitute(time), data)
    status <- eval(substitute(status), data)
    strata <- eval(substitute(strata), data)
    e.lbl <- arguments$status
    s.lbl <- arguments$strata

  } else {
    e.lbl <- deparse(substitute(status))
    s.lbl <- deparse(substitute(strata))
  }

  if (is.null(fail)) fail <- 1

  if (is.null(strata)) {
    ir <- rate(time, status, fail, perPY, rnd, as.character(e.lbl))
    names(dimnames(ir)) <- c("", "")
  } else {
    lvl <- as.character(sort(unique(strata)))
    ir <- do.call(rbind, lapply(lvl, function(z)
      rate(time[strata == z], status[strata == z], fail, perPY, rnd, z)))
    names(dimnames(ir)) <- c(s.lbl, "")
  }
  cat(paste0('\nNote: Estimated rates (per ', perPY, ' person-years-at-risk)',
             '\n      lower/upper bounds of 95% confidence intervals \n',
             '      (', length(status),' records included in the analysis)\n'))
  return(ir)
}

rate <- function(t, e, f, p, r, v)
{
  d <- length(e[e == f])
  py <- sum(t, na.rm = TRUE)
  ir <- d / py
  ef <- exp(1.96 * 1 / sqrt(d))
  df <- round(cbind(D = d, PY = py / p, Rate = ir * p, Lower = (ir / ef) * p,
                    Upper = (ir * ef) * p), r)
  row.names(df) <- v
  return(df)
}
