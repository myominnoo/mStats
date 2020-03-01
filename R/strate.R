#' @title Calculate Incidence Rates from time-to-event data
#'
#' @description
#' \code{strate} calculates incidence rates and Corresponding 95\% CI.
#'
#' @param data Dataset
#' @param time person-time variable
#' @param status outcome variable: preferrably 1 for event, 0 for censored
#' @param by stratified variable
#' @param fail Specify failure event
#' @param per units to be used in reported rates
#' @param rnd Rounding of numbers
#' @param plot logical value to display plots of rates across a categorical
#' variable
#' @param print.table logical value to display formatted outputs
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
#' \enumerate{
#'   \item Essential Medical Statistics, Betty R. Kirkwood & Jonathan A.C. Sterne,
#'   Second Edition. Chapter 22, page 229 & 239
#' }
#'
#' @seealso
#'
#' \code{\link{stmh}}, \code{\link{mhodds}}, \code{\link{tabOdds}},
#' \code{\link{tabRisks}}
#'
#' @keywords
#'
#' incidence rate
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
#' library(survival)
#' data(lung)
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
#' ## create ageband
#' lung1 <- egen(lung, age, c(56, 61, 71), var_new = ageband)
#' tab(lung1, ageband)
#'
#' strate(lung1, time, status, by = ageband, per = 100000)
#'
#' data(pbc)
#' str(pbc)
#' strate(pbc, time, status)
#' strate(pbc, time, status, trt)
#' strate(pbc, time, status, sex)
#' strate(pbc, time, status, stage, per = 100000)
#' }


#' @export
strate <- function(data = NULL, time, status, by = NULL, fail = 1,
                   per = 1, rnd = 4, plot = TRUE,
                   print.table = TRUE)
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
    by.txt <- paste0("\nby categories of: '", by.name, "'")
    row.names(f) <- by.lvl
  }

  names(f) <- c("Diseased", "Person-Time", "Incidence Rate", "[95% Conf.",
                "Interval]")

  if (print.table) {
    texts <- paste0("Tabulation of failure rates\n", "Failure Event: '",
                    paste0(status.name, collapse = ""), "' == '", fail,
                    "'", by.txt)
    printText(f, texts, "Failure Event: ")
    printMsg("Notes:")
    if (!is.null(by)) {
      printMsg(paste0("categories of ", by.name, ": ",
                      paste0(by.lvl, collapse = " | ")))
    }
    printMsg(paste0("Estimated per ", per, " person-years-at-risk",
                    collapse = ""))
    printMsg(paste0(length(status), " records included in the analysis",
                    collapse = ""))
  }

  if (!is.null(by) & plot) {
    ir <- as.numeric(as.character(f[, 3]))
    lower <- as.numeric(as.character(f[, 4]))
    upper <- as.numeric(as.character(f[, 5]))
    by <- as.factor(row.names(f))
    plot(by, ir, ylim = c(0, max(upper)),
         main = paste0("Incidence rates across '", by.name, "'"),
         xlab = by.name,
         ylab = paste0("Incidence Rates per ", per, " Person-Years"))
    nrow_by <- nrow(f)
    segments(1:nrow_by, ir, 1:nrow_by, upper, col = "blue", lwd = 2)
    segments(1:nrow_by, ir, 1:nrow_by, lower, col = "blue", lwd = 2)
  }

  invisible(f)
}

#' @param t time var
#' @param s status var
#' @param f failure var
#' @param p person-time var
#' @param r rnd var
#' @param v vector
#' @rdname strate
#' @export
strate.calc <- function(t, s, f, p, r, v)
{
  d <- length(s[s == f])
  py <- sum(t, na.rm = TRUE)
  ir <- d / py
  ef <- exp(1.96 * 1 / sqrt(d))
  df <- c(ir * p, (ir / ef) * p, (ir * ef) * p)
  df <- sprintf(df, fmt = paste0('%#.', r,'f' ))
  df <- as.data.frame(matrix(c(d, round(py / p, r), df),
                             ncol = 5, byrow = TRUE))
  return(df)
}
