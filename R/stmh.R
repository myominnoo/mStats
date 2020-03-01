#' @title Calculate Incidence Rate Ratios from time-to-event data
#'
#' @description
#' \code{stmh} calculates incidence rate ratios (IRR) of the cohort.
#'
#' @param data an optional data frame
#' @param time specify the timer interval when the subject is at risk
#' @param status event of interest to calculate incidence: 1 for event, 0 for censored
#' @param by specify variable to calculate stratified rates
#' @param fail failure event
#' @param ref_value a number or character representing reference value
#' @param strata variable to stratify by Mantel-Haenszel method
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
#' \strong{Rate Ratios}
#'
#' is calcluated using the following formula:
#' \deqn{RR = Rate of Exposured Group / Rate of Non-Exposed Group}
#'
#' \strong{Confidence interval of RR}
#'
#' is derived using the following formula:
#'
#' \deqn{95\% CI (RR) = Rate x Error Factor}
#'
#' \deqn{Error Factor (rate) = exp(1.96 / \sqrt{1/D1 + 1/D2})}
#'
#' \strong{Test of null hypothesis}
#'
#' is calculated as follows:
#'
#' \deqn{E1 = ( D * Y1/Y)}
#'
#' where D = (D0 + D1) is the total number of events, and
#' Y= (Y0 + Y1) is the total follow-up time.
#'
#' The test is based on the difference between the observed number of
#' events in the exposed group and its expected value, D1 – E1 , which
#' we denote by U. The variance of U, this difference, is equal to:
#'
#' \deqn{V = D \* (Y1/Y) \* ( 1 – (Y1/Y) )}
#'
#' \strong{The square of the standardized difference}
#'
#' \deqn{U^2 / V}
#'
#' which is referred to the χ2 distribution with 1 degree of freedom (df).
#'
#' \code{plot}, if \code{TRUE}, produces a graph of the rate against
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
#' \code{\link{strate}}, \code{\link{mhodds}}, \code{\link{tabOdds}},
#' \code{\link{tabRisks}}
#'
#' @keywords
#'
#' Rate Ratio, Rates, Incidence rates
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
#' library(lung)
#' data(lung)
#' str(lung)
#'
#' ## incidence rates
#' strate(lung, time, status)
#'
#' ## stratified incidence rates
#' strate(lung, time, status, sex, per = 100000)
#' stmh(lung, time, status, sex, per = 100000)
#'
#' ## create ageband
#' lung1 <- egen(lung, age, c(56, 61, 71), var_new = ageband)
#' tab(lung1, ageband)
#'
#' strate(lung1, time, status, ageband, per = 100000)
#' stmh(lung1, time, status, ageband, per = 100000)
#'
#' ## Changing reference value
#' stmh(lung1, time, status, ageband, per = 100000,
#'      ref_value = "lbl[71-82]")
#'
#' data(pbc)
#' str(pbc)
#' strate(pbc, time, status, trt, per = 100000)
#' stmh(pbc, time, status, trt, per = 100000)
#'
#' strate(pbc, time, status, sex, per = 100000)
#' stmh(pbc, time, status, sex, per = 100000)
#' }

#' @export
stmh <- function(data = NULL, time, status, by, fail = 1,
                 ref_value = NULL, strata = NULL,
                 per = 1, rnd = 4,
                 plot = TRUE,
                 print.table = TRUE)
{
  arguments <- as.list(match.call())
  if (!is.null(data)) {
    time <- eval(substitute(time), data)
    status <- eval(substitute(status), data)
    by <- eval(substitute(by), data)
    strata <- eval(substitute(strata), data)
  }

  if (is.null(strata)) {
    x <- as.character()
  } else {
    x <- list()
  }

  UseMethod("stmh", x)
}

#' @rdname stmh
#' @export
stmh.character <- function(data = NULL, time, status, by, fail = 1,
                 ref_value = NULL, strata = NULL,
                 per = 1, rnd = 4,
                 plot = TRUE,
                 print.table = TRUE)
{
  arguments <- as.list(match.call())
  if (!is.null(data)) {
    time <- eval(substitute(time), data)
    status <- eval(substitute(status), data)
    by <- eval(substitute(by), data)
    strata <- eval(substitute(strata), data)
  }
  status.name <- as.character(arguments$status)
  by.name <- as.character(arguments$by)

  t <- strate(data = NULL, time, status, by, fail, per, rnd,
         plot = FALSE, print.table = FALSE)
  lvl <- row.names(t)


  if (is.null(ref_value)) {
    ref_value <- lvl[1]
    ref_lvl <- lvl %in% ref_value
  } else {
    ref_lvl <- lvl %in% ref_value
    if (!any(ref_lvl)) {
      stop(" >>> Wrong Reference Value <<< ")
    }
  }

  if (length(lvl) == 2) {
    rr <- calc_RR(t, ref_lvl, rnd)
  } else {
    rr <- do.call(
      rbind,
      lapply(1:nrow(t), function(z) {
        if (row.names(t)[z] != ref_value) {
          t.loop <- rbind(t[ref_lvl, ],
                          t[z, ])
          calc_RR(t.loop, c(TRUE, FALSE), rnd)
        }
      })
    )
  }

  f <- as.data.frame(cbind(
    as.numeric(as.character(t[ref_lvl, 3])),
    "Reference", "Reference", "", "", ""
  ))
  names(f) <- c("Incidence Rate", "Rate Diff.",
                "Rate Ratio", "[95% Conf.", "Interval]",
                "P>chi2")

  row.names(f) <- row.names(t[ref_lvl, ])

  f <- rbind(f, rr)

  if (print.table) {
    printText(
      f,
      paste0("Maximum likelihood estimates of Rate Ratios\n",
             "Comparing '", status.name, "' across '", by.name, "'"),
      split = "Comparing"
    )

    if (!is.null(attr(status, "label")) |
        !is.null(attr(by, "label"))) {
      printMsg("Labels:")
      printMsg(paste0(by.name, ": ",
                      attr(by, "label"), collapse = ""))
      printMsg(paste0(status.name, ": ",
                      attr(status, "label"), collapse = ""))
    }
  }

  invisible(f)
}

#' @rdname stmh
#' @export
stmh.list <- function(data = NULL, time, status, by, fail = 1,
                      ref_value = NULL, strata = NULL,
                      per = 1, rnd = 4,
                      plot = TRUE,
                      print.table = TRUE)
{
  arguments <- as.list(match.call())
  if (!is.null(data)) {
    time <- eval(substitute(time), data)
    status <- eval(substitute(status), data)
    by <- eval(substitute(by), data)
    strata <- eval(substitute(strata), data)
  }
  time.name <- as.character(arguments$time)
  status.name <- as.character(arguments$status)
  by.name <- as.character(arguments$by)
  strata.name <- as.character(arguments$strata)

  t <- strate(data = NULL, time, status, by, fail, per, rnd,
              plot = FALSE, print.table = FALSE)
  lvl <- row.names(t)

  if (length(lvl) > 2) {
    stop(" >>> Only Stratified 2x2 tables are supported. <<< ")
  }

  if (is.null(ref_value)) {
    ref_value <- lvl[1]
    ref_lvl <- lvl %in% ref_value
  } else {
    ref_lvl <- lvl %in% ref_value
    if (!any(ref_lvl)) {
      stop(" >>> Wrong Reference Value <<< ")
    }
  }

  strata.lvl <- sort(unique(strata))

  f <- do.call(
    rbind,
    lapply(strata.lvl, function(z) {
      t.time <- time[strata == z]
      t.status <- status[strata == z]
      t.by <- by[strata == z]

      v <- strate(time = t.time, status = t.status, by = t.by,
                  fail = fail, per = per, rnd = rnd,
                  plot = FALSE, print.table = FALSE)
      D0 <- as.numeric(as.character(v[1,1]))
      Y0 <- as.numeric(as.character(v[1,2]))
      D1 <- as.numeric(as.character(v[2,1]))
      Y1 <- as.numeric(as.character(v[2,2]))
      RR = (D1 * Y0) / (D0 * Y1)
      Y <- Y0 + Y1
      D <- D0 + D1
      E1 <- D * (Y1 / Y)
      U <- D1 - E1
      V <- E1 * (1 - (Y1 / Y))
      SE <- exp(1.96 * sqrt( (1/D1) + (1/D0)))
      cbind(D0, Y0, D1, Y1, Y,
            Q = D1 * Y0 / Y,
            R = D0 * Y1 / Y,
            RR,
            U,
            V,
            lower = RR / SE,
            upper = RR * SE,
            pvalue = pchisq(U^2/V, 1, lower.tail = FALSE))
    })
  )
  row.names(f) <- strata.lvl
  t <- rbind(f, Total = colSums(f))

  V <- t["Total", "V"]
  Q <- t["Total", "Q"]
  R <- t["Total", "R"]
  mhRR <- Q / R
  chi <- t["Total", "U"] ^ 2 / V
  chi <- pchisq(chi, df = 1, lower.tail = FALSE)
  EF <- exp(1.96 * sqrt(V / (Q * R)))

  f <- as.data.frame(f[, c("R", "RR", "lower", "upper", "pvalue")])
  f[, 1] <- sprintf(f[, 1], fmt = paste0('%#.', rnd,'f' ))
  f[, 2] <- sprintf(f[, 2], fmt = paste0('%#.', rnd,'f' ))
  f[, 3] <- sprintf(f[, 3], fmt = paste0('%#.', rnd,'f' ))
  f[, 4] <- sprintf(f[, 4], fmt = paste0('%#.', rnd,'f' ))
  f[, 5] <- sprintf(f[, 5], fmt = paste0('%#.', 5,'f' ))
  mh <- c(sprintf(c(RR = mhRR,
                    lower = mhRR / EF,
                    upper = mhRR * EF), fmt = paste0('%#.', rnd,'f' )),
          sprintf(c(pvalue = chi), fmt = paste0('%#.', 5,'f' )))

  f <- rbind(f, "MH-RR" = c(R = "Overall", mh))
  names(f) <- c("Incidence Rate", "Rate Ratio",
                "[95% Conf.", "Interval]", "P>chi2")

  if (print.table) {
    printText(
      f,
      paste0("Overall and Stratified estimates of Rate Ratios\n",
             "Comparing '", status.name, "' across '", by.name,
             "'\nStratified by '", strata.name, "'"),
      split = "Comparing"
    )

    if (!is.null(attr(status, "label")) |
        !is.null(attr(by, "label"))) {
      printMsg("Labels:")
      printMsg(paste0(status.name, ": ",
                      attr(status, "label"), collapse = ""))
      printMsg(paste0(by.name, ": ",
                      attr(by, "label"), collapse = ""))
      printMsg(paste0(strata.name, ": ",
                      attr(strata, "label"), collapse = ""))
    }
  }

  if (!is.null(strata) & plot) {
    rr <- as.numeric(as.character(f[, 2]))
    lower <- as.numeric(as.character(f[, 3]))
    upper <- as.numeric(as.character(f[, 4]))
    by <- as.factor(row.names(f))
    plot(by, rr, ylim = c(0, max(upper)),
         main = paste0("Incidence Rate Ratios of '", by.name,
                       "' \nstratified by '", strata.name, "'"),
         xlab = by.name,
         ylab = paste0("Incidence Rates per ", per, " Person-Year(s)"))
    nrow_by <- nrow(f)
    segments(1:nrow_by, rr, 1:nrow_by, upper, col = "blue", lwd = 2)
    segments(1:nrow_by, rr, 1:nrow_by, lower, col = "blue", lwd = 2)
    abline(h = f["MH-RR", "Rate Ratio"], col = "red", lty = 2)
  }

  invisible(f)
}


#' @param t 2x2 table input to calaculate odds ratio of non-reference
#' @param ref_lvl logical vector to indicate where reference category is
#' @rdname stmh
#' @export
calc_RR <- function(t, ref_lvl, rnd) {
  ir <- as.numeric(as.character(t[, 3]))
  rate_ref <- ir[ref_lvl]
  rate_nonRef <- ir[!ref_lvl]

  rr <- rate_nonRef / rate_ref
  rd <- rate_nonRef - rate_ref
  D <- as.numeric(as.character(t[, 1]))
  Y <- as.numeric(as.character(t[, 2]))
  ef <- exp( 1.96 * sqrt( (1/D[1]) + (1/D[2])))

  Y1Y <- Y[!ref_lvl] / sum(Y)
  U <- D[!ref_lvl] - (sum(D) * Y1Y)
  V <- (sum(D) * Y1Y * (1 - Y1Y))
  Q <- U^2 / V
  p.value <- pchisq(q = Q, df = 1, lower.tail = FALSE)

  f <- as.data.frame(cbind(
    rate_nonRef,
    sprintf(rd, fmt = paste0('%#.', rnd, 'f')),
    sprintf(rr, fmt = paste0('%#.', rnd, 'f')),
    sprintf(rr / ef, fmt = paste0('%#.', rnd, 'f')),
    sprintf(rr * ef, fmt = paste0('%#.', rnd, 'f')),
    sprintf(p.value, fmt = paste0('%#.', 5, 'f'))
  ))
  names(f) <- c("Incidence Rate", "Rate Diff.",
                "Rate Ratio", "[95% Conf.", "Interval]",
                "P>chi2")
  row.names(f) <- row.names(t)[!ref_lvl]
  return(f)
}

