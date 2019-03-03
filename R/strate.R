#' @title Tabulate and stratify incidence rates 
#'
#' @description
#' \code{\link{strate}} calculates incidence rates of the cohort. Rates of event 
#' occurrences, known as incidence rates are outcome measures in longitudinal studies. 
#' Calculation is based on the method descibed by Kirkwood & Sterne: 
#' "Essential Medical Statistics: Betty R. Kirkwood & Jonathan A.C. Sterne" Second 
#' Edition, Part D, Chapter 22, page 229 & 238. 
#' @param time specify the timer interval when the subject is at risk
#' @param event event of interest to calculate incidence
#' @param data an optional data frame 
#' @param strata specify variable to calculate stratified rates
#' @param fail failure event
#' @param perPY units to be used in reported rates
#' @param rnd Rounding of numbers 
#' @seealso \code{\link{isum}}
#' @keywords tabulation, frequency table, cross-tabulation, contigency table
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' ## use diet data from PaulDickman
#' ## Source: \url{http://www.pauldickman.com/survival/stataintro.pdf}
#' 
#' diet <- foreign::read.dta("http://www.pauldickman.com/survival/diet.dta")
#' diet$suryear <- as.numeric(diet$dox - diet$doe) / 365.25
#' 
#' ## incidence rates
#' strate(suryear, chd, data = diet)
#' strate(suryear, chd, data = diet, perPY = 1000)
#' 
#' ## stratified incidence rates
#' strate(suryear, chd, diet, job, perPY = 1000) # strata
#' strate(suryear, chd, diet, hieng, perPY = 1000) #strata

#' @export
strate <- function(time, event, data = NULL, strata = NULL, fail = NULL, 
                     perPY = 1, rnd = 4)
{
  if (!is.null(data)) {
    arguments <- as.list(match.call())
    time <- eval(substitute(time), data)
    event <- eval(substitute(event), data)
    strata <- eval(substitute(strata), data)
    e.lbl <- arguments$event
    s.lbl <- arguments$strata
    
  } else {
    e.lbl <- deparse(substitute(event))
    s.lbl <- deparse(substitute(strata))
  }

  if (is.null(fail)) fail <- 1
  
  if (is.null(strata)) {
    ir <- rate(time, event, fail, perPY, rnd, as.character(e.lbl))
    names(dimnames(ir)) <- c("", "")
  } else {
    lvl <- as.character(sort(unique(strata)))
    ir <- do.call(rbind, lapply(lvl, function(z)
      rate(time[strata == z], event[strata == z], fail, perPY, rnd, z)))
    names(dimnames(ir)) <- c(s.lbl, "")
  }
  cat(paste0('\nEstimated rates (per ', perPY, ' person-years)', 
             ' and \nlower/upper bounds of ', 
             '95% confidence intervals \n', 
             '\t(', length(event),' records included in the analysis)\n'))
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
