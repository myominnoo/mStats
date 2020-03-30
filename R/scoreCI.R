#' @title Calculate confidence intervals by the Wilson Score method
#'
#' @description
#'
#' \code{scoreCI()} generates confidence intervals by the
#' Wilson Score method
#'
#' \code{ciCollapse} formats two values in this format
#' \code{(##.# - ##.#)}.
#'
#' @param p proportion
#' @param n sample size
#' @param z confidence level
#' @param correct a logical indicating whether to apply continuity correction
#' @param ci a vector of two values (lower and upper CI values)
#' @param sep separator for line break
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#' @param bracket a logical indicating whether to paste bracket to ci values
#'
#' @details
#'
#' \code{scoreCI}
#'
#' The Wilson score interval is an improvement over the normal approximation
#' interval in that the actual coverage probability is closer to the nominal
#' value. It was developed by Edwin Bidwell Wilson (1927). (Wikipedia)
#'
#' \strong{Reference:}
#' \enumerate{
#'     \item Brown, Lawrence D.; Cai, T. Tony; DasGupta, Anirban (2001). "Interval
#'     Estimation for a Binomial Proportion". Statistical Science. 16 (2): 101–133.
#'     \item Wallis, Sean A. (2013). "Binomial confidence intervals and contingency
#'     tests: mathematical fundamentals and the evaluation of alternative methods"
#'     .Journal of Quantitative Linguistics. 20 (3): 178–208.
#'     \item Newcombe, R. G. (1998). "Two-sided confidence intervals for the single
#'     proportion: comparison of seven methods". Statistics in Medicine. 17 (8):
#'     857–872. doi:10.1002/(SICI)1097-0258(19980430)17:8<857::AID-SIM777>3.0.CO;2-E.
#'     PMID 9595616.
#' }
#'
#' @seealso \code{\link{diagTest}}
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
#' scoreCI(.20, 200)
#' scoreCI(.20, 200, correct = TRUE)
#'
#' @export
scoreCI <- function(p, n, z = 1.96, correct = FALSE)
{
  if (correct) {
    x <- (2 * n * p) + ((z)^2 - 1)
    se <- z * sqrt( z^2 - ( 2 + (1/n) ) + ( 4 * n * p * (1 - p) ) + 1)
    d <- 2 * ( n + z^2 )
  } else {
    x <- (2 * n * p) + (z)^2
    se <- z * sqrt( z^2 + (4 * n * p * (1 - p) ))
    d <- 2 * ( n + z^2 )
  }
  ll <- (x - se) / d
  ul <- (x + se) / d
  return(c(ll, ul))
}


#' @rdname scoreCI
#' @export
ciCollapse <- function(ci, sep = " - ", rnd = 2, bracket = TRUE)
{
  ci <- sprintf(ci, fmt = paste0('%#.', rnd, 'f'))
  ci <- paste0(paste0(ci, collapse = sep), collapse = "")
  if (bracket)
    ci <- paste0("(", ci, ")")
  return(ci)
}
