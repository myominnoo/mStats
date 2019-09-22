#' @title Screening tests
#' @description
#' \code{screening} reports sensitivity, specificity, positive and negative predictive
#' values
#' @param x a factor object or a table
#' @param y an optional factor object
#' @param p disease prevalence
#' @details
#' The screening tests are based on Bayes' Theorem. These tests help clinicians to
#' correctly predict the presence or absence of a particular disease from the
#' knowlege of test results (positive or negative) and/or the status of presenting
#' symptoms (present or absent).
#'
#' information regarding the likelihood of positive and negative test results and
#' the likelihood of the presence or absence of a particular symptom in patients with
#' and without a particular disease.
#'
#' \strong{Reference:}
#' \enumerate{
#'   \item Biostatistics A Foundation for Analysis in the Health Sciences (10th Edition).
#'   Chapter 3.5 BAYES’ THEOREM, SCREENING TESTS, SENSITIVITY, SPECIFICITY
#' }
#'
#' @seealso \code{\link{strate}}
#' @keywords diagnostic tests, screening tests, sensitivity, specificity, ppv, npv
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' # Biostatistics A Foundation for Analysis in the Health Sciences (10th Edition)
#' # numbers taken from Example 3.5.1
#' t1 <- as.table(matrix(c(436, 5, 14, 495), byrow = TRUE, ncol = 2))
#' screening(t1, p = .113)

#' @export
screening <- function(x, y = NULL, p = NULL) {
  if (class(x) == "table") {
    t <- x
  } else {t <- table(x, y, exclude = "ifany")}
  a <- t[1,1]
  b <- t[1,2]
  c <- t[2,1]
  d <- t[2,2]
  t <- sum(t)

  ss <- a / (a + c)
  sp <- d / (b + d)

  if (is.null(p)) {
    p <- (a + c) / t
    q <- (b + d) / t
  } else {
    q <- 1 - p
    ppv <- (ss * p) / ( (ss * p) + (b / (b + d) * q) )
    npv <- (sp * q) / ( (sp * q) + (c / (a + c) * p) )
  }

  f <- c(sensitivity = ss, specificity = sp,
         ppv = ppv, npv = npv)
  cat("\nCharacteristics of SCREENING TEST",
      "\nNote: ", ifelse(is.null(p),
                         "Prevalence - not given, using sample's figure",
                         paste0("Prevalence = ", p)), "\n",
      "\nSensitivity, Pr(T+ | D+): ", ss,
      "\nSpecificity, Pr(T- | D-): ", sp,
      "\nPositive Predictive Value, Pr(D+ | T+): ", ppv,
      "\nNegative Predictive Value, Pr(D- | T-): ", npv, "\n\n")
  invisible(f)
}
