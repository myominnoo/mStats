#' @title Regression models, reporting overall significance of the model
#' @description
#' \code{testModelFit} overall significance of the model and interpretation
#' @param model object glm or lm type
#' @details
#' This test asks whether the model with predictors fits significantly better
#' than a model with just an intercept (i.e., a null model).
#'
#' The test statistic is the difference between the residual deviance for the
#' model with predictors and the null model. The test statistic is distributed
#' chi-squared with degrees of freedom equal to the differences in degrees of
#' freedom between the current and the null model (i.e., the number of predictor
#' variables in the model).
#'
#' \strong{Reference: }
#' \enumerate{
#'   \item LOGIT REGRESSION | R DATA ANALYSIS EXAMPLES. UCLA: Statistical Consulting
#'   Group. from https://stats.idre.ucla.edu/r/dae/logit-regression/ (accessed
#'   September 27, 2019)
#' }
#'
#' @seealso \code{\link{fModelOutput}}
#' @keywords model fit, significance of model
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#' ## example from IRDE website:
#' ## https://stats.idre.ucla.edu/r/dae/logit-regression/
#' mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
#' codebook(mydata)
#' tab(admit, mydata)
#' tab(rank, mydata)
#'
#' mylogit <- glm(admit ~ gre + gpa + factor(rank), data = mydata, family = "binomial")
#' summary(mylogit)
#'
#' fModelOutput(mylogit) # generates parameters
#' testModelFit(mylogit) # test overall significant of the model
#' }

#' @export
testModelFit <- function(model) {
  df.diff <- with(model, df.null - df.residual)
  test <- with(model, null.deviance - deviance)
  p.value <- with(model,
                  pchisq(null.deviance - deviance,
                         df.null - df.residual,
                         lower.tail = FALSE))
  if (p.value < 0.05) txt <- "fits" else txt <- "does not fit"
  cat("\n\t\tMeasure of model fit\n",
      "\nX-square = ", test,
      ", df = ", df.diff,
      ", p-value = ", p.value,
      "\n\nInterpretation: \n\tThe model as a whole ",
      txt, " significantly better than an empty model.", sep = "")
}
