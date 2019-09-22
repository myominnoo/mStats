#' @title Reporting coefficients of regression models
#' @description
#' \code{model.output} display corresponding estimates of different models along
#' with 95% confidence intervals and p-values.
#' @param model object glm or lm type
#' @param rnd rounding digits
#' @param raw logical value
#' @details
#' If raw is set to TRUE, a table containing raw estimate, standard errors, z value and
#' p-value is generated.
#'
#' @seealso \code{\link{model.fit}}
#' @keywords model output, coefficients display
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
#' model.output(mylogit)
#' model.output(mylogit, raw = TRUE)
#' model.fit(mylogit)
#' }

#' @export
model.output <- function(model, rnd = 1, raw = FALSE)
{
  if (any(grepl("binomial", model$call))) {
    if (raw) {op <- coef(summary(model))} else {
      op <- cbind(exp(cbind(coef(model), confint(model))),
                  p.value = coef(summary(model))[,4])[-1,]
    }
  } else {
    message("Wrong type of model: cannot generate output ...")
    op <- NULL
  }
  # sprintf(t[,1:3], fmt = paste0('%#.', rnd, 'f', collapse = ""))
  return(op)
}
