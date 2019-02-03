#' @title Process Logistic Regression model and Display Odds Ratios
#'
#' @description
#' Takes several covariates and generates Odds Ratios outputs at the same time. Outputs include unadjusted / adjusted OR, 95% CI and P-values.
#'
#' @param covar a vector containing all the covariates to be fitted
#' @param outcome a symbolic description of the dependent variable to be fitted.
#' @param data a data frame containing the variables in the model.
#' @param aOR a logical value indicating either unadjusted or adjusted model
#' @param write.csv a logical value to save the cox table output to CSV file.
#' @param file either a character string naming a file.
#' @param print.to.console print the table to the console even if the output is assigned to an object.
#' @seealso cox.output, itab, isum, inumsum

#' @export
logistic.output <- function(covar, outcome, data, aOR = FALSE, write.csv = FALSE,
                            file = NULL, print.to.console = FALSE)

{
  arguments <- as.list(match.call())
  outcome <- eval(arguments$outcome, data)
  if (aOR) {
    f <- as.formula(paste0(arguments$outcome, " ~ ", paste(covar, collapse = " + ")))
    m <- tryCatch(glm(f, data = data, family = "binomial"), error=function(e) NULL)
    c <- cbind(exp(cbind(OR = coef(m),
                        suppressMessages(confint(m)))),
                        p.value = coef(summary(m))[,4])
    c <- c[row.names(c) != "(Intercept)", ]
  } else {
    f <- sapply(covar, function(x)
      as.formula(paste0(arguments$outcome, " ~ ", x)))
    # glm binomial function
    glm.log <- function (y) {
      return(tryCatch(glm(y, data = data, family = "binomial"), error=function(e) NULL))
    }
    m <- lapply(f, glm.log)
    c <- NULL
    for(i in 1:length(m)) {
      c <- rbind(c, cbind(exp(cbind(OR = coef(m[[i]]),
                                    suppressMessages(confint(m[[i]])))),
                          p.value = coef(summary(m[[i]]))[,4]))
    }
    c <- c[row.names(c) != "(Intercept)", ]
  }
  colnames(c)[1] <- ifelse(aOR, "aOR", "uOR")

  # write to CSV file
  if (write.csv) {
    file.name <- ifelse(is.null(file),
                        ifelse(aOR, "aOR_logistic.csv", "uOR_logistic.csv"), file)
    write.table(c, file.name, row.names = TRUE, col.names = TRUE, sep = ",")
  }
  # print to console
  if (print.to.console) print(c)
  return(c)
}
