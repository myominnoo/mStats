#' @title Process Cox survival model and Display Hazard Ratios
#'
#' @description
#' Takes several covar and generates Hazard Ratios outputs at the same time. Outputs include unadjusted / adjusted HR, 95% CI and P-values.
#'
#' @param covar a vector containing all the covariates to be fitted
#' @param time time variable for right censored data, this is the follow up time. For interval data, the first argument is the starting time for the interval.
#' @param event The status indicator, normally 0=alive, 1=dead. Other choices are TRUE/FALSE (TRUE = death) or 1/2 (2=death). For interval censored data, the status indicator is 0=right censored, 1=event at time, 2=left censored, 3=interval censored. For multiple enpoint data the event variable will be a factor, whose first level is treated as censoring. Although unusual, the event indicator can be omitted, in which case all subjects are assumed to have an event.
#' @param data a data frame containing the variables in the model.
#' @param aHR a logical value indicating either unadjusted or adjusted model
#' @param write.csv a logical value to save the cox table output to CSV file.
#' @param file either a character string naming a file.
#' @param print.to.console print the table to the console even if the output is assigned to an object.
#' @seealso itab, isum, inumsum

#' @export
cox.output <- function(covar, time, event, data, aHR = FALSE, write.csv = FALSE,
                       file = NULL, print.to.console = FALSE)

{
  arguments <- as.list(match.call())
  time <- eval(arguments$time, data)
  event <- eval(arguments$event, data)
  # process batches of formulas
  f.txt <- paste0('survival::Surv(', arguments$time, ', ', arguments$event, ') ~ ')
  if (aHR) {
    f <- as.formula(paste0(f.txt, paste(covar, collapse = " + ")))
    # run cox models
    surv <- tryCatch(survival::coxph(f, data = data), error=function(e) NULL)
    sm <- summary(surv)
    c <- cbind("HR" = sm$coefficients[, 2],
                        exp(confint(surv)),
                        "p.value" = sm$coefficients[, 5])
  } else {
    f <- sapply(covar,
                function(y) as.formula(paste(f.txt, y)))
    # run cox models
    surv <- function (y) {
      return(tryCatch(survival::coxph(y, data = data), error=function(e) NULL))
    }
    f <-  lapply(f, surv)
    c <- NULL
    for(i in 1:length(f)) {
      sm <- summary(f[[i]])
      c <- rbind(c, cbind("HR" = sm$coefficients[, 2],
                          exp(confint(f[[i]])),
                          "p.value" = sm$coefficients[, 5]))
    }
  }
  colnames(c)[1] <- ifelse(aHR, "aHR", "uHR")

  # write to CSV file
  if (write.csv) {
    file.name <- ifelse(is.null(file),
                        ifelse(aHR, "aHR_COX.csv", "uHR_COX.csv"), file)
    write.table(c, file.name, row.names = TRUE, col.names = TRUE, sep = ",")
  }
  # print to console
  if (print.to.console) print(c)
  return(c)
}


