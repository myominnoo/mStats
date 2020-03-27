#' @title Logistic Regression Models
#'
#' @description
#' \code{logistic()} produces regression outputs which mirror
#' outputs from `STATA`.
#'
#' @param data Dataset
#' @param y Dependent variable
#' @param ... Independent variable or multiple variables
#' @param odds_ratio if `TRUE`, odds ratios, exponentiated coefficients
#' are calculated.
#' Otherwise, coefficients are estimated.
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#'
#' @details
#'
#' \code{logistic} is based on \code{\link{glm}} with binomial family.
#' All statistics presented in the function's output are derivates of
#' \code{\link{glm}},
#' except AIC value which is obtained from \code{\link{AIC}}.
#'
#' \strong{Outputs}
#'
#' Outputs can be divided into three parts.
#'
#' 1) Information about the model
#'
#' Here provides number of observations (Obs.), chi value from Likelihood Ratio
#' test (LR chi2) and its degree of freedom, p-value from LR test,
#' Pseudo R Squared, log likelihood and AIC values.
#'
#' 2) Regression Output
#'
#' Coefficients from summary of model are tabulated here along with 95\%
#' confidence interval.
#'
#'
#' @return
#'
#' A list of two `data.frame` and `model`
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
#'
#' ## use infert data
#' data(infert)
#'
#' ## run logistic regression
#' logistic(infert, case, induced, spontaneous)
#'
#' ## get coefficient instead of odds ratio
#' logistic(infert, case, induced, spontaneous, odds_ratio = FALSE)
#'
#' @export
## to add ordinal and multinomial logistic regression
logistic <- function(data, y, ... , odds_ratio = TRUE, rnd = 3)
{

    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    ## assign data into .data for further evaluation
    .data <- data
    .y.name <- deparse(substitute(y))

    ## get variable names within three dots to search for duplicates
    .vars <- as.character(enquos(.args, c("data", "y", "odds_ratio", "plot", "rnd")))

    ## if there is no vars, then we will put ".".
    if (length(.vars) == 0) {
        .vars.txt <- "."
    } else {
        .vars.txt <- paste(.vars, collapse = " + ")
    }

    ## construct texts for model evaluation
    .formula <- paste0(.y.name, " ~ ", .vars.txt)
    .txt <- paste0("glm(", .formula, ", family = binomial, data = .data)")

    ## run logistic model
    .model <- eval(parse(text = .txt))
    .summary <- summary(.model)

    ## get coefficients
    .coef <- data.frame(coef(.summary)[, 1:3],
                        confint.default(.model),
                        coef(.summary)[, 4])
    names(.coef) <- c("e", "se", "z", "ll", "ul", "p")

    if (odds_ratio) {
        .coef$se <- sqrt(exp(coef(.model))^2 * diag(vcov(.model)))
        .coef$e <- exp(.coef$e)
        .coef$ll <- exp(.coef$ll)
        .coef$ul <- exp(.coef$ul)
    }


    ## create data frame
    .df <- data.frame(row.names(.coef), "|",
                      sprintf(.coef$e, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.coef$se, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.coef$z, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.coef$ll, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.coef$ul, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.coef$p, fmt = paste0('%#.', rnd + 2, 'f')),
                      stringsAsFactors = FALSE)
    names(.df) <- c(.y.name, "|",
                    ifelse(odds_ratio, "Odd Ratio", "Coef."),
                    "Std. Err.", "z",
                    "[95% Conf.", "Interval]", "P>|z|")

    .df <- addDashLines(.df, .vLine = 2)


    ## additional info about models
    .obs <- length(resid(.model))

    ## LR Chi Square Test
    .chi <- with(.model, null.deviance - deviance)
    .chi.df <- with(.model, df.null - df.residual)
    .lr <- pchisq(.chi, .chi.df, lower.tail = FALSE)

    ## https://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/
    # McFadden's R squared
    .model.null <- eval(parse(text = paste0("glm(", .y.name,
                                            " ~ 1, family = binomial, data = .data)")))
    .logllh <- logLik(.model)
    .pseudo.r <- 1 - (.logllh / logLik(.model.null))

    ## create table
    .df.info <- data.frame(
        Info = c("Obs.", paste0("LR chi2 (", .chi.df, ")"),
                 "Prob > chi2", "Pseudo R-Squared", "Log Likelihood", "AIC"),
        X = "|",
        Statistics = c(.obs,
                       sprintf(.chi, fmt = paste0('%#.', rnd, 'f')),
                       sprintf(.lr, fmt = paste0('%#.', rnd + 2, 'f')),
                       sprintf(c(.pseudo.r, .logllh, AIC(.model)),
                               fmt = paste0('%#.', rnd, 'f'))),
        stringsAsFactors = FALSE)

    names(.df.info)[2] <- "|"
    .df.info <- addDashLines(.df.info, .vLine = 2)



    ## print outputs
    printText2(.df.info, "Logistic Regression Model", .printDF = TRUE)
    printMsg(paste0("Formula: ", .txt))
    printText2(.df, "Regression Output", .printDF = TRUE)

    ## Construct labels
    ## add label for by: cross-tabulation
    .lbl <- sapply(.vars, function(z) attr(.data[[z]], "label"))

    ## Print tabulation
    if (length(.lbl) > 0) {
        ## Print tabulation
        if (any(.lbl != "NULL")) {
            printMsg("Labels")
        }
        sapply(1:length(.vars), function(z) {
            if (.lbl[z] != "NULL") {
                printMsg(paste0(.vars[z], ": ", .lbl[z]))
            }
        })
    }

    getnPrintLabel(.data, .y.name)


    ## return list
    .df <- list(.df.info, .df, .model)
    attr(.df, "model") <- "logit"


    invisible(.df)
}
