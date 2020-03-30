#' @title Linear Regression Model
#'
#' @description
#' \code{regress()} produces regression outputs which mirror
#' outputs from `STATA`.
#'
#' @param data Dataset
#' @param y Dependent variable
#' @param ... Independent variable or multiple variables
#' @param robust if `TRUE`, robust standard errors are calculated. It is
#' used when heteroskedasticity is detected in data.
#' Otherwise, OLS standard errors are estimated.
#' @param plot logical: produces plots for model assumption
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#'
#' @details
#'
#' \code{regress} is based on \code{\link{lm}}. All statistics presented
#' in the function's output are derivates of \code{\link{lm}},
#' except AIC value which is obtained from \code{\link{AIC}}.
#'
#' \strong{Outputs}
#'
#' Outputs can be divided into three parts.
#'
#' 1) Information about the model
#'
#' Here provides number of observations (Obs.), F value, p-value from F test,
#' R Squared value, Adjusted R Squared value, square root of mean square error
#' (Root MSE) and AIC value.
#'
#' 2) Errors
#'
#' Outputs from `anova(model)` is tabulated here. SS, DF and MS indicate
#' sum of square of errors, degree of freedom and mean of square of errors.
#'
#' 3) Regression Output
#'
#' Coefficients from summary of model are tabulated here along with 95\%
#' confidence interval.
#'
#' \strong{using Robust Standard Errors}
#'
#' if heteroskedasticity is present in our data sample,
#' the ordinary least square (OLS) estimator will remain unbiased and consistent,
#' but not efficient. The estimated OLS standard errors
#' will be biased and cannot be solved with a larger sample size.
#' To remedy this, robust standard erros can be used to adjusted standard errors.
#'
#' \deqn{Variance of Robust = (N / N - K) (X'X)^(-1) \sum{Xi X'i ei^2} (X'X)^(-1)}
#'
#' where N =  number of observations, and K =  the number of regressors
#' (including the intercept). This returns a Variance-covariance (VCV) matrix
#' where the diagonal elements are the estimated heteroskedasticity-robust coefficient
#' variances — the ones of interest. Estimated coefficient standard errors
#' are the square root of these diagonal elements.
#'
#' Note: Credits to Kevin Goulding, The Tarzan Blog.
#'
#' @return
#'
#' A list of three `data.frame` and `model`
#'
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
#' ## use airquality dataset
#' data(airquality)
#' codebook(airquality)
#'
#' summ(airquality)
#'
#'
#' ## linear model for Ozone
#' regress(airquality, Ozone, Wind)
#'
#' ## run again with robust standard errors and with plots to check assumption
#' regress(airquality, Ozone, Wind, robust = TRUE, plot = TRUE)
#'
#' ## linear model with multiple predictors
#' regress(airquality, Ozone, Wind, Solar.R, Temp, Month, Day,
#'         robust = TRUE, plot = TRUE)
#'
#'
#' @export
regress <- function(data, y, ... , robust = FALSE, plot = FALSE, rnd = 3)
{

    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    ## assign data into .data for further evaluation
    .data <- data
    .y.name <- deparse(substitute(y))

    ## get variable names within three dots to search for duplicates
    .vars <- as.character(enquos(.args, c("data", "y", "robust", "plot", "rnd")))

    ## if there is no vars, then we will put ".".
    if (length(.vars) == 0) {
        .vars.txt <- "."
    } else {
        .vars.txt <- paste(.vars, collapse = " + ")
    }

    ## construct texts for model evaluation
    .formula <- paste0(.y.name, " ~ ", .vars.txt)
    .txt <- paste0("lm(", .formula, ", data = .data)")

    ## run model
    .model <- eval(parse(text = .txt))
    .summary <- summary(.model)


    ## get errors, SS, DF and MS
    .df.errors <- getSquareErrors(.model, rnd)


    ## get the coefficients and others for final table
    if (robust) {
        .df <- data.frame(coefRobust(.model))
    } else {
        .df <- data.frame(coef(.summary))
    }
    names(.df) <- c("e", "se", "t", "p")

    ## create data frame
    .df <- data.frame(row.names(.df), "|",
                      sprintf(.df$e, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.df$se, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.df$t, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.df$e - (1.96 * .df$se), fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.df$e + (1.96 * .df$se), fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.df$p, fmt = paste0('%#.', rnd + 2, 'f')),
                      stringsAsFactors = FALSE)
    names(.df) <- c(.y.name, "|", "Coef.", "Std. Err.", "t",
                    "[95% Conf.", "Interval]", "P>|t|")

    .df <- addDashLines(.df, .vLine = 2)


    ## additional info about models
    .obs <- length(resid(.model))
    .f <- .summary$fstatistic
    .pf <- pf(.f[1], .f[2], .f[3], lower.tail = FALSE)
    .mse <- as.numeric(.df.errors[.df.errors[["Source"]] == "Residual", "MS"])
    .df.info <- data.frame(
        Info = c("Obs.", paste0("F(", round(.f[2]), ", ", .f[3], ")"),
                 "Prob > F", "R-Squared", "Adj. R-Squared", "Root MSE",
                 "AIC"),
        X = "|",
        Statistics = c(.obs,
                   sprintf(.f[1], fmt = paste0('%#.', rnd, 'f')),
                   sprintf(.pf, fmt = paste0('%#.', rnd + 2, 'f')),
                   sprintf(c(.summary$r.squared,
                             .summary$adj.r.squared, sqrt(.mse), AIC(.model)),
                           fmt = paste0('%#.', rnd, 'f'))),
        stringsAsFactors = FALSE)
    names(.df.info)[2] <- "|"
    .df.info <- addDashLines(.df.info, .vLine = 2)


    ## print outputs
    printText2(.df.info, "Linear Regression Model", .printDF = TRUE)
    printMsg(paste0("Formula: ", .txt))
    print.data.frame(.df.errors, row.names = FALSE)

    .txt <- "Regression Output"
    if (robust) {
        .txt <- paste0(.txt, " [Robust Standard Errors]")
    }
    printText2(.df, .txt, .printDF = TRUE)

    ## Construct labels
    ## add label for by: cross-tabulation
    .lbl <- sapply(.vars, function(z) attr(.data[[z]], "label"))

    ## Print tabulation
    if (length(.lbl) > 0) {
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


    ## plotting
    if (plot) {
        layout(matrix(1:4, 2, 2))
        tryCatch(plot(.model), error = function(cnd) {
            print(cnd)
        })
    }

    ## return list
    .df <- list(.df.info, .df.errors, .df, .model)
    attr(.df, "model") <- "linear"
    invisible(.df)
}


# Helpers -----------------------------------------------------------------

getSquareErrors <- function(.model, rnd = 3)
{
    .anova <- data.frame(anova(.model))

    ## extract Sum Sq, DF, Mean Sq
    .df <- .anova[, c("Sum.Sq", "Df", "Mean.Sq")]
    .df$Sum.Sq <- round(.df$Sum.Sq, rnd)
    .df$Mean.Sq <- round(.df$Mean.Sq, rnd)


    .ind <- .df[row.names(.df) != "Residuals", ]
    .total <- c(Source = "Model", X = "|",
                round(c(sum(.ind$Sum.Sq, na.rm = TRUE),
                        sum(.ind$Df, na.rm = TRUE),
                        mean(.ind$Mean.Sq, na.rm = TRUE)), rnd))

    # ## process to combine
    .ind <- addDashLines(
        data.frame(Source = row.names(.ind), X = "|", .ind,
                   stringsAsFactors = FALSE), .vLine = 2)[-1, ]
    .df.model <- rbind(.ind, .total)

    ## add residual row
    .residual <- cbind(Source = "Residual", X = "|",
                       .df[row.names(.df) == "Residuals", ])

    ## final table
    .df <- addDashLines(rbind(.df.model, .residual), .vLine = 2)
    names(.df) <- c("Source", "|", "SS", "DF", "MS")
    row.names(.df) <- NULL

    .df
}

## robust standard errors for heteroskedasticity
## Heteroskedasticity-robust standard error calculation.
## https://thetarzan.wordpress.com/2011/05/28/heteroskedasticity-robust-and-clustered-standard-errors-in-r/
coefRobust <- function(.model) {
    .summary <- summary(.model)
    .matrix <- model.matrix(.model)
    .square <- residuals(.model)^2
    .xdx <- 0


    ## Here one needs to calculate X'DX. But due to the fact that
    ## D is huge (NxN), it is better to do it with a cycle.
    sapply(1:nrow(.matrix), function(z) {
        .xdx <<- .xdx + .square[z] * .matrix[z, ] %*% t(.matrix[z, ])
    })


    # inverse(X'X)
    .inverse <- solve(t(.matrix) %*% .matrix)

    # Variance calculation (Bread x meat x Bread)
    .var.covar <- .inverse %*% .xdx %*% .inverse

    # degrees of freedom adjustment
    .df.adj <- sqrt(nrow(.matrix))/sqrt(nrow(.matrix)-ncol(.matrix))

    # Standard errors of the coefficient estimates are the
    # square roots of the diagonal elements
    .se <- .df.adj * sqrt(diag(.var.covar))

    .t <- .model$coefficients / .se
    .p <- 2 * pnorm(-abs(.t))


    .df <- cbind(.model$coefficients, .se, .t, .p)
    dimnames(.df) <- dimnames(.summary$coefficients)

    .df
}
