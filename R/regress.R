#' @title Linear Regression Model
#'
#' @description
#' \code{regress()} produces summary of the model
#' with coefficients and 95% Confident Intervals.
#'
#' @param data Dataset
#' @param y Dependent variable
#' @param ... Independent variable or multiple variables
#' @param vce if `TRUE`, robust standard errors are calculated. It is
#' used when heteroskedasticity is detected in data.
#' Otherwise, OLS standard errors are estimated.
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
#' Here provides number of observations (Obs.), F value, p-value
#' from F test,
#' R Squared value, Adjusted R Squared value, square root of mean square
#' error
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
#' the ordinary least square (OLS) estimator will remain unbiased
#' and consistent,
#' but not efficient. The estimated OLS standard errors
#' will be biased and cannot be solved with a larger sample size.
#' To remedy this, robust standard erros can be used to adjusted
#' standard errors.
#'
#' \deqn{Variance of Robust = (N / N - K) (X'X)^(-1)
#'  \sum{Xi X'i ei^2} (X'X)^(-1)}
#'
#' where N =  number of observations, and K =  the number of regressors
#' (including the intercept). This returns a Variance-covariance (VCV)
#' matrix
#' where the diagonal elements are the estimated heteroskedasticity-robust
#' coefficient
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
#'
#'
#' ## linear model for Ozone
#' regress(airquality, Ozone, Wind)
#'
#' ## run again with robust standard errors and with plots to check
#' ## assumption
#' regress(airquality, Ozone, Wind, vce = TRUE)
#'
#' ## linear model with multiple predictors
#' regress(airquality, Ozone, Wind, Solar.R, Temp, Month, Day,
#'         vce = TRUE)
#'
#'
#' @export
regress <- function(data, y, ... , vce = FALSE, rnd = 4)
{
    ## match call arguments
    .args <- as.list(match.call())

    ## copy data to .data
    .data <- data

    ## get names of dataset and headings
    .data_name <- deparse(substitute(data))
    .vars_names <- names(.data)
    .y_name <- deparse(substitute(y))

    ## if input is not a data.frame, stop
    if (!is.data.frame(.data)) {
        stop(paste0("`", .data_name, "` must be a data.frame"),
             call. = FALSE)
    }

    ## get variable names within three dots to search for duplicates
    .vars <- enquotes(.args, c("data", "y", "vce", "rnd"))


    ## construct texts for model evaluation
    .formula <- paste0(.y_name, " ~ ", paste(.vars, collapse = " + "))
    .ftxt <- paste0("lm(", .formula, ", data = .data)")

    ## run model
    .model <- eval(parse(text = .ftxt))
    ## calculate errors and statistics
    .err <- calcRegress(.model, rnd)

    if (vce) {
        .coef <- vceRobust(.model)
        .txt <- paste0("                Linear Regression Output",
                       " with Robust SE")
    } else {
        .coef <- data.frame(coef(summary(.model)))
        .txt <- paste0("                Linear Regression Output")
    }
    ## put intercept to last and names
    .coef <- rbind(.coef[-1, ], .coef[1, ])
    names(.coef) <- c("e", "se", "t", "p")

    ## gather all statistics
    .t <- data.frame(
        cbind(
            row.names(.coef),
            sprintf(.coef$e, fmt = paste0('%#.', rnd, 'f')),
            sprintf(.coef$se, fmt = paste0('%#.', rnd, 'f')),
            sprintf(.coef$t, fmt = paste0('%#.', 2, 'f')),
            sprintf(.coef$p, fmt = paste0('%#.', rnd, 'f')),
            sprintf(.coef$e - (1.96 * .coef$se),
                    fmt = paste0('%#.', rnd, 'f')),
            sprintf(.coef$e + (1.96 * .coef$se),
                    fmt = paste0('%#.', rnd, 'f'))
        )
    )
    .t <- rbind(c(.y_name, "Coef.", "Std.Err", "t",
                  "P>|t|", "[95% Conf.", "Interval]"),
                .t)

    ## combine two tables and fix dash lines
    .df <- fixLinesRegress(.t, .err)

    ## add label for further processing
    attr(.df, "model") <- "lm"

    ## constructs labels
    ## add label for by: cross-tabulation
    .lbl <- sapply(.vars, function(z) attr(.data[[z]], "label"))

    ## Print tabulation and labels
    printDF(.df, .txt)
    printText(paste0("Formula: ",
                     gsub("\\.data", .data_name, .ftxt)))
    sapply(1:length(.vars), function(z) {
        printLabel(.data, .vars[z])
    })

    ## print label for by variable
    printLabel(.data, .args$y)

    invisible(list(result = .df,
                   model = .model))
}

# Helpers -----------------------------------------------------------------

calcRegress <- function(.model, rnd)
{
    ## model summary and F test
    .s <- summary(.model)
    .aov <- anova(.model)

    ## calculate errors
    ## errors without residuals
    .t <- .aov[-nrow(.aov), ]

    ## errors with residuals
    .r <- .aov["Residuals", 1:3]
    .r <- cbind(
        Source = c("Model", "Residuals"),
        rbind(c(colSums(.t[, 1:2]),
                "Mean Sq" = mean(.t[["Mean Sq"]])), .r)
    )
    names(.r) <- c("Source", "DF", "SS", "MS")

    ## calculate MSE
    .mse <- sqrt(.r["Residuals", "MS"])
    .mse <- c("Root MSE",
              sprintf(.mse, fmt = paste0("%#.", 2, "f")))

    ## fix decimal places
    .r[, "SS"] <- sprintf(.r[, "SS"], fmt = paste0("%#.", 1, "f"))
    .r[, "MS"] <- sprintf(.r[, "MS"], fmt = paste0("%#.", 1, "f"))
    row.names(.r) <- NULL

    ## number of observations, f statistics, pvalue
    .obs <- c("Number of Obs", length(.s$residuals))
    ## calculate F statistics
    .f <- .s$fstatistic
    .fv <- .f["value"]
    .fndf <- .f["numdf"]
    .fddf <- .f["dendf"]
    if (is.null(.f)) {
        .fv <- .fndf <- 0
        .fddf <- length(.s$residuals) - 1
        .pf <- 0
    } else {
        .pf <- pf(.fv, .fndf, .fddf, lower.tail = FALSE)
    }

    .f <- c(paste0("F(", .fndf, ", ", .fddf, ")"),
            sprintf(.fv, fmt = paste0("%#.", 2, "f")))
    .pf <- c("Prob > F",
             sprintf(.pf, fmt = paste0("%#.", rnd, "f")))

    ## calculate r squared, adj r squared
    .r2 <- c("R-Squared",
             sprintf(.s$r.squared, fmt = paste0("%#.", rnd, "f")))
    .r2_adj <- c("Adj R-Squared",
                 sprintf(.s$adj.r.squared, fmt = paste0("%#.", rnd, "f")))


    ## calculate AIC
    .aic <- AIC(.model)
    .aic <- c("AIC", sprintf(.aic, fmt = paste0("%#.", 2, "f")))

    ## gather all statistics
    .df <- data.frame(rbind(.obs, .pf))
    names(.df) <- c("Stats", "Value")
    .df <- cbind(.df, dum = "", .r)
    .df <- rbind(.df, c(.r2, "", .mse, .f, .fv),
                 c(.r2_adj, "", .aic, "", ""))
    row.names(.df) <- NULL

    return(.df)
}

## robust standard errors for heteroskedasticity
## Heteroskedasticity-robust standard error calculation.
## https://thetarzan.wordpress.com/2011/05/28/heteroskedasticity-robust-and-clustered-standard-errors-in-r/
vceRobust <- function(.model) {

    .s <- summary(.model)
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


    ## gather all statistics
    .df <- cbind(.model$coefficients, .se, .t, .p)
    .df <- data.frame(.df)
    names(.df) <- colnames(.s$coefficients)

    return(.df)
}
fixLinesRegress <- function(.t, .err)
{
    ## add dash lines for outputs
    .err <- addDashLines(.err)
    .err <- rbind(.err[2:3, ], .err[c(1, 4:5), ])
    .err[3, 3] <- ""

    ## name df colmns
    names(.t) <- names(.err)
    .df <- rbind(.err, .t)
    .df <- addDashLines(.df, .vline = 2)

    ## re-arrange rows
    .df <- rbind(.df[1:6, ],
                 .df[1, ], .df[7, ], .df[1, ],
                 .df[8:nrow(.df), ])
    .df[1, 4] <- ""
    names(.df)[4] <- ""

    ## remove row names
    row.names(.df) <- NULL

    return(.df)
}
