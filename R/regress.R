#' @title Linear Regression Model
#'
#' @description
#' \code{regress()} produces summary of the model
#' with coefficients and 95% Confident Intervals.
#'
#' @param model glm or lm model
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
#'
#' @note
#'
#' Credits to Kevin Goulding, The Tarzan Blog.
#'
#' @return
#'
#' a data.frame named `regress`, a model named `model`
#' and another data.frame named `label`
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
#' ## fit linear model with lm or glm
#' fit <- lm(Ozone ~ Wind, data = airquality)
#'
#' ## linear model for Ozone
#' regress(fit)
#'
#' ## run again with robust standard errors and with plots to check
#' ## assumption
#' regress(fit, vce = TRUE)
#'
#'
#' ## Multiple linear model fit
#' fit.multi <- lm(Ozone ~ Wind + Solar.R + Temp + Month + Day,
#'                 data = airquality)
#' ## linear model with multiple predictors
#' reg <- regress(fit.multi, vce = TRUE)
#'
#' ## predict to generate statistics for model diagnostics.
#' ## Input the output from regress
#' predict(reg)
#'
#' ## Plot to get plots for regression diagnostics
#' ## Input the output from regress
#' # plot(reg)
#'
#' ## Ladder to generate transformation formula
#' ## It seems log transformation would be the most approapriate one
#' ladder(airquality, Ozone)
#'
#' @export
regress <- function(model, vce = FALSE, rnd = 5)
{
    ## match call arguments
    .args <- as.list(match.call())

    ## if input is not a lm or glm, stop
    if (!any(class(model) %in% c("glm", "lm"))) {
        stop(paste0("`", .args$model, "` must be linear model"),
             call. = FALSE)
    }

    .data <- getCall(model)$data
    formula <- getCall(model)$formula

    if (any(class(model) %in% c("glm"))) {
        ## reconstruct model with lm
        .model <- eval(parse(
            text = paste0("lm(", Reduce(paste, deparse(formula)),
                          ", data = ", .data, ")")
        ))
    } else {
        .model <- model
    }

    ## calculate errors and statistics
    .err <- calcRegress(.model, rnd)

    ## Calculate model coefficients and SE
    if (vce) {
        .coef <- vceRobust(.model)
        .txt <- paste0("            Linear Regression Output",
                       " with Robust Standard Error")
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


    ## get y name
    .y_name <- as.character(getCall(model)$formula)[2]
    .t <- rbind(c(.y_name, "Coef.", "Std.Err", "t",
                  "P>|t|", "[95% Conf.", "Interval]"),
                .t)

    ## combine two tables and fix dash lines
    names(.t) <- names(.err)
    .df <- rbind(.err, .t)

    ## add dashlines
    .df <- addDashLines(.df, .vline = 2)
    .df <- rbind(.df[1:8, ],
                 .df[c(1, 9, 1), ],
                 .df[c(10:nrow(.df)), ])
    .df[1, 4] <- ""
    .df[4, 6:8] <- .df[1, 6:8]
    .df[1, 5] <- .df[4, 5]
    names(.df)[4] <- ""
    row.names(.df) <- NULL


    ## get raw data for labelling
    .vars <- all.vars(formula(.model)[-2])
    .data <- .model$model


    ## constructs labels
    ## add label for by: cross-tabulation
    .lbl <- sapply(.vars, function(z) attr(.data[[z]], "label"))

    ## Print tabulation and labels
    printDF(.df, .txt)
    printText(paste0("Fit: ",
                     Reduce(paste, deparse(getCall(model)))))
    sapply(1:length(.vars), function(z) {
        printLabel(.data, .vars[z])
    })

    ## print label for by variable
    .lbl_by <- printLabel(.data, .y_name)

    ## construct label dataset
    .lbl_var <- data.frame(cbind(var = names(.lbl), lbl = .lbl))
    .lbl <- rbind(.lbl_var, c(.y_name, .lbl_by))

    ## create list with class tabulation
    .list <- list(reg = .df,
                  model = .model,
                  lbl = .lbl)

    ## add label for further processing
    attr(.df, "label") <- ifelse(
        vce, "Linear Regression with Robust Standard Errors",
        "Linear Regression"
    )

    ## create class for S3 method to use in summary()
    class(.list) <- "regress"

    invisible(.list)
}




# helpers -----------------------------------------------------------------



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
    .r <- rbind(c(colSums(.t[, 1:2]),
                  "Mean Sq" = mean(.t[["Mean Sq"]])), .r)

    ## DSS Princeton linear101 training
    ## add total row
    .df <- sum(.r$Df)
    .ss <- sum(.r$`Sum Sq`)
    .msq <- .ss / .df
    .r[3, ] <- c(.df, .ss, .msq)
    .r <- cbind(c("Model |", "Residual |", "Total |"), .r)
    names(.r) <- c("Source |", "DF", "SS", "MS")

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
    .r2_adj <- c(
        "Adj R-Squared",
        sprintf(.s$adj.r.squared, fmt = paste0("%#.", rnd, "f"))
    )


    ## calculate AIC
    .aic <- AIC(.model)
    .aic <- c("AIC", sprintf(.aic, fmt = paste0("%#.", 2, "f")))

    ## Process to combine into a table
    ## error table
    .r <- addDashLines(.r)[-1, ]
    .r <- rbind(.r[1:2, ], .r[4:3, ])
    .r[3, 1] <- paste0(.r[3, 1], " +")

    ## result table
    .df <- data.frame(rbind(.obs, .f, .pf, .r2))
    names(.df) <- c("Stats", "Value")
    .df <- cbind(.df, dum = "", .r)
    .df[5:7, ] <- ""
    .df[5:6, 1:2] <- rbind(.r2_adj, .aic)
    .df[6, 6:7] <- .mse
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




##' @rdname regress
##'
##' @description
##'
##' \code{predict} S3 method to predict linear model
##' after running \code{regress} function from \code{mStats}.
##'
##' @details
##'
##'
##' It generates an original data with statistics for model
##' diagnostics:
##'
##' 1. `fitted` (Fitted values)
##'
##' 2. `resid` (Residuals)
##'
##' 3. `std.resid` (Studentized Residuals)
##'
##' 4. `hat` (leverage)
##'
##' 5. `sigma`
##'
##' 6. `cooksd` (Cook's Distance)
##'
##' @inheritParams stats::predict
##'
##' @export
predict.regress <- function(object, ... )
{
    ## get the model from list object
    .model <- object$model
    ## get the original data
    .data <- eval(getCall(.model)$data)
    ## get vars name in the model
    .vars <- all.vars(formula(.model))

    ## calculate model diagnostic statistics
    .hat <- data.frame(lm.influence(.model))
    .dx <- cbind(.model$model,
                 fitted = fitted(.model),
                 resid = resid(.model),
                 std.resid = rstandard(.model),
                 hat = .hat$hat,
                 sigma = .hat$sigma,
                 cooksd = cooks.distance(.model))

    ## merge the two datasets by left-join
    .df <- merge.data.frame(.data, .dx,
                            by = .vars, all.x = TRUE)

    attr(.df$fitted, "label") <- "Fitted values"
    attr(.df$resid, "label") <- "Residuals"
    attr(.df$std.resid, "label") <- "Studentized Residuals"
    attr(.df$hat, "label") <- "Leverage or hat values"
    attr(.df$sigma, "label") <- "hat sigma"
    attr(.df$cooksd, "label") <- "Cook's Distance"

    return(.df)
}


##' @rdname regress
##'
##' @description
##'
##' \code{plot} S3 method to plot linear model
##' after running \code{regress} function from \code{mStats}.
##'
##' This is used to check model diagnostics.
##'
##' @inheritParams base::plot
##'
##' @export
plot.regress <- function(x, ... )
{
    .model <- x$model

    ## Set graph parameters
    par(mfrow = c(2, 2))

    plot(.model)

    ## Reset graph parameters
    par(mfrow = c(1, 1))
}


##' @rdname regress
##'
##' @description
##'
##' \code{ladder} converts a variable into a normally
##' distributed one.
##'
##' @param data dataset
##' @param var variable name
##'
##' @export
ladder <- function(data, var)
{
    ## match call arguments
    .args <- as.list(match.call())
    x <- as.character(.args$var)
    .x <- data[[.args$var]]

    ## get ladder vectors
    .t <- list(.x^3,
               .x^2,
               .x,
               sqrt(.x),
               log(.x),
               1 / sqrt(.x),
               1 / .x,
               1 / .x^2,
               1 / .x^3)

    ## get chi W and p-value
    .s <- do.call(
        rbind,
        lapply(.t, function(z) {
            .s <- shapiro.test(z)
            sprintf(c(.s$statistic, .s$p.value),
                    fmt = paste0("%#.", 5, "f"))
        })
    )

    ## combine all
    .df <- data.frame(c("cube", "squre", "raw", "square-root", "log",
                        "reciprocal root", "reciprocal", "reciprocal square",
                        "reciprocal cube"),
                      c(paste0(x, c("^3", "^2", "")),
                        paste0("sqrt(", x, ")"),
                        paste0("log(", x, ")"),
                        paste0("1 / sqrt(", x, ")"),
                        paste0("1 / ", x),
                        paste0("1 / (", x, "^2)"),
                        paste0("1 / (", x, "^3)")),
                      .s)
    names(.df) <- c("Transformation", "formula", "W", "P-Value")
    .df <- addDashLines(.df, .vline = 2)

    ## print df
    printDF(.df, "Ladder of Transformation")

    invisible(.df)
}


##' @rdname regress
##'
##' @description
##'
##' \code{hettest} performs the Breusch-Pagan test
##' for heteroskedasticity.
##'
##' It presents evidence against the
##' null hypothesis that t=0 in Var(e)=sigma^2 exp(zt).
##'
##' The formula are based on the \code{bptest} function
##' in \code{lmtest} package.
##'
##' @param studentize logical.
##' If set to \code{TRUE} Koenker's studentized version
##' of the test statistic will be used.
##'
##' @details
##'
##' The Breusch-Pagan test fits a linear regression model
##' to the residuals of a linear regression model
##' (by default the same explanatory variables are taken as
##' in the main regression model) and rejects if too
##' much of the variance is explained by the additional
##' explanatory variables. Under \eqn{H_0} the test statistic
##' of the Breusch-Pagan test follows a chi-squared distribution
##' with \code{parameter} (the number of regressors without
##' the constant in the model) degrees of freedom.
##'
##'
##' @references
##'
##' T.S. Breusch & A.R. Pagan (1979),
##'      A Simple Test for Heteroscedasticity and Random
##'      Coefficient Variation.
##'      \emph{Econometrica} \bold{47}, 1287--1294
##'
##' R. Koenker (1981), A Note on Studentizing a Test for
##'       Heteroscedasticity. \emph{Journal of Econometrics}
##'       \bold{17}, 107--112.
##'
##' W. Krämer & H. Sonnberger (1986),
##'       \emph{The Linear Regression Model under Test}.
##'       Heidelberg: Physica
##'
##'
##' @export
hettest <- function(model, studentize = FALSE)
{
    .model <- model$model

    ## get residual square
    .n <- nobs(.model)
    .r <- resid(.model)
    .s2 <- sum(.r^2) / .n

    ## get model matrix for lm.fit
    .Z <- model.matrix(.model)

    if (studentize) {
        .w <- .r^2 - .s2
        .aux <- lm.fit(.Z, .w)
        .bp <- .n * sum(.aux$fitted.values^2) / sum(.w^2)
        .txt <- "Studentized Breusch-Pagan test for heteroskedasticity"
        .txt <- paste0("    ", .txt)
    } else {
        .w <- .r^2 / .s2 - 1
        .aux <- lm.fit(.Z, .w)
        .bp <- 0.5 * sum(.aux$fitted.values^2)
        .txt <- "Breusch-Pagan test for heteroskedasticity"
        .txt <- paste0("          ", .txt)
    }

    .df <- .aux$rank - 1
    .p <- pchisq(.bp, .df, lower.tail = FALSE)

    ## gather all statistics
    .df <- cbind(
        "Constant variance",
        paste0("Fitted values of ", getCall(.model)$formula[2]),
        sprintf(.bp, fmt = paste0('%#.', 2, 'f')),
        sprintf(.p, fmt = paste0('%#.', 5, 'f'))
    )
    .df <- data.frame(.df)
    names(.df) <- c("Null Hypothesis", "Variables",
                    "Chi2(1)", "Prob > Chi2")
    .df <- addDashLines(.df, 2)

    ## Print
    printDF(.df, .txt)

    invisible(.df)
}



##' @rdname regress
##'
##' @description
##'
##' \code{linkTest} determines whether a model in R is
##' 'well specified' using the `STATA`'s `linkTest`.
##'
##' @details
##'
##' The code has been modified from Keith Chamberlain's linktext.
##' www.ChamberlainStatistics.com
##' https://gist.github.com/KeithChamberlain/8d9da515e73a27393effa3c9fe571c3f
##'
##'
##'
##'
##' @export
linkTest <- function(model, vce = FALSE, rnd = 5)
{
    ## get the model from list object
    .model <- model
    ## get the original data
    data <- getCall(.model)$data
    .data <- eval(data)
    ## get vars name in the model
    .vars <- all.vars(formula(.model))

    ## predict hat values
    .fit <- predict(.model)
    .fit2 <- .fit^2
    # Check to see that the predicted and predicted^2 variable actually
    # vary.
    if(round(var(.fit), digits=2) == 0){
        stop("No parameters that vary. Cannot perform test.")
    }

    .df <- cbind(.model$model,
                 hat_ = .fit,
                 hatsq_ = .fit2)

    # ## merge the two datasets by left-join
    # .df <- merge.data.frame(.data, .dx,
    #                         by = .vars, all.x = TRUE, all.y = FALSE)
    .df_name <- paste0(data, "_linkTest")
    assign(.df_name, .df, envir = environment(formula(model)))

    ## re fit model with hat_ and hatsq_
    .txt <- paste0("lm(", .vars[1], " ~ hat_ + hatsq_, data = ",
                   .df_name, ")")
    .refit <- eval(parse(text = .txt))
    .list <- regress(.refit, vce = vce, rnd = rnd)

    invisible(.list)
}

