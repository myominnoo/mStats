#' @title Logistic Regression Models
#'
#' @description
#' \code{logit()} produces regression outputs which mirror
#' outputs from `STATA`.
#'
#' @param data Dataset
#' @param y Dependent variable
#' @param ... Independent variable or multiple variables
#' @param or if `TRUE`, odds ratio is reported. If `FALSE`,
#' coefficients are estimated.
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#'
#' @details
#'
#' \code{logit} is based on \code{\link{glm}} with binomial family.
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
#' logit(infert, case, induced, spontaneous)
#'
#' ## get coefficient instead of odds ratio
#' logit(infert, case, induced, spontaneous, or = FALSE)
#'
#' @export
logit <- function(data, y, ... , or = TRUE, rnd = 4)
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
    .vars <- enquotes(.args, c("data", "y", "or", "rnd"))


    ## construct texts for model evaluation
    .formula <- paste0(.y_name, " ~ ", paste(.vars, collapse = " + "))
    .ftxt <- paste0("glm(", .formula, ", family = binomial, data = .data)")

    ## run model
    .model <- eval(parse(text = .ftxt))
    ## calculate errors and statistics
    .err <- calcLogit(.model, rnd)


    ## get coefficients
    .s <- summary(.model)
    .coef <- data.frame(coef(.s)[, 1:3],
                        confint.default(.model),
                        coef(.s)[, 4])
    ## put intercept to last and names
    .coef <- rbind(.coef[-1, ], .coef[1, ])
    names(.coef) <- c("e", "se", "z", "ll", "ul", "p")

    ## calculate OR
    if (or) {
        .coef$se <- sqrt(exp(coef(.model))^2 * diag(vcov(.model)))
        .coef$e <- exp(.coef$e)
        .coef$ll <- exp(.coef$ll)
        .coef$ul <- exp(.coef$ul)
    }

    ## gather all statistics
    .t <- data.frame(
        cbind(
            row.names(.coef),
            sprintf(.coef$e, fmt = paste0('%#.', rnd, 'f')),
            sprintf(.coef$se, fmt = paste0('%#.', rnd, 'f')),
            sprintf(.coef$z, fmt = paste0('%#.', 2, 'f')),
            sprintf(.coef$p, fmt = paste0('%#.', rnd, 'f')),
            sprintf(.coef$ll, fmt = paste0('%#.', rnd, 'f')),
            sprintf(.coef$ul,
                    fmt = paste0('%#.', rnd, 'f'))
        )
    )
    .t <- rbind(c(.y_name,
                  ifelse(or, "Odds Ratio", "Coef."),
                  "Std.Err", "z", "P>|z|", "[95% Conf.", "Interval]"),
                .t)

    ## combine two tables and fix dash lines
    .df <- fixLinesLogit(.t, .err)

    ## add label for further processing
    attr(.df, "model") <- "logit"

    ## constructs labels
    ## add label for by: cross-tabulation
    .lbl <- sapply(.vars, function(z) attr(.data[[z]], "label"))

    ## Print tabulation and labels
    printDF(.df, "                   Logistic Regression Output")
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


# helpers -----------------------------------------------------------------


calcLogit <- function(.model, rnd)
{
    ## model summary and F test
    .s <- summary(.model)
    ## number of observations, f statistics, pvalue
    .obs <- c("Number of Obs", length(resid(.model)))

    ## LR Chi Square Test
    .chi <- with(.model, null.deviance - deviance)
    .chi.df <- with(.model, df.null - df.residual)
    .chi.p <- pchisq(.chi, .chi.df, lower.tail = FALSE)
    .chi <- c(paste0("LR chi2(", .chi.df, ")"),
              sprintf(.chi, fmt = paste0("%#.", 2, "f")))

    .chi.p <- c("Prob > chi2",
                sprintf(.chi.p, fmt = paste0("%#.", rnd, "f")))

    ## calculate pseudo R-squared
    ## https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-what-are-pseudo-r-squareds/
    ## https://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/
    ## https://stats.stackexchange.com/questions/8511/how-to-calculate-pseudo-r2-from-rs-logistic-regression
    # McFadden's R squared
    .pr2 <- 1 - .model$deviance / .model$null.deviance
    .pr2 <- c("Pseudo R-Squared",
              sprintf(.pr2, fmt = paste0("%#.", rnd, "f")))

    .ll <- c("Log likelihood",
             sprintf(logLik(.model), fmt = paste0("%#.", 2, "f")))

    ## get AIC value
    .aic <- c("AIC",
              sprintf(.s$aic, fmt = paste0("%#.", 2, "f")))

    .df <- data.frame(rbind(.obs, .chi, .chi.p, .aic, .pr2, .ll),
                      "", "", "", "", "")
    names(.df)[1:2] <- c("Stats", "value")
    row.names(.df) <- NULL

    return(.df)
}
fixLinesLogit <- function(.t, .err)
{
    ## add dash lines for outputs
    .err <- addDashLines(.err)
    .err <- rbind(.err[2:4, ], .err[c(1, 5:7), ])

    ## name df colmns
    names(.t) <- names(.err)
    .df <- rbind(.err, .t)
    .df <- addDashLines(.df, .vline = 2)
    row.names(.df) <- NULL

    ## re-arrange rows
    .df <- rbind(.df[1:8, ],
                 .df[1, ], .df[9, ], .df[1, ],
                 .df[10:nrow(.df), ])
    .df[c(1, 5), 4:8] <- names(.df)[4:8] <- ""

    row.names(.df) <- NULL

    return(.df)
}
