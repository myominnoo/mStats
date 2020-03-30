#' @title Tabulate estimates of regression models for comparison
#'
#' @description
#'
#' \code{esttab()} produces publication quality tables which can be exported
#' to `.csv` format.
#'
#' @param ... regression models of `mStats`'s functions
#'
#' @details
#'
#' It is an extention to functions like \code{\link{regress}} and
#' \code{\link{logistic}}. It processes their outputs and generates
#' a publication quality tabulation for model comparisons.
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
#'
#' @examples
#'
#' ## use infert data
#' data(infert)
#' codebook(infert)
#'
#' ## running different logistic regression models
#' m1 <- logistic(infert, case, education, odds_ratio = FALSE)
#' m2 <- logistic(infert, case, education, age, odds_ratio = FALSE)
#' m3 <- logistic(infert, case, education, age, parity, odds_ratio = FALSE)
#' m4 <- logistic(infert, case, education, age, parity, induced, odds_ratio = FALSE)
#' m5 <- logistic(infert, case, education, age, parity, induced, spontaneous,
#'                odds_ratio = FALSE)
#' e <- esttab(m1, m2, m3, m4, m5)
#'
#' @export
esttab <- function( ... )
{
    .models <- list(...)

    .type <- NULL
    ## first get names, coefficients and pvalue
    .coef <- lapply(1:length(.models), function(z) {

        .m <- .models[[z]]

        ## get the model type to separate linear and logit models.
        .type <<- attr(.m, "model")
        .m.len <- ifelse(.type == "linear", 3, 2)

        .coef <- data.frame(.m[[.m.len]])
        # .coef <- .coef[-c(1, nrow(.coef)), c(1,3,ncol(.coef))]
        # names(.coef)[2:3] <- paste0(c("e", "p"), z)
        #
        # .coef
        .coef <- .coef[-c(1, nrow(.coef)), c(1, 3, 4, ncol(.coef))]
        names(.coef) <- c(names(.coef)[1], "e", "s", "p")

        .coef$p <- as.numeric(.coef$p)

        .p1 <- .coef$p < 0.05
        .p2 <- .coef$p < 0.01
        .p3 <- .coef$p < 0.001

        .coef$e[.p1] <- paste0(.coef$e[.p1], "*")
        .coef$e[.p2] <- paste0(.coef$e[.p2], "*")
        .coef$e[.p3] <- paste0(.coef$e[.p3], "*")

        .coef$s <- paste0("(", .coef$s, ")")
        .coef <- .coef[, 1:3]
        names(.coef)[-1] <- paste0(names(.coef)[-1], z)

        .coef
    })


    ## merging all coefficients
    .df <- NULL
    for (i in 1:length(.coef)) {
        if (i == 1) {
            .df <- .coef[[i]]
        } else {
            .df2 <- .coef[[i]]
            .df <- merge.data.frame(.df, .df2, by = names(.df)[1], all = TRUE)
        }
    }

    ## keep intercept in the last row
    .df <- rbind(.df[.df[[1]] != "(Intercept)", ],
                 .df[.df[[1]] == "(Intercept)", ])

    ## split coefficients and pvalue
    .coef <- .df[, -1]
    .coef[is.na(.coef)] <- ""

    .e <- .coef[, grep("e", names(.coef))]
    if (is.character(.e)) {
        .e <- data.frame(cbind(.e), stringsAsFactors = FALSE)
    }
    .se <- .coef[, grep("s", names(.coef))]
    if (is.character(.se)) {
        .se <- data.frame(cbind(.se), stringsAsFactors = FALSE)
    }

    ## variable, coefficient names
    .y.name <- names(.df)[1]
    .coef.names <- .df[[1]]

    .combine <- do.call(
        rbind,
        lapply(1:nrow(.e), function(z) {
            .e <- .e[z, ]
            .se <- .se[z, ]
            names(.se) <- names(.e)
            data.frame("terms" = c(.coef.names[z], "", ""), "|",
                       rbind(.e, .se, ""), stringsAsFactors = FALSE)
        })
    )

    # if (length(.models) == 1) {
    #     names(.combine) <- c("terms", "|", "X1")
    # }

    .df <- rbind(c("", "|", rep(.y.name, ncol(.e))),
                 addDashLines(.combine, .vLine = 2))
    names(.df) <- c("terms", "|", paste0("m", 1:ncol(.e)))
    row.names(.df) <- NULL

    if (.type == "linear") {
        .info.txt <- c("Obs.", "F value", "P>|F|", "R2",
                       "Adj. R2", "Root MSE", "AIC")
    } else {
        .info.txt <- c("Obs.", "LR chi2", "P>|chi2|", "Pseudo R2",
                       "Log Link", "AIC")
    }

    ## first first table
    .info <- do.call(
        cbind,
        lapply(.models, function(z) {
            .info <- z[[1]]
            .info <- .info[-c(1, nrow(.info)), ]
            .info[, "Info"] <- .info.txt
            .info
        })
    )

    .info <- data.frame(cbind(.info[, 1], "|",
                              .info[, names(.info) %in% "Statistics"]),
                        stringsAsFactors = FALSE)
    names(.info) <- names(.df)

    .df <- rbind(.df, .info)


    # print
    .args <- as.list(match.call())
    printText2(.df, paste0("Models: ",
                           paste0("'", .args[-1], "'", collapse = " Vs. ")),
               .printDF = TRUE)
    printMsg("Standard erros in parentheses")
    printMsg("* p<0.05, ** p<0.01, *** p<0.001")

    invisible(list(.df))
}
