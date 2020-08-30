#' @title Display formatted regression table
#'
#' @description
#'
#' \code{esttab()} produces publication-quality
#' regression tables which can be exported.
#'
#' @param ... regression functions from `mStats`
#'
#' @details
#'
#' It is an wrapper function after running regression models.
#' Supported functions include \code{\link{regress}} and
#' \code{\link{logit}}. It processes their outputs and generates
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
#'
#' ## running different logistic regression models
#' m1 <- logit(infert, case, education)
#' m2 <- logit(infert, case, education, age)
#' m3 <- logit(infert, case, education, age, parity, induced)
#'
#' ## creating regression tables
#' esttab(m1, m2, m3)
#'
#' @export
esttab <- function(...)
{
    ## match call arguments
    .args <- as.list(match.call())
    ## add labels to the first row
    .m_names <- as.character(.args)[-1]

    ## get models in a list
    .m <- list(...)

    ## if length < 0, stop
    .m_len <- length(.m)
    stopifnot("At least one model must be specified" = .m_len > 0)

    ## process model and extract coefficients
    .t <- lapply(.m, function(z) {
        .m_type <- attr(z$result, "model")
        switch(.m_type,
               logit = estLogit(z),
               lm = estRegress(z))
    })

    ## merge regression tables list
    .df <- mergeRegTab(.t, .m_names)

    ## get diagnostic statistics to the last rows
    .id <- c("1obs", "2pchi", "3aic", "4pr2", "5ll")
    .dx <- .df$id %in% .id
    .df <- rbind(.df[!.dx, ], .df[.dx, ])

    .df <- addDashLines(.df, .vline = 3)
    .dx <- .df$id %in% .id
    .df <- rbind(.df[!.dx, ], .df[.dx, ], .df[1, ])

    ## remove id column
    .df <- .df[, -1]

    ## print regression tables
    printDF(.df, "    Regression table for Comparison")
    ## print default p-value flags
    printText("Standard Errors in parentheses")
    printText("* p < 0.5, ** p < 0.01, *** p < 0.001")

    invisible(.df)
}


# helpers -----------------------------------------------------------------


mergeRegTab <- function(.t, .m_names)
{
    ## create blank df and merge all lists
    .df <- NULL
    for (i in 1:length(.t)) {
        if (i == 1) {
            .df <- .t[[1]]
        } else {
            .df <- merge.data.frame(
                .df, .t[[i]], by = c("id", "var"),
                all.x = TRUE, all.y = TRUE
            )
        }
    }
    ## replace NA with ""
    .df[is.na(.df)] <- ""

    ## name the df
    names(.df) <- c("id", "Predictors", .m_names)

    return(.df)
}

fixCoef <- function(.df)
{
    ## create flag for p-values
    .p <- as.numeric(.df$p)
    .p_flag <- ifelse(.p < 0.001, "***",
                      ifelse(.p < 0.01, "**",
                             ifelse(.p < 0.05, "*", "")))

    ## get point estimates and SE
    .t <- do.call(
        rbind,
        lapply(1:nrow(.df), function(z) {
            .t <- rbind(paste0(.df$e[z], .p_flag[z]),
                        paste0("(", .df$se[z], ")"))
            cbind(paste(.df$var[z], c("e", "se")),
                  c(.df$var[z], ""),
                  .t)
        })
    )

    .t <- data.frame(.t)
    names(.t) <- c("id", "var", "e")

    return(.t)
}

estLogit <- function(.m)
{
    ## get regression table
    .t <- .m$result
    .df <- .t[12:(nrow(.t) - 1), -2]
    ## rename df for easy indexing
    names(.df) <- c("var", "e", "se", "t", "p", "ll", "ul")

    ## fix coefficients from regression tables
    .df <- fixCoef(.df)

    ## get model diagnostic statistics
    .t <- .t[c(2, 4, 6:8), c(1, 3)]
    .t <- cbind(id = c("1obs", "2pchi", "3aic", "4pr2", "5ll"), .t)

    ## give both the same names and row bind
    names(.t) <- names(.df)
    .df <- rbind(.df, .t)

    return(.df)
}

estRegress <- function(.m)
{
    ## get regression table
    .t <- .m$result
    .df <- .t[10:(nrow(.t) - 1), -2]
    ## rename df for easy indexing
    names(.df) <- c("var", "e", "se", "t", "p", "ll", "ul")

    ## fix coefficients from regression tables
    .df <- fixCoef(.df)

    ## get model diagnostic statistics
    .t <- .t[c(2, 3, 5, 6), c(1, 3)]
    .t <- cbind(id = c("1obs", "2pchi", "3aic", "4pr2"), .t)

    ## give both the same names and row bind
    names(.t) <- names(.df)
    .df <- rbind(.df, .t)

    return(.df)
}
