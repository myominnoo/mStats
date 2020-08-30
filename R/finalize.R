#' @title Finalizing summary measures
#'
#' @description
#'
#' \code{finalize()} organizes summary measures into table format
#' to be used in manuscript writing.
#'
#' @param x output from summary measure functions
#'
#' @details
#' Functions allowed:
#' \code{tab} shows freq (percentage). The function can handle both one-way and
#' cross-tabulation.
#'
#' \code{summ} shows mean (SD), median (IQR), Normality.
#' For grouped measures, p-values from comparison tests are reported.
#'
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' ## one-way tabulation
#' result <- tab(infert, education, parity, case)
#' finalize(result)
#'
#' ## cross-tabulation
#' result <- tab(infert, education, parity, by = case)
#' finalize(result)
#'
#' ## summary measures
#' result <- summ(infert)
#' finalize(result)
#'
#' result <- summ(infert, by = education)
#' finalize(result)
#'
#'
#' @export
finalize <- function(x)
{
    .attr <- attr(x, "label")
    switch(.attr,
           tabulation = finalize.tab(x),
           summary = finalize.summ(x))
}


# helpers -----------------------------------------------------------------


finalize.tab <- function(x)
{
    ## numbers of rows and columns
    .nr <- nrow(x)
    .nc <- ncol(x)
    ## get percentage attributes
    .pct <- attr(x, "percentage")
    .pct[is.na(.pct)] <- "NA"

    ## index for dashed rows
    .ndash <- grep("-", x[["Variables"]])

    ## create sequence numbers to extract frequency and percentage
    if (.pct %in% c("TRUE", "FALSE", "one")) {
        .i <- seq(4, .nc - 2, 2)

        ## paste frequency and percentage
        .f <- do.call(
            cbind,
            lapply(.i, function(z) {
                .df <- data.frame(paste0(x[, z], " (", x[, z + 1], ")"))
                names(.df) <- names(x)[z]
                .df
            })
        )
    } else {
        .i <- seq(4, .nc - 2, 1)
        .f <- x[, .i]
    }


    ## for one-way tabulation, remove Cumulative Frequence column
    ## combine all
    if (.pct == "one") {
        .df <- cbind(x[, 1:2], .f)
    } else {
        .df <- cbind(x[, 1:2], .f, x[, (.nc - 1):.nc])
    }
    ## remove dashed lines
    .df <- .df[-.ndash, ]
    row.names(.df) <- NULL

    return(.df)
}
finalize.summ <- function(x)
{
    ## numbers of rows and columns
    .nr <- nrow(x)
    .nc <- ncol(x)

    ## get grouped and test attributes
    .grp <- attr(x, "grouped")
    .test <- attr(x, "test")

    ## index for dashed rows
    .ndash <- grep("-", x[["Obs."]])

    .df <- data.frame(Variables = x[, 1],
                      m.sd = paste0(x[["Mean"]], " (", x[["Std.Dev"]], ")"),
                      m.iqr = paste0(x[["Median"]], " (", x[["Q1"]], "-", x[["Q3"]], ")"),
                      normal = x[["Normality"]]
    )

    ## if test statistics are reported, add to the result
    if (.grp & .test) {
        .df <- cbind(.df, x[, (.nc-1):.nc])

        ## changes variables names
        names(.df)[1:(ncol(.df)-2)] <-
            c("Variables", "mean (SD)", "median (IQR)", "Normality")
    } else {
        ## changes variables names
        names(.df) <-
            c("Variables", "mean (SD)", "median (IQR)", "Normality")
    }
    ## remove dashed lines
    .df <- .df[-.ndash, ]
    row.names(.df) <- NULL

    return(.df)
}
