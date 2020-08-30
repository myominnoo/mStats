#' @title Number Summary for numerical data
#'
#' @description
#'
#' \code{summ()} generates summary statistics for numerical data as well as
#' grouped summary measures.
#'
#' @param data Dataset
#' @param ... Variable or multiple variables
#' Colon separator \code{:} can be used to specify multiple variables.
#' @param by Varaiable for cross-tabulation
#' @param na.rm A logical value to specify missing values,
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#' @param test `TRUE` calculates p-values from relevant
#' statistical tests. `FALSE` displays minimum and maximum
#' instead of p-values.
#'
#'
#' @details
#'
#' \code{summ()} reports seven number summary statistics, normality and other additional
#' metadata.
#'
#' \preformatted{summ(data, var1)}
#'
#' \preformatted{summ(data, var1, var2, var3:var5, var10)}
#'
#' \preformatted{summ(data)}
#'
#' Normality test is perfomed by Shapiro-Wilk Normality Test. See more at
#' \code{\link{shapiro.test}}.
#'
#''
#' \strong{ANNOTATIONS}
#'
#' \code{Obs.} = observation
#'
#' \code{NA.} = missing data
#'
#' \code{Mean} = Mean value
#'
#' \code{Std.Dev} = Standard deviation
#'
#' \code{Median} = Median value
#'
#' \code{Q1} = First quartile or percentile
#'
#' \code{Q3} = Third quartile or percentile
#'
#' \code{Min} = Minimum value
#'
#' \code{Max} = Maximum value
#'
#' \code{Normality} = P-value from Shapiro-Wilk Normality Test
#'
#'
#' \strong{`Grouped Summary Measures`}
#'
#' If `by` is specified, grouped summary measures are calculated and
#' produced five number summary, excluding minimum and maximum. In addition,
#' if levels of `by` are more than 2, p-values from ANOVA and Kruskal Wallis tests
#' are displayed. Otherwise, Student's t-test and Wilcoxon signed rank test are
#' measured and their respective p-values are tabulated.
#'
#' There are two parts of the final table. The first part tabulates
#' grouped summary measures and second part tabulates one-variable summary
#' measures for corresponding variables.
#'
#'
#' \preformatted{summ(data, var1, var2, by = var3)}
#'
#' \preformatted{summ(data, var1, var2, var3:var5, var10, by = var11)}
#'
#' \preformatted{summ(data, by = var11)}
#'
#'
#' \strong{Using colon `:` spearator}
#'
#' Colon separator \code{:} can be used to indicate sequence of variables.
#'
#'
#' \preformatted{summ(data, var1, var2, var3:var5, var10)}
#'
#'
#'
#' @return
#'
#' summary measures as \code{data.frame}
#'
#'
#'
#' @references
#'
#' Betty R. Kirkwood, Jonathan A.C. Sterne (2006, ISBN:978–0–86542–871–3)
#'
#' @import stats
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
#' ## use iris dataset
#' data(iris)
#'
#' summ(iris, Sepal.Length)
#' summ(iris, Sepal.Length:Petal.Width)
#'
#' summ(iris)
#'
#' summ(iris, by = Species)
#' @export
summ <- function(data, ... , by = NULL, na.rm = FALSE, rnd = 1, test = TRUE)
{
    ## match call arguments
    .args <- as.list(match.call())

    ## copy data to .data
    .data <- data

    ## get names of dataset and headings
    .data_name <- deparse(substitute(data))
    .vars_names <- names(.data)

    ## if input is not a data.frame, stop
    if (!is.data.frame(.data)) {
        stop("`.data` must be a data.frame", call. = FALSE)
    }


    ## get variable names within three dots to search for duplicates
    .vars <- enquotes(.args, c("data", "by", "row.pct",
                               "na.rm", "rnd", "test"))


    ## check colon, and check data types if the whole dataset
    .vars <- checkEnquos(.data, .vars, "summ")

    ## if no variable is available, stop
    if (length(.vars) == 0) {
        stop("No numerical variables are found for tabulation.",
             call. = FALSE)
    }

    ## summary statistics
    by <- as.character(.args$by)
    by <- ifelse(length(by) == 0, "NULL", by)
    if (by == "NULL") {
        .df <- do.call(
            rbind,
            lapply(.vars, function(z) {
                summ1(.data, z, na.rm, rnd)
            })
        )
        .txt <- "Summary"
    } else {
        .df <- do.call(
            rbind,
            lapply(.vars, function(z) {
                summ2(.data, z, by, na.rm, rnd, test)
            })
        )
        .df <- .df[-1, ]
        .txt <- paste0("Summary grouped by '", .args$by, "'")
    }

    ## add Dash lines
    .df <- addDashLines(.df, .vline = 2)


    ## add label for further processing
    attr(.df, "label") <- "summary"
    attr(.df, "grouped") <- ifelse(by == "NULL", FALSE, TRUE)
    attr(.df, "test") <- test

    ## constructs labels
    ## add label for by: cross-tabulation
    .lbl <- sapply(.vars, function(z) attr(.data[[z]], "label"))

    ## Print tabulation and labels
    printDF(.df, .txt)
    sapply(1:length(.vars), function(z) {
        # printDF(.df[[z]], .txt)
        printLabel(.data, .vars[z])
    })

    ## print label for by variable
    printLabel(.data, .args$by)

    invisible(.df)
}



# Helpers -----------------------------------------------------------------

summ1 <- function(data, x, na.rm = FALSE, rnd = 1)
{
    ## copy data to .data
    .data <- data
    ## create single vector .x
    .x <- .data[[x]]

    ## get number of missing values
    .len <- ifelse(na.rm, length(.x[!is.na(.x)]), length(.x))
    .na <- length(.x[is.na(.x)])

    ## assign na.rm as TRUE for all future calculation
    na.rm <- TRUE


    ## construct 7 number summary statistics
    .mu <- mean(.x, na.rm = na.rm)
    # sprintf(, fmt = paste0("%#.", rnd, "f"))
    .std <- sd(.x, na.rm = na.rm)
    .q <- round(quantile(.x, probs = c(0, .25, .5, .75, 1), na.rm = na.rm), rnd)
    .v <- round(c(.mu, .std, .q), rnd)
    .v <- sprintf(.v, fmt = paste0("%#.", rnd, "f"))



    ## get p value from normality test
    pvalue <- tryCatch({
        suppressWarnings(shapiro.test(.x)$p.value)
    }, error = function(err) {
        return(NA)
    })

    pvalue <- sprintf(pvalue, fmt = '%#.3f')

    ## final .df for return
    .df <- data.frame(Variables = x,
                      Obs. = .len, NA. = .na,
                      Mean = .v[1], Std.Dev = .v[2],
                      Median = .v[5], Q1 = .v[4], Q3 = .v[6],
                      Min = .v[3], Max = .v[7],
                      Normality = pvalue)
    row.names(.df) <- NULL
    return(.df)
}

summ2 <- function(data, x, by, na.rm = FALSE, rnd = 1, test = TRUE)
{
    ## copy data to .data
    .data <- data
    ## create single vector .x
    .x <- .data[[x]]
    .by <- .data[[by]]

    ## check NA
    .useNA <- ifelse(na.rm, "no", "ifany")

    ## get levels of character and process NA value if any
    .lvl <- names(table(.by, useNA = .useNA))
    .lvl[is.na(.lvl)] <- "<NA>"

    ## get summary measures
    .df <- do.call(
        rbind,
        lapply(.lvl, function(z) {
            if (z == "<NA>") {
                .data <- .data[is.na(.by), c(x, by)]
            } else {
                .data <- .data[.by == z, c(x, by)]
            }

            .df <- summ1(.data, x, rnd = rnd)
            .df[1, "Variables"] <- paste0("[", z, "]", x)
            .df
        })
    )
    .df <- .df_noTest <- rbind(.df, summ1(.data, x, rnd = rnd))



    ## subset output show only Obs. to Q3
    .display <- c("Variables", "Obs.", "NA.", "Mean", "Std.Dev",
                  "Median", "Q1", "Q3", "Normality")
    .df <- .df[, .display]


    ## get pvalue  from ANOVA and Kruskal Wallis or t.test / Wilcox

    ## calculate p-values from ANOVA and Kruskal Wallis or t.test / Wilcox
    if (length(.lvl) > 2) {
        pvalue <- tryCatch(
            {suppressWarnings(summary(aov(.x ~ .by))[[1]][1,5])},
            error = function(cnd) {return(NA)}
        )

        pvalue <- c(
            pvalue,
            tryCatch(
                {suppressWarnings(kruskal.test(.x ~ .by)$p.value)},
                error = function(cnd) {return(NA)})
        )

        .pvalue.name <- c("ANOVA", "K-Wallis")
    } else {
        pvalue <- tryCatch(
            {suppressWarnings(t.test(.x ~ .by)$p.value)},
            error = function(cnd) {return(NA)}
        )

        pvalue <- c(
            pvalue,
            tryCatch(
                {suppressWarnings(suppressWarnings(wilcox.test(.x ~ .by)$p.value))},
                error = function(cnd) {return(NA)})
        )

        .pvalue.name <- c("t-test", "Wilcoxon")
    }
    pvalue <- sprintf(pvalue, fmt = '%#.3f')


    ## add pvalue back to .df
    .df$p1 <- c(pvalue[1], rep("", nrow(.df) - 1))
    .df$p2 <- c(pvalue[2], rep("", nrow(.df) - 1))
    names(.df)[(ncol(.df)-1):ncol(.df)] <- .pvalue.name

    if (!test) {
        .df <- .df_noTest
    }

    ## add dash lines and overall summary measures
    .df <- addDashLines(.df)
    .df <- rbind(.df[-(nrow(.df)-1), ], .df[nrow(.df)-1, ])

    return(.df)
}

