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
#' \enumerate{
#'   \item Essential Medical Statistics, Betty R. Kirkwood & Jonathan
#'   A.C. Sterne,
#'   Second Edition.
#'   \item An Introduction to MEdical Statistics, Martin Bland,
#'   Thrid Edition,
#'   \item STATA DATA MANAGEMENT. UCLA: Statistical Consulting Group.
#'    from https://stats.idre.ucla.edu/stata/seminars/stata-data-management/
#'    (accessed Febrary 25, 2020).
#' }
#'
#'
#' @import stats
#'
#' @concept
#'
#' number summary statistics descriptive five number seven number
#'
#' gouped summary grouped by student t test t-test wilcoxon signed rank
#'
#' ANOVA analyis of variance kruskal-wallis
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
#' \dontrun{
#'
#'
#' ## UCLA IDRE Example
#' ## Website:
#' path <- "https://stats.idre.ucla.edu/stat/data/hsbdemo.dta"
#' hsb <- haven::read_dta(path)
#' codebook(hsb)
#'
#'
#'
#'
#' ## single variable
#' summ(hsb, math)
#'
#' ## multiple variables
#' summ(hsb, math, write)
#'
#' ## using colon separator
#' summ(hsb, write:socst, awards, cid)
#'
#' ## the whole dataset
#' summ(hsb)
#'
#'
#'
#'
#' # Example from IDRE UCLA
#' path <- "https://stats.idre.ucla.edu/stat/data/patient_pt1_stata_dm.dta"
#' hosp <- haven::read_dta(path)
#' codebook(hosp)
#'
#'
#' ## to use piping function
#' library(magrittr)
#'
#'
#' ## summary measures
#' summ(hosp)
#'
#' ## grouped summary measures by sex
#' summ(hosp, age, rbc:test2, by = sex)
#'
#'
#' ## remove NA values from summary measures
#' hosp %>%
#'     replace(sex, NA, sex == 12.2) %>%
#'     summ(age, rbc:test2, by = sex, na.rm = FALSE)
#'
#'
#' hosp %>%
#'     replace(sex, NA, sex == 12.2) %>%
#'     summ(age, rbc:test2, by = sex, na.rm = TRUE)
#'
#'
#' ## the whole dataset
#' summ(hosp, by = sex)
#' }
#'
#' @export
summ <- function(data, ... , by = NULL, na.rm = FALSE, rnd = 1)
{
    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    ## assign data into .data for further evaluation
    .data <- data
    .vars.names <- names(.data)


    ## get variable names within three dots to search for duplicates
    .vars <- as.character(enquos(.args, c("data", "by", "row.pct", "na.rm", "rnd")))


    ## Check if colon is there.
    ## if present, retrieve variables between the two variables
    if (any(grepl(":", .vars))) {
        .vars <- do.call(
            c,
            lapply(.vars, function(z) {
                .colon <- grepl(":", z)
                if (.colon) {
                    splitByColon(data, z, .colon)
                } else {
                    z
                }
            })
        )
    }



    ## if .vars is length zero, then .vars is all variables
    if (length(.vars) == 0) {
        .vars <- .vars.names
        .summ.type <- c("numeric", "double", "integer", "logical")

        ## get the types of variables
        .vars.type <- unlist(lapply(data, function(z) {
            .class <- class(unlist(z))[1]
            if (.class == "haven_labelled") {
                .class <- typeof(unlist(z))[1]
            }
            .class
        }))

        ## get only those whose type are in .summ.type
        .vars <- .vars[.vars.type %in% .summ.type]
    }


    ## if no variable is available, stop
    if (length(.vars) == 0) {
        stop(" ... No variable found for tabulation ... ")
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

        ## formulate title
        .sum.txt <- "Summary"
    } else {
        .df <- do.call(
            rbind,
            lapply(.vars, function(z) {
                summ2(.data, z, by, na.rm, rnd)
            })
        )
        .sum.txt <- paste0("Summary grouped by '", .args$by, "'")
    }


    ## add Dash lines
    .df <- addDashLines(.df, .vLine = 2)


    ## add total summary for group summary
    if (by != "NULL") {
        .df.total <- do.call(
            rbind,
            lapply(.vars, function(z) {
                summ1(.data, z, na.rm, rnd)
            })
        )

        ## subset output show only Obs. to Q3
        .display <- c("Variable", "|",
                      "Obs.", "NA.", "Mean", "Std.Dev", "Median", "Q1", "Q3",
                      "Normality")

        .df.total <- .df.total[, .display]
        ## add pvalue back to .df
        .df.total$p1 <- .df.total$p2 <- rep("", nrow(.df.total))
        names(.df.total)[(ncol(.df.total)-1):ncol(.df.total)] <- names(.df)[11:12]
        .df <- rbind(.df, .df.total)
    }



    ## constructs labels
    ## add label for by: cross-tabulation
    .lbl <- sapply(.vars, function(z) attr(.data[[z]], "label"))

    ## Print tabulation
    printText2(.df, .sum.txt, .printDF = TRUE)

    ## print labels
    if (any(.lbl != "NULL")) {
        printMsg("Labels")
    }
    sapply(1:length(.vars), function(z) {
        if (.lbl[z] != "NULL") {
            printMsg(paste0(.vars[z], ": ", .lbl[z]))
        }
    })


    ## print by label
    getnPrintLabel(.data, .args$by)

    invisible(.df)
}




# Helpers -----------------------------------------------------------------


summ1 <- function(data, x, na.rm = FALSE, rnd = 1)
{
    ## assign as .data and .x for further evaluation
    .data <- data
    .x.name <- x
    .x <- data[[x]]


    ## get number of missing values
    .len <- ifelse(na.rm, length(.x[!is.na(x)]), length(.x))

    .na <- length(.x[is.na(.x)])

    ## assign na.rm as TRUE for all future calculation
    na.rm <- TRUE


    ## construct 7 number summary statistics
    .mu <- mean(.x, na.rm = na.rm)
    .std <- sd(.x, na.rm = na.rm)
    # .cv <- std / mu * 100
    .q <- round(quantile(.x, probs = c(0, .25, .5, .75, 1), na.rm = na.rm), rnd)
    .v <- round(c(.mu, .std, .q), rnd)

    ## get p value from normality test
    pvalue <- tryCatch({
        suppressWarnings(shapiro.test(.x)$p.value)
    }, error = function(err) {
        return(NA)
    })

    pvalue <- sprintf(pvalue, fmt = '%#.3f')


    ## final .df for return
    .df <- data.frame(Variable = .x.name, "|" = "|",
                      Obs. = .len, NA. = .na, Mean = .v[1], Std.Dev = .v[2],
                      Median = .v[5], Q1 = .v[4], Q3 = .v[6],
                      Min = .v[3], Max = .v[7],
                      Normality = pvalue,
                      stringsAsFactors = FALSE)

    names(.df)[2] <- "|"
    row.names(.df) <- NULL

    return(.df)
}


summ2 <- function(data, x, by, na.rm = FALSE, rnd = 1)
{
    ## assign as .data and .x for further evaluation
    .data <- data
    .x.name <- x
    .x <- data[[x]]
    .by.name <- by
    .by <- data[[by]]


    ## check NA
    .useNA <- ifelse(na.rm, "no", "ifany")


    ## get levels of character and process NA value if any
    .tbl <- table(.by, useNA = .useNA)
    .lvl <- names(.tbl)
    .lvl[is.na(.lvl)] <- "<NA>"


    .df <- do.call(
        rbind,
        lapply(.lvl, function(z) {
            if (z == "<NA>") {
                .d <- .data[is.na(.by), ]
            } else {
                .d <- .data[.by == z, ]
            }
            .d <- summ1(.d, .x.name, rnd = rnd)
            .d[1, "Variable"] <- paste0("[", z, "]", .x.name)
            .d
        })
    )


    ## subset output show only Obs. to Q3
    .display <- c("Variable", "|",
                  "Obs.", "NA.", "Mean", "Std.Dev", "Median", "Q1", "Q3",
                  "Normality")

    .df <- .df[, .display]



    ## get pvalue  from ANOVA and Kruskal Wallis or t.test / Wilcox

    ## calculate p-values from ANOVA and Kruskal Wallis or t.test / Wilcox
    if (length(.lvl) > 2) {
        pvalue <- tryCatch({suppressWarnings(summary(aov(.x ~ .by))[[1]][1,5])},
                           error = function(cnd) {return(NA)})

        pvalue <- c(
            pvalue,
            tryCatch({suppressWarnings(kruskal.test(.x ~ .by)$p.value)},
                     error = function(cnd) {return(NA)}))

        .pvalue.name <- c("ANOVA", "K-Wallis")
    } else {
        pvalue <- tryCatch({suppressWarnings(t.test(.x ~ .by)$p.value)},
                           error = function(cnd) {return(NA)})

        pvalue <- c(
            pvalue,
            tryCatch({suppressWarnings(suppressWarnings(wilcox.test(.x ~ .by)$p.value))},
                     error = function(cnd) {return(NA)}))

        .pvalue.name <- c("t-test", "Wilcoxon")
    }
    pvalue <- sprintf(pvalue, fmt = '%#.3f')


    ## add pvalue back to .df
    .df$p1 <- c(pvalue[1], rep("", nrow(.df) - 1))
    .df$p2 <- c(pvalue[2], rep("", nrow(.df) - 1))
    names(.df)[(ncol(.df)-1):ncol(.df)] <- .pvalue.name


    return(.df)
}
