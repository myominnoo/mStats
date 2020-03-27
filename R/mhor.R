#' @title Calculating Odds Ratios
#'
#' @description
#' \code{mhor()} calculates odds ratios, Mantel Haenszel pooled estimates and
#' 95% CI.
#'
#' @param data Dataset
#' @param ... Variable or multiple variables
#' Colon separator \code{:} can be used to specify multiple variables.
#' @param by Varaiable for cross-tabulation
#' @param strata Variable for stratification
#' @param exp_value value for exposure as reference
#' @param case_value value for outcome as reference
#' @param plot logical value to display plots of odds ratios
#' including MH estimates across a categorical variable
#' @param na.rm A logical value to specify missing values, `NA` in the table
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#'
#' @details
#'
#'
#' Value can be set as baseline by specifying `exp_value`. This is used
#' when the exposed and case values are not in the right place.
#'
#' It produces a table with Odds Ratio,95% CI as well as
#' p-value. If \code{strata} is specified, `Mantel-Haenzsel` Pooled
#' estimates of `Odds Ratio` is generated along with Chi-sqaured test for heterogeneity.
#'
#' \strong{Odds Ratio, OR}
#'
#' \deqn{OR = (D1 x H0) / (D0 x H1)}
#'
#' \strong{Error Factor, EF using Woolf's formula}
#'
#' \deqn{95\% CI = OR / EF or OR x EF}
#'
#' \deqn{EF = exp(1.96 x SE(log(OR)))}
#'
#' \deqn{SE(log(OR)) = \sqrt{1/D1 + 1/H1 + 1/D0 + 1/H0}}
#'
#' \strong{Calculating p-value from Wald's z test}
#'
#' \deqn{z = log OR / SE (log OR)}
#'
#'
#'
#' \strong{Mantel-Haenszel's OR}
#'
#' \deqn{ORMH = Q / R}
#'
#' \deqn{Q = \sum{(D1i x H0i) / ni}}
#'
#' \deqn{R = \sum{(D0i x H1i) / ni}}
#'
#' \strong{Calculating CI for MH-OR}
#'
#' \deqn{95\% CI = OR / EF or OR x EF}
#'
#' \deqn{SE(ORMH) = \sqrt{V / (Q x R)}}
#'
#' \deqn{V = \sum{(Di x Hi x n0i x n1i) / ( (ni)^2 x (ni - 1))}}
#'
#' \strong{Chi-square test for MHOR, df = 1}
#'
#' \deqn{X^2 (MH), Chi-square value = U^2 / V}
#'
#' \deqn{U = O - E}
#'
#' \deqn{O = \sum{D1i}}
#'
#' \deqn{E = \sum{Di x n1i / ni}}
#'
#'
#' \strong{Chi-square test for Heterogeneity}
#'
#' \deqn{X^2 = \sum{(D1i x H0i - ORMH x D0i x H1i)^2 / ORMH x Vi x ni^2}}
#'
#' @references
#'
#' \enumerate{
#'     \item Betty R. Kirkwood, Jonathan A.C. Sterne (2006, ISBN:978–0–86542–871–3)
#'     \item B. Burt Gerstman (2013, ISBN:978-1-4443-3608-5)
#'     \item Douglas G Altman (2005, ISBN:0 7279 1375 1)
#' }
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
#'
#' ### Example from Essential Medical Statistics
#' # Page 178, Chapter 18: Controlling for confounding: Stratification
#' lepto <- expandTables(
#'     male = c(36, 14, 50, 50), female = c(24, 126, 10, 90),
#'     exp_name = "area", exp_lvl = c("Rural", "Urban"),
#'     case_name = "ab", case_lvl = c("Yes", "No"),
#'     strata_name = "gender"
#' )
#'
#' ## label variables and data
#' lepto <- labelData(lepto, "Prevalence survey of leptospirosis in West Indies")
#' lepto <- labelVar(lepto,
#'               c(area, ab, gender),
#'               c("Type of area", "Leptospirosis Antibodies",
#'                 "Gender: Male or female"))
#'
#' ## check dataset
#' codebook(lepto)
#'
#' ## Calculate OR
#' mhor(lepto, area, by = ab, case_value = "Yes")
#'
#' ## calculate stratified OR
#' mhor(filter(lepto, gender == "male"), area, by = ab, case_value = "Yes")
#' mhor(filter(lepto, gender == "female"), area, by = ab, case_value = "Yes")
#'
#'
#' ## Calculate MHOR
#' mhor(lepto, area, by = ab, strata = gender, case_value = "Yes")
#'
#' \dontrun{
#' ### Simpson's Paradox: Example from Burt Gerstman's Epidemiology
#' ### Chapter 14, Page 326
#'
#' library(magrittr)
#' simpson <- expandTables(
#'     "1" = c(1000, 9000, 50, 950),
#'     "2" = c(95, 5, 5000, 5000),
#'     exp_name = "treatment",
#'     exp_lvl = c("new", "standard"),
#'     case_name = "outcome",
#'     case_lvl = c("alive", "dead"),
#'     strata_name = "clinic") %>%
#'     labelVar(c(treatment, outcome, clinic),
#'               c("Treatment: new or standard",
#'                 "Outcome: alive or dead", "clinic: 1 or 2")) %>%
#'     labelData("Example of Simpson's Paradox")
#'
#' ## checking structure
#' codebook(simpson)
#'
#' ## Calculate OR
#' mhor(simpson, treatment, by = outcome)
#'
#' ## Calculate MHOR
#' mhor(simpson, treatment, by = outcome, strata = clinic)
#'
#'
#'
#'
#'
#' ## Exposure to passive smoking among female lung cancer cases
#' ## and controls in four studies
#' ## Statistics with confidence, Douglas G Altman, second edition
#' ## Chapter 7, page 64
#'
#' lung <- expandTables(
#'     "1" = c(14, 61, 8, 72),
#'     "2" = c(33, 164, 8, 32),
#'     "3" = c(13, 15, 11, 10),
#'     "4" = c(91, 254, 43, 148),
#'     exp_name = "smoking",
#'     exp_lvl = c("Yes", "No"),
#'     case_name = "cancer",
#'     case_lvl = c("case", "control"),
#'     strata_name = "study"
#' ) %>%
#'     labelVar(c(smoking, cancer, study),
#'              c("Passive Smoking", "Female lung cancer cases and controls",
#'                "Study Number")) %>%
#'     labelData(paste0("Passive smoking among female lung cancer cases",
#'                      "and controls in four studies"))
#'
#' ## check dataset
#' codebook(lung)
#'
#'
#' ## calculate OR
#' mhor(lung, smoking, by = cancer, exp_value = "Yes")
#'
#' ## calculate MHOR
#' mhor(lung, smoking, by = cancer, strata = study, exp_value = "Yes")
#' }
#'
#' @export
mhor <- function(data, ... , by, strata = NULL, exp_value = NULL, case_value = NULL,
                 plot = TRUE, na.rm = FALSE, rnd = 3)
{
    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    ## assign data into .data for further evaluation
    .data <- data

    ## get variable names within three dots to search for duplicates
    ## <<<< Change this ----- <<<<<<<<
    .vars <- as.character(enquos(.args, c("data", "by", "strata", "exp_value",
                                          "case_value", "plot", "na.rm", "rnd")))

    ## check colon, and check data types if the whole dataset
    .vars <- checkEnquos(.data, .vars, .types = "tab")



    ## if by not specify, stop
    by <- as.character(.args$by)
    if (length(by) == 0) {
        stop(" ... Specify 'by' ... ")
    }

    strata <- as.character(.args$strata)
    if (length(strata) == 0) {
        .tbl <- lapply(.vars, function(z) {
            tabOR(.data, z, by, exp_value, case_value, na.rm, rnd)
        })
        .df <- do.call(
            rbind,
            lapply(.tbl, function(z) z)
        )
        .txt <- paste0("Odds Ratio Estimates of '", .args$by, "'")
    } else {
        .tbl <- lapply(.vars, function(z) {
            tabMHOR(.data, z, by, strata, exp_value, case_value, plot, na.rm, rnd)
        })
        .df <- do.call(
            rbind,
            lapply(1:length(.tbl[[1]]), function(z) .tbl[[1]][[z]])
        )
        .txt <- paste0("Mantel-Haenszel Pooled Estimates of '", .args$by, "'")
    }


    ## constructs labels
    ## add label for by: cross-tabulation
    .lbl <- sapply(.vars, function(z) attr(.data[[z]], "label"))

    ## Print tabulation
    sapply(1:length(.vars), function(z) {
        printText2(.df, .txt, .printDF = TRUE)
        if (.lbl[z] != "NULL") {
            printMsg("Labels")
            printMsg(paste0(.vars[z], ": ", .lbl[z]))
        }
    })


    ## print by label
    .by.name <- as.character(.args$by)
    .by.lbl <- attr(.data[[.by.name]], "label")
    if (!is.null(.by.lbl)) {
        printMsg(paste0(.by.name, ": ", .by.lbl))
    }

    ## print by label
    .strata.name <- as.character(.args$strata)
    if (length(.strata.name) > 0) {
        .strata.lbl <- attr(.data[[.strata.name]], "label")
    } else {
        .strata.lbl <- NULL
    }
    if (!is.null(.strata.lbl)) {
        printMsg(paste0(.strata.name, ": ", .strata.lbl))
    }

    invisible(list(.df))
}



# Helpers -----------------------------------------------------------------



calcOR <- function(.tbl, .x.name, rnd = 3) {
    .a <- .tbl[1, 1]
    .b <- .tbl[1, 2]
    .c <- .tbl[2, 1]
    .d <- .tbl[2, 2]
    .or <- (.a * .d) / (.b * .c)
    .se <- sqrt((1/.a) + (1/.b) + (1/.c) + (1/.d))
    .ll <- exp(log(.or) - (1.96 * .se))
    .ul <- exp(log(.or) + (1.96 * .se))

    ## test for null hypothesis
    ## equal to p-value from chi-square test
    .chi <- tryCatch({
        suppressWarnings(chisq.test(.tbl, correct = FALSE)$p.value)
    }, error = function(err) {
        return(NA)
    })

    ## create data frame
    .df <- data.frame(
        cbind(rownames(.tbl), "|",
              .tbl,
              rbind(c(sprintf(.or, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.ll, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.ul, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.chi, fmt = paste0('%#.', rnd, 'f'))),
                    c("", "", "", ""))
        ), stringsAsFactors = FALSE
    )
    names(.df) <- c(.x.name, "|", colnames(.tbl),
                    "Odds Ratio", "[95% Conf.", "Interval]", "P>|z|")


    row.names(.df) <- NULL

    ## add dash lines
    addDashLines(.df, .vLine = 2)
}


tabOR <- function(.data, .x, .by, exp_value = NULL, case_value = NULL,
                  na.rm = FALSE, rnd = 1)
{
    .x.name <- .x
    .by.name <- .by
    .x <- .data[[.x]]
    .by <- .data[[.by]]


    ## check NA
    .useNA <- ifelse(na.rm, "no", "ifany")


    ## create tables
    .tbl <- table(.x, .by, useNA = .useNA)

    ## if by is not binary, stop
    if (length(colnames(.tbl)) > 2)
        stop(" ... 'by' must be binary ... ")

    ## change row and col orders
    .tbl <- rowColOrder(.tbl, exp_value, case_value)

    .na <- is.na(row.names(.tbl))
    if (any(.na)) {
        row.names(.tbl)[.na] <- "NA"
    }

    ## split tables to get 2x2 tables
    if (nrow(.tbl) > 2) {
        .tbl <- splitTables(.tbl, exp_value)
    } else {
        .tbl <- list(.tbl)
    }


    ## calcalate OR
    .tbl <- lapply(.tbl, function(z) {
        calcOR(z, .x.name, rnd)
    })

    ## combine if more than one results
    .df <- NULL
    for (i in 1:length(.tbl)) {
        if (i == 1) {
            .df <- .tbl[[i]]
        } else {
            .df <- rbind(.df, .tbl[[i]][-1, ])
        }
        row.names(.df) <- NULL
    }

    return(.df)
}
tabMHOR <- function(.data, .x, .by, .strata, exp_value = NULL, case_value = NULL,
                    plot = TRUE, na.rm = FALSE, rnd = 1)
{
    .x.name <- .x
    .by.name <- .by
    .x <- .data[[.x]]
    .by <- .data[[.by]]

    ## process strata. if na.rm, remove NA value
    .strata <- .data[[.strata]]
    .strata.lvl <- unique(.strata)
    if (na.rm) {
        .strata.lvl <- .strata.lvl[!is.na(.strata.lvl)]
        .x <- .x[!is.na(.strata.lvl)]
        .by <- .by[!is.na(.strata.lvl)]
    }


    ## check NA
    .useNA <- ifelse(na.rm, "no", "ifany")

    ## create tables
    .tbl <- table(.x, .by, useNA = .useNA)


    ## if by is not binary, stop
    if (length(colnames(.tbl)) > 2)
        stop(" ... 'by' must be binary ... ")


    ## split tables to get 2x2 tables
    ## and get row names
    .tbl <- splitTables(.tbl, exp_value)
    .rownames <- lapply(.tbl, function(z) {
        rownames(z)
    })


    .df <- lapply(.rownames, function(z) {
        .equal <- .x %in% z
        .x <- .x[.equal]
        .by <- .by[.equal]
        .strata <- .strata[.equal]
        splitStrataOR(.x, .by, .strata, .x.name, .by.name, exp_value, case_value, plot,
                    na.rm, rnd)
    })

    .df
}


splitStrataOR <- function(.x, .by, .strata, .x.name, .by.name, exp_value = NULL,
                        case_value = NULL, plot, na.rm, rnd)
{
    .strata.lvl <- unique(.strata)
    if (na.rm) {
        .strata.lvl <- .strata.lvl[!is.na(.strata.lvl)]
    }


    ## check NA
    .useNA <- ifelse(na.rm, "no", "ifany")

    ## calculate MHOR
    .tbl <- lapply(.strata.lvl, function (z) {
        calcMHOR(.strata, z, .x, .x.name, .by,  exp_value, case_value, .useNA, rnd)
    })


    ## check exposure value
    .tbl1 <- do.call(rbind, lapply(.tbl, function(z) {
        if (is.null(exp_value)) {
            z[1, ]
        } else {
            z[z[, .x.name] == exp_value, ]
        }
    }))

    ## calculate MHOR and 95% CI
    .derive <- sapply(.tbl1[, c("r", "q", "v", "o", "e", "b", "c", "d", "n")],
                      FUN = sum)
    .mhor <- .derive["q"] / .derive["r"]
    .se <- sqrt(.derive["v"] / (.derive["q"] * .derive["r"]))
    .ef <- exp(1.96 * .se)
    .ll <- .mhor / .ef
    .ul <- .mhor * .ef

    ## Test for null hypothesis
    .u <- .derive["o"] - .derive["e"]
    .x2 <- .u^2 / .derive["v"]
    .chi <- tryCatch({
        suppressWarnings(pchisq(.x2, df = 1, lower.tail = FALSE))
    }, error = function(err) {
        return(NA)
    })


    ## calculate test for heterogeneity
    .hetero <- ((.tbl1[, "o"] * .tbl1[, "d"]) -
            (.mhor * .tbl1[, "c"] * .tbl1[, "b"]))^2 /
        (.mhor * .tbl1[, "v"] * (.tbl1[, "n"]^2))
    .hetero <- sum(.hetero)
    .hetero <- pchisq(.hetero, df = 1, lower.tail = FALSE)
    .hetero <- tryCatch({
        suppressWarnings(pchisq(.hetero, df = 1, lower.tail = FALSE))
    }, error = function(err) {
        return(NA)
    })



    ## add dash lines and process df
    .tbl <- lapply(.tbl, function(z) {
        .df <- z[, 1:9]
        .df <- cbind(.df[, 1:5],
                     rbind(.df[1, 6:9],
                           rep("", 4)))
        addDashLines(.df, .vLine = 3)
    })


    ## combine if more than one results
    .df <- NULL
    for (i in 1:length(.tbl)) {
        if (i == 1) {
            .df <- .tbl[[i]]
        } else {
            .df <- rbind(.df, .tbl[[i]][-1, ])
        }
        row.names(.df) <- NULL
    }


    .df <- rbind(.df,
                 c("Pooled", "estimate", "of", "MH", "OR",
                   sprintf(c(.mhor, .ll, .ul, .chi), fmt = paste0('%#.', rnd, 'f'))))

    .df <- rbind(.df,
                 c("", "", "", "", "", "Chi-Squared", "test of Hetero", "geneity",
                   sprintf(.hetero, fmt = paste0('%#.', rnd, 'f'))))

    row.names(.df) <- NULL



    ## plot OR and 95% CI
    if (plot) {
        .or <- as.numeric(c(.tbl1[["Odds Ratio"]], .mhor))
        .ll <- as.numeric(c(.tbl1[["[95% Conf."]], .ll))
        .ul <- as.numeric(c(.tbl1[["Interval]"]], .ul))
        names(.or) <- c(.tbl1[["Strata"]], "MHOR")
        plotRisks(.or, .ll, .ul, .x.name, .by.name, "Odds Ratios")
    }

    .df
}



calcMHOR <- function(.strata, .strata.lvl, .x, .x.name, .by, exp_value, case_value,
                     .useNA, rnd)
{
    if (is.na(.strata.lvl)) {
        .equal <- is.na(.strata)
    } else {
        .equal <- .strata == .strata.lvl
    }
    .x <- .x[.equal]
    .by <- .by[.equal]

    ## create tables
    .tbl <- table(.x, .by, useNA = .useNA)

    ## if by is not binary, stop
    if (length(colnames(.tbl)) > 2)
        stop(" ... 'by' must be binary ... ")

    ## change row and col orders
    .tbl <- rowColOrder(.tbl, exp_value, case_value)


    .a <- .tbl[1, 1]
    .b <- .tbl[1, 2]
    .c <- .tbl[2, 1]
    .d <- .tbl[2, 2]

    .or <- (.a * .d) / (.b * .c)
    .se <- sqrt((1/.a) + (1/.b) + (1/.c) + (1/.d))
    .ll <- exp(log(.or) - (1.96 * .se))
    .ul <- exp(log(.or) + (1.96 * .se))

    ## test for null hypothesis
    ## equal to p-value from chi-square test
    .chi <- tryCatch({
        suppressWarnings(chisq.test(.tbl, correct = FALSE)$p.value)
    }, error = function(err) {
        return(NA)
    })

    or <- sprintf(c(.or, .ll, .ul, .chi), fmt = paste0('%#.', rnd, 'f'))

    ## calculation for MHOR
    .n <- sum(.tbl)
    .q <- .a * .d / .n
    .r <- .c * .b / .n

    .case <- .a + .c
    .control <- .b + .d
    .n1 <- .a + .b
    .n0 <- .c + .d
    .v.de <- (.n^2) * (.n - 1)
    .v <- (.case * .control / .v.de) * .n1 * .n0
    .e <- (.case * .n1) / .n

    ## create result data
    .df <- data.frame(cbind(.strata.lvl, x.name = row.names(.tbl), "|",
                            .tbl, rbind(or, or)),
                      cbind(.r, .q, .v, .a, .e, .b, .c, .d, .n),
                      stringsAsFactors = FALSE)
    names(.df) <- c("Strata", .x.name, "|", colnames(.tbl),
                    "Odds Ratio", "[95% Conf.", "Interval]", "P>|x2|",
                    "r", "q", "v", "o", "e", "b", "c", "d", "n")


    row.names(.df) <- NULL

    .df
}
