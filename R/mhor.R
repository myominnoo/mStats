#' @title Calculating Odds Ratios
#'
#' @description
#' \code{mhor()} calculates odds ratios, Mantel Haenszel pooled estimates and
#' 95% CI.
#'
#' @param data Dataset
#' @param exp exposure or independent variables
#' @param case case or dependent variables (outcomes)
#' @param strata if specified, MH OR is calculated.
#' @param exp_value value for exposure as reference
#' @param case_value value for outcome as reference
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#'
#' @details
#'
#'
#' Value can be set as baseline by specifying `exp_value`. This is used
#' when the exposed and case values are not at the right place in 2x2 tables.
#'
#' It produces a table with Odds Ratio,95% CI as well as
#' p-value. If \code{strata} is specified, `Mantel-Haenzsel` Pooled
#' estimates of `Odds Ratio` is generated along with Chi-squared test for
#' homogeneity.
#'
#' Attributable fractions, \code{Attr. Frac. Exp} and  \code{Attr. Frac. Pop}
#' among exposed and population are calculated when OR is greated than or
#' equal to 1.
#' If OR is less than 1, preventable fractions,  \code{Prev. Frac. Exp}
#' and  \code{Attr. Frac. Pop} are calculated.
#'
#' \code{Prop.exp} is proportion among exposed.
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
#' ### Example from Essential Medical Statistics
#' # Page 160, table 16.5 Hypothetical data from a survey to examine
#' # the prevalence of asthma among patients at a particular general practice.
#'
#' gp <- expandTables(c(81, 995, 57, 867),
#'                    exp_name = "sex",
#'                    exp_lvl = c("women", "men"),
#'                    case_name = "asthma",
#'                    case_lvl = c("yes", "no"))
#' gp <- labelData(gp, "Hypothetical data: prevalence of asthma")
#' gp <- labelVar(gp, sex="Patient's sex", asthma="Having asthma")
#'
#' ## check dataset
#' codebook(gp)
#'
#' ## calculate OR
#' mhor(gp, sex, asthma)
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
#' lepto <- labelVar(lepto, area="Type of area", ab = "Leptospirosis Antibodies",
#'                   gender="Gener: Male or Female")
#'
#' ## check dataset
#' codebook(lepto)
#'
#' ## Calculate OR
#' mhor(lepto, area, ab)
#' ## set exposure and case values to desired ones
#' mhor(lepto, area, ab, exp_value = "Rural", case_value = "Yes")
#'
#' ## Calculate MHOR
#' mhor(lepto, area, ab, gender, "Rural", "Yes")
#'
#'
#'
#' @export
mhor <- function(data, exp , case, strata = NULL, exp_value = NULL,
                 case_value = NULL, rnd = 4)
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


    ## get exp and case name
    exp <- as.character(.args$exp)
    case <- as.character(.args$case)
    strata <- as.character(.args$strata)


    ## create single vector .exp
    .exp <- .data[[exp]]
    .case <- .data[[case]]

    ## create table
    .tbl <- table(.exp, .case, useNA = "no")

    ## case var must be binary.
    if (ncol(.tbl) > 2) {
        stop(paste0("'", case, "' must be binary."), call. = FALSE)
    }

    ## if strata is not specified, calculate OR. Otherwise, calculate MHOR
    if (length(strata) == 0) {

        ## change row and col orders
        .tbl <- rowColOrder(.tbl, exp_value, case_value)
        ## split tables if nrow > 2
        .tbl <- splitTables(.tbl, .exp.value = exp_value)

        ## calculate OR
        .df <- do.call(
            rbind,
            lapply(.tbl, function(z) {
                calcOR(z, exp, rnd)
            })
        )

        ## create label
        .lbl <- paste0("      Odds Ratio : '", case, "'")
    } else {
        ## exp var must be binary for MHOR.
        if (nrow(.tbl) > 2) {
            stop(paste0("'", exp, "' must be binary."), call. = FALSE)
        }

        ## get strata levels
        .lvl <- unique(.data[[strata]])
        .mhor <- splitStrata(.data, .exp, .case, strata,
                             .lvl, exp_value, case_value, rnd)
        ## get crude OR
        .crude <- calcOR(.tbl, case, rnd)[7, -2]
        .crude[1, 1] <- "Crude"

        ## get stratum-specific OR
        .df <- do.call(
            rbind,
            lapply(.lvl, function(z) {
                .exp <- .exp[.data[[strata]] == z]
                .case <- .case[.data[[strata]] == z]
                ## create 2x2 tables
                .tbl <- table(.exp, .case, useNA = "no")

                ## change row and col orders
                .tbl <- rowColOrder(.tbl, exp_value, case_value)

                .t <- calcOR(.tbl, case, rnd)[7, -2]
                .t[1, 1] <- z
                .t
            })
        )

        ## change names to row-combine
        names(.mhor) <- names(.crude) <- names(.df) <-
            c(strata, "Odds Ratios", "[95% Conf.", "Interval]", "Pr>chi2")
        .t <- rbind(.crude, .mhor)
        .t <- addDashLines(.t)
        .t <- rbind(.t[-4, ], .t[4, ])

        ## add stratum-specific OR and 95% CI
        .df <- rbind(.df, .t)

        ## add dash lines
        .df <- addDashLines(.df, .vline = 2)

        ## create label
        .lbl <- paste0("                Mantel-Haenszel (M-H) Odds Ratio")
    }


    ## print results
    printDF(.df, .lbl)
    ## print variable labels
    printLabel(.data, exp)
    printLabel(.data, case)
    if (length(strata) != 0) {
        printLabel(.data, strata)
    }

    invisible(.df)
}


# helpers -----------------------------------------------------------------

calcOR <- function(tbl, x, rnd = 3)
{
    ## add rows and then proportions
    .tbl <- tbl
    .row_total <- rowSums(.tbl)
    .col_total <- colSums(cbind(.tbl, .row_total))
    .tbl_all <- rbind(cbind(.tbl, Total = .row_total), Total = .col_total)
    .prp_exp <- .tbl_all[ , 1] / .tbl_all[, 3]


    ## get odds ratios and 95% CI
    .a <- .tbl[1, 1]
    .b <- .tbl[1, 2]
    .c <- .tbl[2, 1]
    .d <- .tbl[2, 2]
    .or <- (.a * .d) / (.b * .c)
    .fisher <- tryCatch({
        suppressWarnings(fisher.test(.tbl))
    }, error = function(cnd) {
        return(NA)
    })
    .confint <- .fisher$conf.int
    ## get p-value chi-square
    .chi <- tryCatch({
        suppressWarnings(chisq.test(.tbl, correct = FALSE))
    }, error = function(cnd) {
        return(NA)
    })


    ## calculate attributable or prevent
    .m1 <- .a + .b
    if (.or >= 1) {
        .afe <- (.or - 1) / .or
        .afe_confint <- (.confint - 1) / .confint
        .afp <- .afe * .a / .m1
        .lbl <- c("Odds ratio" = "Odds Ratio",
                  "Attributable Fraction among exposed" = "Attr. Frac. Exp",
                  "Attributable Fraction among population" = "Attr. Frac. Pop")
    } else {
        .afe <- 1 - .or
        .afe_confint <- c(1 - .confint[2], 1- .confint[1])
        .afp <- (.a / .m1 * .afe) / ((.a / .m1 * .afe) + .or)
        .lbl <- c("Odds ratio" = "Odds Ratio",
                  "Prevented Fraction among exposed" = "Prev. Frac. Exp",
                  "Prevented Fraction among population" = "Prev. Frac. Pop")
    }

    ## combine all in one dataframe
    .df <- as.data.frame(.tbl_all)
    .df <- cbind(row.names(.tbl_all), .df,
                 "Prop.exp" = sprintf(.prp_exp,
                                      fmt = paste0("%#.", rnd, "f")))
    names(.df)[1] <- x


    ## get other estimates
    .est <- data.frame(
        .lbl,
        sprintf(c(.or, .afe, .afp),
                fmt = paste0("%#.", rnd, "f")),
        c(sprintf(c(.confint[1], .afe_confint[1]),
                  fmt = paste0("%#.", rnd, "f")), ""),
        c(sprintf(c(.confint[2], .afe_confint[2]),
                  fmt = paste0("%#.", rnd, "f")), ""),
        c(sprintf(.chi$p.value,
                  fmt = paste0("%#.", rnd, "f")), "", "")
    )
    names(.est) <- names(.df)

    ## combine into final dataframe
    .df <- rbind(.df,
                 c(" ", "Estimates", "[95% Conf.", "Interval]", "Pr>chi2"),
                 .est)
    row.names(.df) <- NULL


    ## add dash lines
    .df <- addDashLines(.df)
    .df <- rbind(.df[2:4, ], .df[1, ], .df[5:8, ])
    .df <- addDashLines(.df, .vline = 2)
    row.names(.df) <- NULL

    return(.df)
}


splitStrata <- function(.data, .exp, .case, strata, .lvl, exp_value, case_value, rnd)
{
    ## calculate MHOR and 95% CI, chi square test for homogeneity
    ## chi-square test for MHOR
    .t <- do.call(
        rbind,
        lapply(.lvl, function(z) {
            .exp <- .exp[.data[[strata]] == z]
            .case <- .case[.data[[strata]] == z]
            .t <- table(.exp, .case, useNA = "no")

            ## change row and col orders
            .t <- rowColOrder(.t, exp_value, case_value)

            calcMHOR(.t)
        })
    )

    .mhor <- sum(.t[["q"]]) / sum(.t[["r"]])
    .mhse <- sqrt(sum(.t[["v"]]) / (sum(.t[["q"]]) * sum(.t[["r"]])))
    .mhconfint <- c(.mhor / exp(1.96 * .mhse), .mhor * exp(1.96 * .mhse))

    ## test that combine OR = 1
    .chi <- (sum(.t[["o"]]) - sum(.t[["e"]]))^2 / sum(.t[["v"]])
    .chi <- tryCatch({
        suppressWarnings(pchisq(.chi, df = 1, lower.tail = FALSE))
    }, error = function(err) {
        return(NA)
    })

    ## test of homogeneity
    .homox <- ((.t[, "o"] * .t[, "h0"]) -
                   (.mhor * .t[, "d0"] * .t[, "h1"]))^2 /
        (.mhor * .t[, "v"] * (.t[, "n"]^2))
    .homox <- sum(.homox)
    .homo <- tryCatch({
        suppressWarnings(pchisq(.homox, df = length(.lvl) - 1, lower.tail = FALSE))
    }, error = function(err) {
        return(NA)
    })

    ## return a matrix
    .df <- rbind(
        c("M-H combined",
          sprintf(c(.mhor, .mhconfint, .chi), fmt = paste0('%#.', rnd, 'f'))),
        c("Homogeneity Test (M-H)",
          paste0("chi(", length(.lvl) - 1, ")"),
          sprintf(.homox, fmt = paste0('%#.', 2, 'f')),
          "Pr>chi2",
          sprintf(.homo, fmt = paste0('%#.', rnd, 'f')))
    )
    data.frame(.df)
}


calcMHOR <- function(.tbl)
{
    d1 <- .tbl[1, 1]
    h1 <- .tbl[1, 2]
    d0 <- .tbl[2, 1]
    h0 <- .tbl[2, 2]
    d <- d1 + d0
    h <- h1 + h0
    n1 <- d1 + h1
    n0 <- d0 + h0
    n <- sum(.tbl)

    .q <- d1 * h0 / n
    .r <- d0 * h1 / n
    .v <- (d * h * n0 * n1) / ((n)^2 * (n - 1))
    .e <- (d * n1) / n
    data.frame(q = .q, r = .r, v = .v, e = .e, o = d1,
               d0 = d0, h1 = h1, h0 = h0, n = n)
}

