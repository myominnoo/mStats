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
#' Rows and Columns can be rearranged by specifying
#' `exp_value` and `case_value`. This is used
#' when the exposed and case values are not at the right place in 2x2 tables.
#'
#' Reference row value can be specified in `exp_value`.
#'
#'
#' Attributable fractions, \code{Attr. Frac. Exp} and  \code{Attr. Frac. Pop}
#' among exposed and population are calculated when OR is greated than or
#' equal to 1.
#' If OR is less than 1, preventable fractions,  \code{Prev. Frac. Exp}
#' and  \code{Attr. Frac. Pop} are calculated.
#'
#' It produces a table with Odds Ratio, 95% CI as well as
#' p-value. If \code{strata} is specified, `Mantel-Haenzsel` Pooled
#' estimates of `Odds Ratio` is generated along with Chi-squared test for
#' homogeneity.
#'
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
        stop(paste0("`", ".data_name", "` must be a data.frame"), call. = FALSE)
    }

    ## get exp and case name
    exp <- as.character(.args$exp)
    case <- as.character(.args$case)
    strata <- as.character(.args$strata)

    ## check if these variables are in the dataset
    lapply(c(exp, case) , function(z) {
        if (!(z %in% .vars_names)) {
            stop(paste0("'", z, "' not found."), call. = FALSE)
        }
    })

    ## create single vectors for exp and case
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
        .lbl <- paste0(" Measure of Association : Odds Ratio for '", case, "'")
    } else {
        ## exp var must be binary for MHOR.
        if (nrow(.tbl) > 2) {
            stop(paste0("'", exp, "' must be binary."), call. = FALSE)
        }

        ## create single vector for strata
        .strata <- .data[[strata]]

        ## calculate MHOR
        .df <- calcMHOR(.exp, .case, .strata, exp_value, case_value, rnd)

        ## calculate crude OR
        .crude <- calcOR(table(.exp, .case, useNA = "no"), exp, rnd)[7, -2]
        .crude[1, 1] <- "Crude"

        ## add crude OR to MHOR table
        .df[nrow(.df)-3, ] <- .crude
        names(.df)[1] <- strata


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


calcOR <- function(.tbl, exp, rnd)
{
    ## create row and column totals and odds.
    .tblr <- cbind(.tbl, Total = rowSums(.tbl))
    .tblc <- rbind(.tblr, Total = colSums(.tblr))
    .tblf <- cbind(
        .tblc,
        Odds = sprintf(.tblc[, 1] / .tblc[, 2], fmt = paste0("%#.", rnd, "f"))
    )

    ## calculate odds ratio, 95% CI, and p-value
    .a <- as.numeric(.tbl[1, 1])
    .b <- as.numeric(.tbl[1, 2])
    .c <- as.numeric(.tbl[2, 1])
    .d <- as.numeric(.tbl[2, 2])
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
        .lbl <- c("Odds Ratio", "Attr. Frac. Exp", "Attr. Frac. Pop")
    } else {
        .afe <- 1 - .or
        .afe_confint <- c(1 - .confint[2], 1- .confint[1])
        .afp <- (.a / .m1 * .afe) / ((.a / .m1 * .afe) + .or)
        .lbl <- c("Odds Ratio", "Prev. Frac. Exp", "Prev. Frac. Pop")
    }

    ## combine all in one dataframe
    .df <- data.frame(row.names(.tblf), .tblf)

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
    names(.est) <- names(.df) <- c(exp, colnames(.tblf))

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


calcMHOR <- function(.exp, .case, .strata, exp_value, case_value, rnd)
{

    ## make three tables
    .tbl <- table(.exp, .case, .strata, useNA = "no")
    .lvl_len <- dim(.tbl)[3]

    ## calculate MHOR
    .t <- tryCatch({
        mantelhaen.test(.tbl, correct = FALSE)
    }, warning = function(w) {
        mantelhaen.test(.tbl, correct = FALSE, exact = TRUE)
    }, error = function(cnd) {
        return(NA)
    })
    .mhor <- c("M-H Combined",
               sprintf(c(.t$estimate, .t$conf.int, .t$p.value),
                       fmt = paste0("%#.", rnd, "f")))

    # calculate chi-square and p-value for homogeneity test
    # Woolf test of homogeneity of odds ratios (Jewell 2004, page 154).
    .t <- do.call(
        rbind,
        lapply(1:.lvl_len, function(z) {
            ## change row and col orders
            .t <- rowColOrder(.tbl[,,z], exp_value, case_value)
            .strata_name <- dimnames(.tbl)$.strata[z]
            a <- as.numeric(.t[1, 1])
            b <- as.numeric(.t[1, 2])
            c <- as.numeric(.t[2, 1])
            d <- as.numeric(.t[2, 2])

            or <- log(((a + 0.5) * (d + 0.5)) /
                          ((b + 0.5) * (c + 0.5)))
            var <- (1 / (a + 0.5)) +
                (1 / (b + 0.5)) + (1 / (c + 0.5)) + (1 / (d + 0.5))
            w <- 1 / as.numeric(var)
            wor <- w * or

            ## calculate OR
            res <- calcOR(.t, "tocombine", rnd)[7, -2]
            res[1, 1] <- .strata_name

            ## return result
            .df <- data.frame(res, or, w, wor)
            names(.df)[1:5] <- c("", "Point Estimate",
                                 "[95% Conf.", "Interval]", "Pr>chi2")
            .df
        })
    )


    lnOR <- sum(.t$wor) / sum(.t$w)

    # Equation 10.3 from Jewell (2004):
    .chi <- sum(.t$w * (.t$or - lnOR)^2)
    .pvalue <- tryCatch({
        suppressWarnings(pchisq(.chi, df = .lvl_len - 1, lower.tail = FALSE))
    }, error = function(err) {
        return(NA)
    })


    ## combine estimates
    .t <- .t[, 1:5]
    .df <- data.frame(
        rbind(
            # c("", "Point Estimate", "[95% Conf.", "Interval]", "Pr>chi2"),
            c("", "", "", "", ""),
            .mhor,
            c("M-H Homogeneity Test", paste0("chi(", .lvl_len - 1, ")"),
              sprintf(.chi, fmt = paste0('%#.', 2, 'f')), "Pr>chi2",
              sprintf(.pvalue, fmt = paste0('%#.', rnd, 'f')))
        )
    )
    names(.df) <- names(.t)
    .df <- addDashLines(.df)
    .df <- rbind(.df[-4, ], .df[4, ])

    .df <- rbind(.t, .df)
    row.names(.df) <- NULL
    return(.df)
}
