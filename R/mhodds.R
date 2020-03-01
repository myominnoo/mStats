#' @title Calculating Odds Ratios
#'
#' @description
#' \code{mhodds()} generates cross-tabulation between two variables and
#' display odds as well as Odds Ratios of failure \code{var_case}
#' against a categorical explanatory variable \code{var_exp}.
#'
#' It is used with cross-sectional as well as case-control data.
#'
#' @param data a data frame object (Optional)
#' @param var_exp Exposure variable.
#' @param var_case Case or outcome variable should be binary vector.
#' @param strata variable for stratified (Mantel-Haenszel Method)
#' analysis
#' @param ref_exp Exposed level
#' @param ref_case outcome
#' @param na.rm A logical value to specify missing values, <NA> in the table
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#' @param plot logical value to display plots of rates across a categorical
#' variable
#' @param print.table logical value to display formatted outputs
#'
#' @details
#' The variable \code{var_case} should coded 1 for case and 0 for non-case.
#' The arugment \code{exposed} can be used to set the reference value for
#' comparison of odds ratios.
#'
#' It produces a simple table with odds, Odds Ratio,95% CI as well as
#' p-value. If \code{strata} is specified, \code{Mantel-Haenzsel\'s Odds
#' Ratio} is generated along with chi-sqaured test for heterogeneity.
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
#' \strong{Mantel-Haenszel's OR}
#'
#' \deqn{ORMH = Q / R}
#'
#' \deqn{Q = \sum{(D1i x H0i) / ni}}
#'
#' \deqn{R = \sum{(D0i x H1i) / ni}}
#'
#' \strong{95\% CI of ORMH}
#'
#' \deqn{95\% CI = OR / EF or OR x EF}
#'
#' \deqn{SE(ORMH) = \sqrt{V / (Q x R)}}
#'
#' \deqn{V = \sum{(Di x Hi x n0i x n1i) / ( (ni)^2 x (ni - 1))}}
#'
#' @references
#'
#' \enumerate{
#'     \item Essential Medical Statistics, Betty R. Kirkwood & Jonathan A.C.
#' Sterne, Second Edition. Chapter 3
#'
#'     \item B. Burt Gerstman. Epidemology Kept Simple. Wiley-Blackwell, 2013:
#' Chapter 13, Confidence Intervals and p-values
#' }
#'
#'
#' @seealso
#'
#' \code{\link{stmh}}, \code{\link{tabOdds}}, \code{\link{tabRisks}},
#'
#' \code{\link{expandTables}}
#'
#' @keywords
#'
#' odds, odds ratio, frequency table, statistics, descriptive,
#' MHOR, Mantel-Haenzsel's Odds Ratio
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
#'     labelVars(c(treatment, outcome, clinic),
#'               c("Treatment: new or standard",
#'                 "Outcome: alive or dead", "clinic: 1 or 2")) %>%
#'     labelData("Example of Simpson's Paradox")
#'
#' ## checking structure
#' codebook(simpson)
#'
#' ## Calculating Odds ratio
#' mhodds(simpson, treatment, outcome)
#'
#' ## Mantel-Haenzsel's Odds Ratio
#' mhodds(simpson, treatment, outcome, clinic)
#'
#' ## Checking OR for Clinic 1
#' simpson %>%
#'     pick(clinic == 1) %>%
#'     mhodds(treatment, outcome)
#'
#' ## Checking OR for Clinic  2
#' simpson %>%
#'     pick(clinic == 2) %>%
#'     mhodds(treatment, outcome)
#'
#'
#'
#'
#' ### Example from Essential Medical Statistics
#' # Page 178, Chapter 18: Controlling for confounding: Stratification
#' library(magrittr)
#' lepto <- expandTables(
#'     male = c(36, 14, 50, 50), female = c(24, 126, 10, 90),
#'     exp_name = "area", exp_lvl = c("Rural", "Urban"),
#'     case_name = "ab", case_lvl = c("Yes", "No"),
#'     strata_name = "gender"
#' ) %>%
#'     labelData("Prevalence survey of leptospirosis in West Indies") %>%
#'     labelVars(c(area, ab, gender),
#'               c("Type of area", "Leptospirosis Antibodies",
#'                 "Gender: Male or female"))
#'
#' codebook(lepto)
#'
#' ## OR
#' mhodds(lepto, area, ab, ref_case = "Yes")
#'
#' ## MH OR
#' mhodds(lepto, area, ab, gender, ref_case = "Yes")
#'
#'
#'
#'
#' ## Asthma Example from Essential Medical Statistics
#' ## page 160
#'
#' library(magrittr)
#' asthma <- expandTables(c(81, 995, 57, 867),
#'               exp_name = "sex",
#'               exp_lvl = c("Women", "Man"),
#'               case_name = "asthma",
#'               case_lvl = c("Yes", "No")) %>%
#'           labelData("Hypothetical Data of Asthma Prevalence") %>%
#'           labelVars(c(sex, asthma),
#'               c("Man or Woman", "Asthma or No Asthma"))
#'
#' ## Checking codebook
#' codebook(asthma)
#'
#' mhodds(asthma, sex, asthma)
#'
#'
#'
#'
#'
#'
#' ## The odds ratio, J Martin Bland, Douglas G Altman, BMJ 2000;320:1468
#' hay <- expandTables(c(141, 420, 928, 13525),
#'                     exp_name = "eczema",
#'                     exp_lvl = c("Yes", "No"),
#'                     case_name = "hayFever",
#'                     case_lvl = c("Yes", "No")) %>%
#'        labelData("hay fever and eczema in 11 year old children") %>%
#'        labelVars(c(eczema, hayFever),
#'                 c("prevalence of eczema", "prevalence of hay fever"))
#'
#' codebook(hay)
#'
#' mhodds(hay, eczema, hayFever)
#' }

#' @export
mhodds <- function(data = NULL, var_exp, var_case, strata = NULL,
                   ref_exp = NULL, ref_case = NULL, na.rm = FALSE, rnd = 3,
                   plot = TRUE, print.table = TRUE)
{
    arguments <- as.list(match.call())
    if (!is.null(data)) {
        var_exp <- eval(substitute(var_exp), data)
        var_case <- eval(substitute(var_case), data)
        strata <- eval(substitute(strata), data)
    }

    if (is.null(strata)) {
        data <- structure(data, class = "bi")
    } else {
        data <- structure(data, class = "mh")
    }

    UseMethod("mhodds", data)
}

#' @rdname mhodds
#' @export
mhodds.default <- function(data = NULL, var_exp, var_case, strata = NULL,
                           ref_exp = NULL, ref_case = NULL, na.rm = FALSE, rnd = 3,
                           plot = TRUE, print.table = TRUE)
{
    stop(" >>> Data Type is not supported. <<< ")
}

#' @rdname mhodds
#' @export
mhodds.bi <- function(data = NULL, var_exp, var_case, strata = NULL,
                      ref_exp = NULL, ref_case = NULL, na.rm = FALSE, rnd = 3,
                      plot = TRUE, print.table = TRUE)
{
    arguments <- as.list(match.call())
    if (!is.null(data)) {
        var_exp <- eval(substitute(var_exp), data)
        var_case <- eval(substitute(var_case), data)
    }

    exp_name <- arguments$var_exp
    case_name <- arguments$var_case

    na.rm <- ifelse(na.rm, "no", "ifany")

    t <- table(var_exp, var_case, useNA = na.rm)

    # change table order by reference level in exp and case
    t <- tblRowColOrder(t, ref_exp, ref_case)

    t.nrow <- nrow(t)
    ## to change the display of table more than one <---
    if (t.nrow > 2) {
        t <- splitTables(t, ref_exp)
        t <- lapply(t, function(z) {
            if (!is.null(z)) {
                calc_OR(z, ref_exp, rnd)
            }
        })

    } else {
        t <- calc_OR(t, ref_exp, rnd)
    }

    if (print.table) {

        if (t.nrow > 2) {
            lapply(t, function(z) {
                lvl <- row.names(z)
                if (!is.null(z)) {
                    printText(
                        z,
                        paste0("Maximum likelihood estimates of Odds Ratios\n",
                               "Comparing odds of '", case_name, "' among '",
                               exp_name, "'"),
                        split = "Comparing "
                    )
                    printMsg("Note:")
                    printMsg(paste0(
                        "Comparing '", lvl[2], "' Versus '", lvl[1], "'"))
                }
            })
        } else {
            printText(
                t,
                paste0("Maximum likelihood estimates of Odds Ratios\n",
                       "Comparing odds of '", case_name, "' among '",
                       exp_name, "'"),
                split = "Comparing "
            )
        }

        if (!is.null(attr(var_case, "label")) |
            !is.null(attr(var_exp, "label"))) {
            printMsg("Labels:")
            printMsg(paste0(exp_name, ": ",
                            attr(var_exp, "label"), collapse = ""))
            printMsg(paste0(case_name, ": ",
                            attr(var_case, "label"), collapse = ""))
        }
    }

    invisible(t)
}

#' @rdname mhodds
#' @export
mhodds.mh <- function(data = NULL, var_exp, var_case, strata,
                      ref_exp = NULL, ref_case = NULL, na.rm = FALSE, rnd = 3,
                      plot = TRUE, print.table = TRUE)
{
    arguments <- as.list(match.call())
    if (!is.null(data)) {
        var_exp <- eval(substitute(var_exp), data)
        var_case <- eval(substitute(var_case), data)
        strata <- eval(substitute(strata), data)
    }

    exp_name <- as.character(arguments$var_exp)
    case_name <- as.character(arguments$var_case)
    strata_name <- as.character(arguments$strata)

    strata_lvl <- as.character(unique(strata))
    if (is.null(ref_exp)) {
        ref_exp <- as.character(unique(var_exp)[1])
    }

    t <- table(var_exp, var_case)
    t.nrow <- nrow(t)
    ## to change the display of table more than one <---
    if (t.nrow > 2) {
        stop(paste(" >>> Exposure levels more than 2 are not ",
                   "supported yet. <<< "))
    } else {
        f <- calc_mhor(data, strata, strata_lvl, exp_name, case_name,
                       ref_exp, ref_case, na.rm, rnd)
    }

    hetero <- f[[2]]
    f <- f[[1]]

    if (print.table) {
        printText(
            f,
            paste0("Mantel-Haenzsel estimate of Odds Ratios of '",
                   exp_name, "' ~ '", case_name, "'",
                   "\nStratified by '", strata_name, "'",
                   "\nReference: '", exp_name, "' == '", ref_exp, "'"),
            split = "Stratified by ")

        printMsg(paste0("Chi-squared test of heterogeneity [P>Chi2]: ",
                        sprintf(hetero, fmt = '%#.5f')))

        if (!is.null(attr(var_case, "label")) |
            !is.null(attr(var_exp, "label")) |
            !is.null(attr(strata, "label"))) {
            printMsg("Labels:")
            printMsg(paste0(exp_name, ": ",
                            attr(var_exp, "label"), collapse = ""))
            printMsg(paste0(case_name, ": ",
                            attr(var_case, "label"), collapse = ""))
            printMsg(paste0(strata_name, ": ",
                            attr(strata, "label"), collapse = ""))
        }
    }


    if (plot) {
        colNames <- c("Odds Ratio", "[95% Conf.", "Interval]")
        t <- f[-(nrow(f)-1), ]
        t.row <- t[, "[Strata]"]
        t <- t[, colNames]
        t <- do.call(
            cbind,
            lapply(t, function(z) {
                as.numeric(z)
            })
        )
        row.names(t) <- t.row
        plotRisks(t, exp_name, case_name, "Odds Ratio")
        abline(h = t["MH's OR", "Odds Ratio"], col = "red")
    }
    invisible(f)

}

#' @param t 2x2 table input to calaculate odds ratio of non-reference
#' @param ref_lvl logical vector to indicate where reference category is
#' @rdname mhodds
#' @export
calc_OR <- function(t, ref_lvl, rnd)
{
    t.row <- row.names(t)
    ref_lvl <- ifelse(is.null(ref_lvl), t.row[1], ref_lvl)
    ref_lvl <- t.row %in% ref_lvl
    d1 <- as.numeric(as.character(t[ref_lvl, 1]))
    d0 <- as.numeric(as.character(t[!ref_lvl, 1]))
    h1 <- as.numeric(as.character(t[ref_lvl, 2]))
    h0 <- as.numeric(as.character(t[!ref_lvl, 2]))
    SE <- sqrt((1/d1) + (1/h1) + (1/d0) + (1/h0))
    EF <- exp(1.96 * SE)
    OR <- ((d1 * h0) / (d0 * h1))
    lower <- OR / EF
    upper <- OR * EF

    chi <- tryCatch({
        suppressWarnings(chisq.test(t, correct = FALSE)$p.value)
    }, error = function(err) {
        return(NA)
    })

    f <- data.frame(t[ref_lvl, 1], t[ref_lvl, 2],
                    sprintf(OR, fmt = paste0('%#.', rnd, 'f')),
                    sprintf(lower, fmt = paste0('%#.', rnd, 'f')),
                    sprintf(upper, fmt = paste0('%#.', rnd, 'f')),
                    sprintf(chi, fmt = '%#.5f'),
                    stringsAsFactors = FALSE)
    names(f) <- c(colnames(t), "Odds Ratio", "[95% Conf.", "Interval]",
                  "Pr(>|chi2|)")
    row.names(f) <- t.row[ref_lvl]

    v <- data.frame(t[!ref_lvl, 1], t[!ref_lvl, 2],
                    "Reference", "", "", "", stringsAsFactors = FALSE)
    names(v) <- names(f)
    row.names(v) <- t.row[!ref_lvl]

    f <- rbind(f, v)

    return(f)
}


#' @param t 2x2 table input to calaculate odds ratio of non-reference
#' @param strata_lvl levels in strata variable
#' @rdname mhodds
#' @export
calc_RawMHOR <- function(t, ref_exp, strata_lvl)
{
    d1i <- as.numeric(as.character(t[1, 1]))
    h1i <- as.numeric(as.character(t[1, 2]))
    d0i <- as.numeric(as.character(t[2, 1]))
    h0i <- as.numeric(as.character(t[2, 2]))
    ni <- sum(d1i, h1i, d0i, h0i)
    di <- sum(d1i, d0i)
    hi <- sum(h1i, h0i)
    n1i <- sum(d0i, h0i)
    n0i <- sum(d1i, h1i)

    Q <- d1i * h0i / ni
    R <- d0i * h1i / ni
    V <- (di * hi * n0i * n1i) / ((ni ^ 2) * (ni - 1))
    E <- (di * n1i) / ni
    O <- d1i
    t <- cbind(t[ref_exp, ], Q, R, V, E, O,
               d1i, h0i, d0i, h1i, ni)
    row.names(t) <- strata_lvl

    return(t)
}

#' @param strata_lvl levels in strata variable
#' @param exp_name Name of \code{Exposure} Variable
#' Exposed and non-exposed
#' @param case_name Name of \code{Case} variable
#' @rdname mhodds
#' @export
calc_mhor <- function(data, strata, strata_lvl, exp_name, case_name,
                      ref_exp, ref_case, na.rm, rnd)
{
    f <- do.call(
        rbind,
        lapply(strata_lvl, function(z) {
            d <- data[with(data, strata == z), ]
            e <- d[, exp_name]
            c <- d[, case_name]
            t <- mhodds.bi(
                data = NULL, var_exp = e, var_case = c,
                strata = NULL, ref_exp = ref_exp, ref_case = ref_case,
                na.rm = na.rm, rnd = rnd,
                print.table = FALSE
            )
            t.row <- row.names(t)
            t.row <- paste0(t.row[t.row == ref_exp], "[", z, "]")
            t <- calc_RawMHOR(t, ref_exp, t.row)
            t
        })
    )

    Q <- sum(f[, "Q"])
    R <- sum(f[, "R"])
    V <- sum(f[, "V"])
    E <- sum(f[, "E"])
    O <- sum(f[, "O"])
    mhor <- Q / R
    mhse <- sqrt(V / (Q * R))
    mhef <- exp(1.96 * mhse)
    chi <- (O - E) ^ 2 / V
    chi <- pchisq(chi, df = 1, lower.tail = FALSE)

    ## Calculations for the Chi-squared test of heterogeneity
    hetero <- ((f[, "d1i"] * f[, "h0i"]) -
                   (mhor * f[, "d0i"] * f[, "h1i"]))^2 /
        (mhor * f[, "V"] * (f[, "ni"]) ^ 2)
    hetero <- sum(hetero)
    hetero <- pchisq(hetero, df = 1, lower.tail = FALSE)

    mhor <- data.frame(cbind(
        "MH's OR", ">>>", ">>>",
        sprintf(mhor, fmt = paste0('%#.', rnd, 'f')),
        sprintf(mhor / mhef, fmt = paste0('%#.', rnd, 'f')),
        sprintf(mhor * mhef, fmt = paste0('%#.', rnd, 'f')),
        sprintf(chi, fmt = '%#.5f')
    ))

    f <- f[, 1:6]

    rowNames <- row.names(f)
    colNames <- names(f)
    f <- data.frame(rowNames, f, stringsAsFactors = FALSE)

    v <- generateLinesDF(f)
    f <- rbind(f, v)
    row.names(f) <- NULL
    names(f) <- c("[Strata]", colNames)
    names(mhor) <- names(f)
    f <- rbind(f, mhor)

    return(list(f, hetero))
}


#' @rdname mhodds
#' @export
splitTables <- function(t, ref_exp = NULL)
{
    t.row <- row.names(t)
    ref_exp <- ifelse(is.null(ref_exp), t.row[1], ref_exp)
    t <- lapply(t.row, function(z) {
        if (z != ref_exp) {
            t <- rbind(t[ref_exp, ], t[z, ])
            row.names(t) <- c(t.row[t.row == ref_exp],
                              t.row[t.row == z])
            t
        }
    })

    return(t)
}
