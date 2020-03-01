#' @title Calculating Risks
#'
#' @description
#' \code{tabRisks()} cross-tabulates two variables and
#' reports risks of failure \code{var_case} among exposed and
#' unexposed levels of explanatory variable \code{var_exp}.
#' It is used with cross-sectional data.
#'
#' @param data a data frame object (Optional)
#' @param var_exp Exposure variable.
#' @param var_case Case or outcome variable should be binary vector.
#' @param ref_exp Exposed level
#' @param ref_case outcome
#' @param na.rm A logical value to specify missing values, <NA> in the table
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#' @param plot logical value to display plots of rates across a categorical
#' variable
#' @param print.table logical value to display formatted outputs
#'
#' @details
#' The variable \code{var_case} typically should be coded
#' 1 for case and 0 for non-case. A tabulation with Risks and
#' corresponding 95\% Confidence Intervals (CI) is displayed.
#'
#' Risks are sometimes called proportions, incidence proportions or
#' prevalence.
#'
#' 95\% CI is calculated by large sample method.
#'
#' \strong{Calculating Risks}
#'
#' \deqn{Risks = Outcome of Interest (A) / Sample Size (n)}
#'
#' \deqn{Standard Error, SE = \sqrt(p x (1 - p) / n)}
#'
#' \deqn{95\% CI = Risks +/- (1.96 x SE)}
#'
#' @note
#'
#' This method should be avoided in small samples. Quadratic or
#' exact binomial methods are preferred in this regard.
#'
#' @references
#'
#' B. Burt Gerstman. Epidemology Kept Simple. Wiley-Blackwell, 2013:
#' Chapter 13, Confidence Intervals and p-values
#'
#' @seealso \code{\link{tabOdds}}, \code{\link{mhodds}}
#'
#' @keywords
#'
#' Risks, risk ratio, incidence, prevalence,
#'
#' frequency table, statistics, descriptive
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
#' tabRisks(simpson, treatment, outcome)
#' tabRisks(simpson, treatment, outcome, ref_exp = "standard")
#' tabRisks(simpson, treatment, outcome, ref_case = "dead")
#' tabRisks(simpson, treatment, outcome, "standard", "dead")
#'
#' ## Risks for Clinic 1
#' simpson %>%
#'     pick(clinic == 1) %>%
#'     tabRisks(treatment, outcome)
#'
#' ## Risks for Clinic 2
#' simpson %>%
#'     pick(clinic == 2) %>%
#'     tabRisks(treatment, outcome)
#'
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
#' tabRisks(asthma, sex, asthma)
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
#' tabRisks(hay, eczema, hayFever)
#' }

#' @export
tabRisks <- function(data = NULL, var_exp, var_case, ref_exp = NULL,
                     ref_case = NULL, na.rm = FALSE, rnd = 3,
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
    t <- data.frame(rbind(t, Total = colSums(t)),
                    stringsAsFactors = FALSE)

    n <- rowSums(t)
    p <- (t[, 1] / n)
    q <- 1- p
    SE <- sqrt(p * q / n)
    ll <- p - (1.96 * SE)
    ul <- p + (1.96 * SE)
    t <- cbind(t, round(cbind(Total = n, Risks = p, "[95% Conf." = ll,
        "Interval]" = ul), rnd))

    if (print.table) {
        printText(t, paste0("Risks of '", case_name, "' among '",
                            exp_name, "'"))

        if (!is.null(attr(var_case, "label")) |
            !is.null(attr(var_exp, "label"))) {
            printMsg("Labels:")
            printMsg(paste0(exp_name, ": ",
                            attr(var_exp, "label"), collapse = ""))
            printMsg(paste0(case_name, ": ",
                            attr(var_case, "label"), collapse = ""))
        }
    }

    if (plot) {
        plotRisks(t, exp_name, case_name, "Risks")
    }

    invisible(t)
}


#' @title
#' Change reference level by row or column
#'
#' @description
#' \code{tblRowColOrder()} changes the order of levels in rows or
#' columns by specifying names of levels.
#'
#' @param t table
#' @param ref_row name of row levels
#' @param ref_col name of column levels
#'
#' @details
#'
#' This function is used in calculating risks and odds.
#'
#'
#' @describeIn tabRisks
#'
#' @seealso
#'
#' \code{\link{tabRisks}}, \code{\link{tabOdds}}
#'
#' @export
tblRowColOrder <- function(t, ref_row = NULL, ref_col = NULL)
{
    if (is.table(t)) {
        t.row <- rownames(t)
        t.col <- colnames(t)
        t.dim <- names(dimnames(t))
        if (!is.null(ref_row)) {
            t <- rbind(t[t.row == ref_row, ], t[t.row != ref_row, ])
            row.names(t) <- c(t.row[t.row == ref_row],
                              t.row[t.row != ref_row])
        }
        if (!is.null(ref_col)) {
            t <- cbind(t[, t.col == ref_col], t[, t.col != ref_col])
            colnames(t) <- c(t.col[t.col == ref_col],
                             t.col[t.col != ref_col])
        }
        names(dimnames(t)) <- t.dim
    }

    return(t)
}


#' @title
#' Plots estimates and 95\% Confidence Intervals
#'
#' @description
#' \code{plotRisks()} produces a bar plot with estimates and
#' corresponding CIs.
#'
#' @param t table
#' @param exp_name name of exposure variable
#' @param case_name name of outcome variable
#' @param ylab name of y axis
#'
#' @describeIn tabRisks
#'
#' @seealso
#'
#' \code{\link{tabRisks}}, \code{\link{tabOdds}}
#'
#' @export
plotRisks <- function(t, exp_name, case_name, ylab = "Risks")
{
    exp_name <- as.character(exp_name)
    case_name <- as.character(case_name)

    risks <- t[, ylab]
    lower <- t[, "[95% Conf."]
    upper <- t[, "Interval]"]
    by <- row.names(t)
    t <- data.frame(risks, lower, upper, by)
    t <- t[order(t$by), ]

    plot(t$by, t$risks, ylim = c(0, max(t$upper)),
         main = paste0(ylab, " of '", case_name, "' among '",
                       exp_name, "'"),
         xlab = exp_name,
         ylab = ylab)
    nrow_by <- nrow(t)
    segments(1:nrow_by, t$risks, 1:nrow_by, t$upper, col = "blue", lwd = 2)
    segments(1:nrow_by, t$risks, 1:nrow_by, t$lower, col = "blue", lwd = 2)
}
