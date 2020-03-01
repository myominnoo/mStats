#' @title Calculating Odds
#'
#' @description
#' \code{tabOdds} generates cross-tabulation between two variables and
#' display odds of failure \code{var_case} against a categorical
#' explanatory variable \code{var_exp}. It is used with cross-sectional
#' data.
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
#' The variable \code{var_case} should coded 1 for case and 0 for non-case.
#' A simple table illustrating cases and controls as well as odds for each
#' category is generated.
#'
#' \strong{Calculating Odds}
#'
#' \deqn{OR = d1 x h0 / d0 x h1}
#'
#' \strong{Error Factor} (EF)
#'
#' \deqn{EF = exp(1.96 x SE(log odds))}
#'
#' \deqn{SE(log odds) = \sqrt{1/d + 1/h}}
#'
#' @references
#'
#' \enumerate{
#'     \item Essential Medical Statistics, Betty R. Kirwood, Second
#' Edition
#'
#'     \item Statistics Notes: The odds ratio; J Martin Bland, Douglas G Altman
#' BMJ 2000;320:1468
#' }
#'
#' @seealso
#'
#' \code{\link{mhodds}}, \code{\link{tabRisks}}
#'
#' @keywords
#'
#' odds, odds ratio, frequency table, statistics, descriptive
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
#' tabOdds(simpson, treatment, outcome)
#' tabOdds(simpson, treatment, outcome, ref_exp = "standard")
#' tabOdds(simpson, treatment, outcome, ref_case = "dead")
#' tabOdds(simpson, treatment, outcome, "standard", "dead")
#'
#' ## Odds for Clinic 1
#' simpson %>%
#'     pick(clinic == 1) %>%
#'     tabOdds(treatment, outcome)
#'
#' ## Odds for Clinic 2
#' simpson %>%
#'     pick(clinic == 2) %>%
#'     tabOdds(treatment, outcome)
#'
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
#' tabOdds(asthma, sex, asthma)
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
#' tabOdds(hay, eczema, hayFever)
#' }

#' @export
tabOdds <- function(data = NULL, var_exp, var_case, ref_exp = NULL,
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

    odds <- t[, 1] / t[, 2]
    SE <- sqrt((1/sum(t[, 1])) + (1/sum(t[, 2])))
    EF <- exp(1.96 * SE)
    lower <- odds / EF
    upper <- odds * EF
    t <- cbind(t, round(cbind(Odds = odds, "[95% Conf." = lower,
                              "Interval]" = upper), rnd))

    if (print.table) {
        printText(t, paste0("Odds of '", case_name, "' among '",
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
        plotRisks(t, exp_name, case_name, "Odds")
    }

    invisible(t)
}
