#' @title Calculating measures of Risk including Risk Ratio
#'
#' @description
#' \code{mhrr()} calculates different measures of risk including risk
#' ratios (RR) as well as
#' Mantel-Haenszel pooled estimates.
#'
#' @param data Dataset
#' @param ... Variable or multiple variables
#' Colon separator \code{:} can be used to specify multiple variables.
#' @param by Varaiable for cross-tabulation
#' @param strata Variable for stratification
#' @param exp_value value for exposure as reference
#' @param case_value value for outcome as reference
#' @param measure choose different measures related to risks. This is not
#' case-sensitive.
#'
#' By default, measure is `RATIO` and calculates `rate ratio`.
#'
#' For Risk Ratio;
#'
#' `ratio` = `Risk Ratio`
#'
#' `diff` = `Risk Diff.` (`Risk Difference`)
#'
#' `ar` = `Attributable Risk`, specifically `Attributable Risk Percent` or
#' `Proportional Attributable Risk` (This is not converted to Percent scale)
#'
#' `par` = `Pop. AR` (`Population Attributable Risk`)
#'
#' For Mantel-Haenszel estimates,
#'
#' `ratio` = `Risk Ratio`
#'
#' `diff` = `Risk Diff.` (`Risk Difference`)
#'
#' @param plot logical value to display plots of risk ratios or differences
#' including MH estimates across a categorical variable
#' @param na.rm A logical value to specify missing values, `NA` in the table
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#'
#' @details
#'
#' Value can be set as baseline by specifying `exp_value`. This is used
#' when the exposed and case values are not in the right place.
#'
#' It produces a table with Odds Ratio,95% CI as well as
#' p-value. If \code{strata} is specified, `Mantel-Haenzsel` Pooled
#' estimates of `Odds Ratio` is generated along with Chi-sqaured test for heterogeneity.
#'
#' The following entails formulas used for calculating measures.
#'
#' \deqn{Risk among exposed, R1 = A / (A + C)}
#'
#' \deqn{Risk among unexposed, R0 = B / (B + D)}
#'
#' \deqn{Number among exposed, N1 = A + B}
#'
#' \deqn{Number among unexposed, N0 = C + D}
#'
#' \deqn{Number among diseased, D1 = A + C}
#'
#' \deqn{Number among health, D0 = B + D}
#'
#' \deqn{Sample size, N = N1 + N0}
#'
#'
#'
#'
#' \strong{Risk Ratio, RR}
#'
#' Risk ratio is sometimes called as relative risk (RR).
#'
#' \deqn{Risk Ratio = R1 / R0}
#'
#'
#' ## using delta method, See page 155 of Essential Medical Statistics
#' \deqn{SE(log RR) = sqrt(1/A - 1/N1 + 1/C - 1/N0)}
#'
#' \deqn{Lower Limit of CI = exp(log RR - 1.96 x SE(log RR)) }
#'
#' \deqn{Upper Limit of CI = exp(log RR + 1.96 x SE(log RR)) }
#'
#' ## Test of null hypothesis for Risk Ratio
#'
#' \deqn{z = log RR / SE(log RR)}
#'
#'
#' ## Mantel-Haenszel Method for RR
#'
#' k = Strata
#'
#' \deqn{φˆMH = \sum{Ak x N0k / Nk} / \sum{Ck x N1k / Nk}}
#'
#' \deqn{SE(log φˆMH) = \sqrt{ \sum{D1k x N1k x N0k / Nk^2 - Ak x Ck / Nk} / \sum{Ak x N0k / Nk} x \sum{Ck x N1k / Nk}}}
#'
#'
#' \deqn{Lower Limit of CI = exp(log φˆMH - 1.96 x SE(log φˆMH)) }
#'
#' \deqn{Upper Limit of CI = exp(log φˆMH + 1.96 x SE(log φˆMH)) }
#'
#'
#' ## Mantel–Haenszel test statistic
#'
#' A test of association (H0: φMH = 1 cohort studies; H0: ψMH = 1 case–control studies)
#' is carried out with the Mantel–Haenszel test statistic using chi-squared distribution:
#'
#' \deqn{χ2MH = (\sum{Ak} - \sum{N1k x M1k / Nk})^2 / \sum{N1k x N0k x M1k x M0k / Nk^2 x (Nk - 1)}}
#'
#' Degree of freedom is 1.
#'
#'
#'
#'
#'
#'
#' \strong{Risk Difference, RD}
#'
#' \deqn{RD = R1 - R0}
#'
#' \deqn{SE(RD) = \sqrt{(R1 (1 - R1) / N1) + R0 (1 - R0) / N0}}
#'
#' \deqn{Lower Limit of CI = RD - (1.96 x SE(RD))}
#'
#' \deqn{Upper Limit of CI = RD + (1.96 x SE(RD))}
#'
#' ## Test that the difference between two proportions is zero
#'
#' \deqn{z = RD / SE(RD)}
#'
#' ## Mantel-Haenszel Method for Risk Difference
#'
#' This method was proposed by Cochran and by Mantel and Haenszel.
#' Cochran proposed using the weights nkmk/Nk,
#' which he showed empirically to be optimal in testing a hypothesis of zero
#' risk difference if the risk differences were constant on a logit scale. These
#' weights will be called the Cochran-Mantel-Haenszel (CMH) weights and the
#' estimator based on these weights will be called the CMH estimator.
#' See details at Thomas W. O'Gorman (1994) doi.org/10.1016/0197-2456(94)90017-5
#'
#' k = strata
#'
#' \deqn{Weight of CMH estimator, W = N1k x N2k / Nk}
#'
#' \deqn{RD_CMH = \sum{Wk x RDk} / \sum{Wk}}
#'
#' \deqn{Variance of RD_CMH, Lk = (Ak x Bk x N0^3 + Ck x Dk x N1^3) / N1k x N0k x Nk^2}
#'
#' \deqn{Lower limit of CI = RD_CMH - (1.96 x \sum{Lk}^1/2 / \sum(Wk))}
#'
#' \deqn{Lower limit of CI = RD_CMH + (1.96 x \sum{Lk}^1/2 / \sum(Wk))}
#'
#'
#' ## Mantel–Haenszel test statistic: Same as Risk Ratio
#'
#' A test of association (H0: φMH = 1 cohort studies; H0: ψMH = 1 case–control studies)
#' is carried out with the Mantel–Haenszel test statistic using chi-squared distribution:
#'
#' \deqn{χ2MH = (\sum{Ak} - \sum{N1k x M1k / Nk})^2 / \sum{N1k x N0k x M1k x M0k / Nk^2 x (Nk - 1)}}
#'
#' Degree of freedom is 1.
#'
#'
#'
#' \strong{Population Attributable Risk, PAR}
#'
#'
#' A measure of the proportion of individuals in the total population with
#' the disease attributed to exposure to the risk factor is given by the
#' attributable risk (AR). P is the prevalence of the risk factor in the population
#' and RR is the relative risk for disease associated with the risk factor.
#'
#' \deqn{PAR = P (RR - 1) / (1 + P (RR - 1))}
#'
#' \deqn{Lower limit of CI = P (RR_LL - 1) / (1 + P (RR_LL - 1))}
#'
#' \deqn{Upper limit of CI = P (RR_UL - 1) / (1 + P (RR_UL - 1))}
#'
#' RR_LL = Lower limit of CI of RR
#'
#' RR_UL = Upper limit of CI of RR
#'
#'
#'
#'
#'
#' \strong{Attributable Risk, AR}
#'
#' The function produces Attributable Risk percent (Raw form = not converted to 100%).
#'
#' \deqn{AR = RR - 1 / RR}
#'
#' \deqn{SE(AR) = \sqrt{D1 / N (1 - D1 / N) (1/N1 + 1/N0)}}
#'
#'
#'
#' \strong{Efficacy}
#'
#' The efficacy of a treatment or intervention is measured by the proportion of cases
#' that it prevents. Efficacy is directly calculated from the risk ratio
#' comparing disease outcome in the treated versus control group. For a successful
#' treatment (or intervention) this ratio will be less than 1.
#'
#'
#' \deqn{Efficacy = 1 - RR}
#'
#' \deqn{Lower limit of CI = 1- RR x exp(1.96 x SE(log RR))}
#'
#' \deqn{Lower limit of CI = 1- RR / exp(1.96 x SE(log RR))}
#'
#'
#' @concept
#'
#' risk ratio relative risk risk difference, attributable risk population attributable
#'
#' Efficacy mantel-haenszel cochran CMH pooled estimates
#'
#' AR PAR RR
#'
#' @references
#'
#' \enumerate{
#'     \item Essential Medical Statistics, Betty R. Kirkwood & Jonathan A.C.
#' Sterne, Second Edition.
#'     \item B. Burt Gerstman. Epidemology Kept Simple. Wiley-Blackwell, 2013:
#'     \item Statistics with confidence, Douglas G Altman, second edition
#'     \item Thomas W. O'Gorman (1994) doi.org/10.1016/0197-2456(94)90017-5
#' }
#'
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
#' \dontrun{
#'
#'
#' ### Demonstration: Calculating Risk Ratios
#'
#'
#' ## Essential Medical Statistics, Betty R. Kirkwood, Second Edition
#' ## Chapter 16, Table 16.4, Page 154
#' ## For Risk Ratio
#' lung <- expandTables(
#'     c(39, 29961, 6, 59994),
#'     exp_name = "smoking",
#'     exp_lvl = c("Smokers", "Non-smokers"),
#'     case_name = "cancer",
#'     case_lvl = c("Yes", "No")
#' ) %>%
#'     labelVar(c(smoking, cancer),
#'              c("Smoking versus non-smoking", "Lung cancer: Yes or No")) %>%
#'     labelData("Association between smoking and lung cancer (Follow up one year)")
#'
#' codebook(lung)
#'
#' ## tabulate Risks
#' tabRisks(lung, smoking, by = cancer, exp_value = "Smokers", case_value = "Yes")
#'
#' ## calculate RR
#' mhrr(lung, smoking, by = cancer, exp_value = "Smokers", case_value = "Yes")
#'
#' ## calculate risk difference
#' mhrr(lung, smoking, by = cancer, exp_value = "Smokers", case_value = "Yes",
#'      measure = "diff", rnd = 5)
#' ## Risk difference = 0.0012 = 0.12 %
#'
#'
#'
#'
#'
#' ### Simpson's Paradox: Example from Burt Gerstman's Epidemiology
#' ### Chapter 14, Page 326
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
#' ## tabulate risks
#' tabRisks(simpson, treatment, by = outcome)
#'
#' ## calculate RR
#' mhrr(simpson, treatment, by = outcome)
#'
#' ## calculate MHRR
#' mhrr(simpson, treatment, by = outcome, strata = clinic)
#'
#'
#'
#'
#'
#' ## Demonstration: Calculating Risk Differences
#'
#'
#' ## Essential Medical Statistics, Betty R. Kirkwood, Second Edition
#' ## Chapter 16, Table 16.2, Page 150
#' ## For Risk Difference
#' flu <- expandTables(
#'     c(20, 220, 80, 140),
#'     exp_name = "treatment",
#'     exp_lvl = c("Vaccine", "Placebo"),
#'     case_name = "influenza",
#'     case_lvl = c("Yes", "No")
#' ) %>%
#'     labelVar(c(treatment, influenza),
#'              c("Vaccine or Placebo", "Influenza: Yes or No")) %>%
#'     labelData("Results from an influenza vaccine trial")
#'
#' codebook(flu)
#' tabRisks(flu, treatment, by = influenza,
#'          exp_value = "Vaccine", case_value = "Yes")
#'
#' ## Calculate RD
#' mhrr(flu, treatment, by = influenza,
#'          exp_value = "Vaccine", case_value = "Yes", measure = "diff")
#'
#'
#'
#'
#'
#'
#' ## Example from StatsDirect
#' ## Risk difference
#' ## https://www.statsdirect.com/help/meta_analysis/risk_difference.htm
#' aspirin <- expandTables(
#'     "MRC-1" = c(615, 49, 624, 67),
#'     "CDP" = c(758, 44, 771, 64),
#'     "MRC-2" = c(832, 102, 850, 126),
#'     "GASP" = c(317, 32, 309, 38),
#'     "PARIS" = c(810, 85, 406, 52),
#'     "AMIS" = c(2267, 246, 2257, 219),
#'     exp_name = "treatment",
#'     exp_lvl = c("Aspirin", "Placebo"),
#'     case_name = "outcome",
#'     case_lvl = c("patient", "death"),
#'     strata_name = "trials"
#' ) %>%
#'     labelVar(c(treatment, outcome, trials),
#'              c("Aspirin versus Placebo", "Preventing deaths", "Trials")) %>%
#'     labelData(paste0("placebo-controlled randomized trials of the effect of ",
#'                      "aspirin in preventing death after myocardial infarction"))
#'
#' codebook(aspirin)
#'
#' ## Calculate MH Risk Difference
#' mhrr(aspirin, treatment, by = outcome, strata = trials,
#'      exp_value = "Aspirin", case_value = "patient", measure = "diff")
#'
#'
#'
#'
#'
#'
#' ## Demonstration: Population Attributable Risks
#'
#' ## Statistics with Confidence, Douglas G. Altman, Second Edition
#' ## Chapter 7, Table 7.2, Page 59
#' ## For Attributable risks
#' ulcers <- expandTables(c(6, 16, 112, 729),
#'                        exp_name = "history",
#'                        exp_lvl = c("Yes", "No"),
#'                        case_name = "pylori",
#'                        case_lvl = c("Yes", "No")) %>%
#'     labelVar(c(history, pylori),
#'              c("Infected with H. pylori", "History of Ulcer in mother")) %>%
#'     labelData(paste0("Prevalence of H. pylori infection in preschool children",
#'                      " according to mother's history of ulcer"))
#'
#'
#' ## codebook
#' codebook(ulcers)
#'
#' mhrr(ulcers, history, by = pylori, exp_value = "Yes", case_value = "Yes",
#'      measure = "par")
#'
#' ## Interpretation: The proportion of children with Helicobacter pylori infection due
#' # to history of ulcer in the mother is estimated
#' # at 26 per 1000 with a wide 95% confidence interval ranging from 0.3
#' # per 1000 to 74 per 1000.
#'
#'
#'
#'
#'
#' ## Demonstration: Attributable Risk
#'
#' ## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
#' cancer <- expandTables(c(19, 139, 53, 789),
#'              exp_name = "delivery",
#'              exp_lvl = c("Delivery25", "Delivery25+"),
#'              case_name = "bc",
#'              case_lvl = c("Yes", "No")) %>%
#'     labelVar(c(delivery, bc),
#'              c("Gave birth at or after 25 years", "Breast Cancer")) %>%
#'     labelData(paste0("Example: https://www2.ccrb.cuhk.edu.hk/stat/confidence%",
#'                      "20interval/CI%20for%20relative%20risk.htm"))
#'
#' codebook(cancer)
#'
#' ## Risk Ratio
#' mhrr(cancer, delivery, by = bc, case_value = "Yes")
#'
#' ## Attributable Risk (Same as Risk difference)
#' mhrr(cancer, delivery, by = bc, case_value = "Yes", measure = "diff")
#'
#' ## Attributable Risk Percent
#' mhrr(cancer, delivery, by = bc, case_value = "Yes", measure = "ar")
#'
#' ## interpretation: AR = 0.477 = 0.477 * 100 = 47.7%
#'
#'
#'
#'
#'
#' ## Demonstration: Calculating Efficacy
#'
#'
#'
#' ## Essential Medical Statistics, Betty R. Kirkwood, Second Edition
#' ## Chapter 16, Table 16.2, Page 150
#' ## For Risk Difference
#' flu <- expandTables(
#'     c(20, 220, 80, 140),
#'     exp_name = "treatment",
#'     exp_lvl = c("Vaccine", "Placebo"),
#'     case_name = "influenza",
#'     case_lvl = c("Yes", "No")
#' ) %>%
#'     labelVar(c(treatment, influenza),
#'              c("Vaccine or Placebo", "Influenza: Yes or No")) %>%
#'     labelData("Results from an influenza vaccine trial")
#'
#' codebook(flu)
#' mhrr(flu, treatment, by = influenza,
#'          exp_value = "Vaccine", case_value = "Yes", measure = "efficacy")
#'
#' ## Efficacy = 0.771 = 0.771 x 100% = 77.1% (64.1% to 85.5%)
#'
#'
#'
#'
#'
#' ### Calculating MHOR and MHRR
#'
#' ## The Cochran-Mantel-Haenszel Method
#' ## http://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704-ep713_confounding-em/
#' ## BS704-EP713_Confounding-EM7.html#headingtaglink_3
#'
#' cvd <- expandTables(
#'     "age49" = c(10, 90, 35, 465),
#'     "age50+" = c(36, 164, 25, 175),
#'     exp_name = "obesity",
#'     exp_lvl = c("obese", "non-obese"),
#'     case_name = "cvd",
#'     case_lvl = c("Yes", "No"),
#'     strata_name = "age"
#' ) %>%
#'     labelVar(c(obesity, cvd, age),
#'              c("Obesity", "Cardiovascular disease", "Age group: <50 versus >= 50")) %>%
#'     labelData("Obesity and Incident Cardiovascular Disease by Age Group")
#'
#' codebook(cvd)
#' mhor(cvd, obesity, by = cvd, strata = age, exp_value = "obese", case_value = "Yes")
#' mhrr(cvd, obesity, by = cvd, strata = age, exp_value = "obese", case_value = "Yes")
#'
#'
#'
#' ## Tripepi G. (2010)
#' ## https://www.karger.com/Article/Fulltext/319590
#' pneumo <- expandTables(
#'     "age61" = c(7, 25, 17, 67),
#'     "age62+" = c(31, 26, 23, 31),
#'     exp_name = "sero",
#'     exp_lvl = c("Positive", "Negative"),
#'     case_name = "outcome",
#'     case_lvl = c("deaths", "survivors"),
#'     strata_name = "age"
#' ) %>%
#'     labelVar(c(sero, outcome, age),
#'              c("C. pneumoniae infection: serology test", "Outcome: death or survived",
#'                "Age categories: <62 or >= 62")) %>%
#'     labelData(paste0("Number of deaths/survivors in the study population according to",
#'                      " both C. pneumoniae infection and age categories"))
#'
#' codebook(pneumo)
#' mhrr(pneumo, sero, by = outcome, strata = age, exp_value = "Positive")
#' mhor(pneumo, sero, by = outcome, strata = age, exp_value = "Positive")
#'
#'
#'
#' ## Tripepi G. (2010)
#' ## https://www.karger.com/Article/Fulltext/319590
#' lung <- expandTables(
#'     "non-smokers" = c(53, 15, 10, 3),
#'     "smokers" = c(35, 53, 52, 79),
#'     exp_name = "alcohol",
#'     exp_lvl = c("abuse", "no-abuse"),
#'     case_name = "cancer",
#'     case_lvl = c("Yes", "No"),
#'     strata_name = "smoking"
#' ) %>%
#'     labelVar(c(alcohol, cancer, smoking),
#'              c("Alcohol abuse", "Lung cancer", "Smoking status")) %>%
#'     labelData("Tripepi G. (2010) https://www.karger.com/Article/Fulltext/319590")
#'
#' codebook(lung)
#' mhor(lung, alcohol, by = cancer, strata = smoking, case_value = "Yes")
#' mhrr(lung, alcohol, by = cancer, strata = smoking, case_value = "Yes")
#'
#'
#'
#' }
#'
#' @export
mhrr <- function(data, ... , by, strata = NULL, exp_value = NULL, case_value = NULL,
                 measure = "ratio", plot = TRUE, na.rm = FALSE, rnd = 3)
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
                                          "case_value", "measure", "plot",
                                          "na.rm", "rnd")))

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
            tabRR(.data, z, by, exp_value, case_value, measure, na.rm, rnd)
        })
        .df <- do.call(
            rbind,
            lapply(.tbl, function(z) z)
        )
        .txt <- switch (toupper(measure),
                        RATIO = "Risk Ratio",
                        DIFF = "Risk Difference",
                        AR = "Attributable Risk (AR)",
                        PAR = "Population Attributable Risk (Pop. AR)",
                        EFFICACY = "Efficacy")
        .txt <- paste0(.txt, " Estimates of '", .args$by, "'")
    } else {
        .tbl <- lapply(.vars, function(z) {
            tabMHRR(.data, z, by, strata, exp_value, case_value, measure,
                    plot, na.rm, rnd)
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

    invisible(.df)
}



# Helpers -----------------------------------------------------------------


tabRR <- function(.data, .x, .by, exp_value = NULL, case_value = NULL,
                  measure = "diff", na.rm = FALSE, rnd = 1)
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


    ## split tables to get 2x2 tables
    if (nrow(.tbl) > 2) {
        .tbl <- splitTables(.tbl, exp_value)
    } else {
        .tbl <- list(.tbl)
    }



    ## calcalate OR

    ## if measure is ratio, calculate risk ratio
    ## if diff, then risk difference
    ## if AR, then attributable risk
    ## if PAR, then population attributable risk
    ## if efficacy, then Efficacy
    .tbl <- switch (toupper(measure),
                    RATIO = lapply(.tbl, function(z) {
                        calcRR(z, .x.name, rnd)
                    }),
                    DIFF = lapply(.tbl, function(z) {
                        calcRD(z, .x.name, rnd)
                    }),
                    AR = lapply(.tbl, function(z) {
                        calcAR(z, .x.name, rnd)
                    }),
                    PAR = lapply(.tbl, function(z) {
                        calcPAR(z, .x.name, rnd)
                    }),
                    EFFICACY = lapply(.tbl, function(z) {
                        calcEff(z, .x.name, rnd)
                    })
    )


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

## Calculate Risk Ratio
## Essential Medical Statistics Page 154 - 158
calcRR <- function(.tbl, .x.name, rnd = 3)
{
    .a <- .tbl[1, 1]
    .b <- .tbl[1, 2]
    .c <- .tbl[2, 1]
    .d <- .tbl[2, 2]

    .n1 <- (.a + .b)
    .p1 <- .a / .n1
    .n0 <- (.c + .d)
    .p0 <- .c / .n0

    .rr <- .p1 / .p0
    ## use delta method
    .se <- sqrt((1/.a) - (1/.n1) + (1/.c) - (1/.n0))
    .ll <- .rr / exp(1.96 * .se)
    .ul <- .rr * exp(1.96 * .se)

    ## Test of null hypothesis
    .z <- log(.rr) / .se
    .pvalue <- tryCatch({
        suppressWarnings(2*pnorm(-abs(.z)))
    }, error = function(err) {
        return(NA)
    })


    ## create data frame
    .df <- data.frame(
        cbind(rownames(.tbl), "|",
              .tbl,
              # rbind(sprintf(.p1, fmt = paste0('%#.', rnd, 'f')),
              #       sprintf(.p0, fmt = paste0('%#.', rnd, 'f'))),
              rbind(c(sprintf(.rr, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.ll, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.ul, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.pvalue, fmt = paste0('%#.', rnd, 'f'))),
                    c("", "", "", ""))
        ), stringsAsFactors = FALSE
    )
    names(.df) <- c(.x.name, "|", colnames(.tbl),
                    # "Risks",
                    "Risk Ratio", "[95% Conf.", "Interval]", "P>|z|")


    row.names(.df) <- NULL

    ## add dash lines
    addDashLines(.df, .vLine = 2)
}

## calculate RISK DIFFERENCE
## Essential Medical Statistics Page 152
calcRD <- function(.tbl, .x.name, rnd = 3) {
    .a <- .tbl[1, 1]
    .b <- .tbl[1, 2]
    .c <- .tbl[2, 1]
    .d <- .tbl[2, 2]

    .n1 <- (.a + .b)
    .p1 <- .a / .n1
    .n0 <- (.c + .d)
    .p0 <- .c / .n0

    .rd <- .p1 - .p0
    .se <- sqrt((.p1 * (1 - .p1) / .n1) +  (.p0 * (1 - .p0) / .n0))
    .ll <- .rd - (1.96 * .se)
    .ul <- .rd + (1.96 * .se)

    ## Test that the difference between two proportions is zero
    .p <- (.c + .a) / (.n0 + .n1)
    ## add tests for continuity correction for sample size less than 40 <<<<< ----
    .z <- .rd / sqrt(.p * (1 - .p) * ((1/.n1) + (1/.n0)))
    .pvalue <- tryCatch({
        suppressWarnings(2*pnorm(-abs(.z)))
    }, error = function(err) {
        return(NA)
    })


    ## create data frame
    .df <- data.frame(
        cbind(rownames(.tbl), "|",
              .tbl,
              # rbind(sprintf(.p1, fmt = paste0('%#.', rnd, 'f')),
              #       sprintf(.p0, fmt = paste0('%#.', rnd, 'f'))),
              rbind(c(sprintf(.rd, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.ll, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.ul, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.pvalue, fmt = paste0('%#.', rnd, 'f'))),
                    c("", "", "", ""))
        ), stringsAsFactors = FALSE
    )
    names(.df) <- c(.x.name, "|", colnames(.tbl),
                    # "Risks",
                    "Risk Diff.", "[95% Conf.", "Interval]", "P>|z|")


    row.names(.df) <- NULL

    ## add dash lines
    addDashLines(.df, .vLine = 2)
}


## Calculate Attributable risks for the exposed group
## Essential Medical Statistics Page 152
## https://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20relative%20risk.htm
calcAR <- function(.tbl, .x.name, rnd = 3)
{
    .a <- .tbl[1, 1]
    .b <- .tbl[1, 2]
    .c <- .tbl[2, 1]
    .d <- .tbl[2, 2]

    .n1 <- (.a + .b)
    .p1 <- .a / .n1
    .n0 <- (.c + .d)
    .p0 <- .c / .n0
    .n <- sum(.tbl)

    .rr <- .p1 / .p0

    .ar <- (.rr - 1) / .rr
    .se <- sqrt( (.a + .c) / .n * (1 - ((.a + .c) / .n)) * (1/.n1 + 1/.n0))
    .ll <- .ar - (1.96 * .se / ((.a / .n1) - (.c / .n0)))
    .ul <- .ar + (1.96 * .se / ((.a / .n1) - (.c / .n0)))


    ## Add p-value later
    .pvalue <- ""

    ## create data frame
    .df <- data.frame(
        cbind(rownames(.tbl), "|",
              .tbl,
              # rbind(sprintf(.p1, fmt = paste0('%#.', rnd, 'f')),
              #       sprintf(.p0, fmt = paste0('%#.', rnd, 'f'))),
              rbind(c(sprintf(.ar, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.ll, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.ul, fmt = paste0('%#.', rnd, 'f'))),
                    c("", "", ""))
        ), stringsAsFactors = FALSE
    )
    names(.df) <- c(.x.name, "|", colnames(.tbl),
                    # "Risks",
                    "AR (%)", "[95% Conf.", "Interval]")


    row.names(.df) <- NULL

    ## add dash lines
    addDashLines(.df, .vLine = 2)
}


## to add PAR for multivariate data <<<-----
## Calculate Population Attributable risks
## Altman's Statistics with confidence Page 59
calcPAR <- function(.tbl, .x.name, rnd = 3)
{
    .a <- .tbl[1, 1]
    .b <- .tbl[1, 2]
    .c <- .tbl[2, 1]
    .d <- .tbl[2, 2]

    .n1 <- (.a + .b)
    .p1 <- .a / .n1
    .n0 <- (.c + .d)
    .p0 <- .c / .n0

    .rr <- .p1 / .p0
    ## prevalence of risk factor in the population
    .p <- (.a + .b) / sum(.tbl)
    ## use delta method
    .se <- sqrt((1/.a) - (1/.n1) + (1/.c) - (1/.n0))
    .ll <- .rr / exp(1.96 * .se)
    .ul <- .rr * exp(1.96 * .se)

    .ar <- (.p * (.rr - 1)) / (1 + (.p * (.rr - 1)))
    .ll <- (.p * (.ll - 1)) / (1 + (.p * (.ll - 1)))
    .ul <- (.p * (.ul - 1)) / (1 + (.p * (.ul - 1)))

    ## Add p-value later
    .pvalue <- ""

    ## create data frame
    .df <- data.frame(
        cbind(rownames(.tbl), "|",
              .tbl,
              # rbind(sprintf(.p1, fmt = paste0('%#.', rnd, 'f')),
              #       sprintf(.p0, fmt = paste0('%#.', rnd, 'f'))),
              rbind(c(sprintf(.ar, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.ll, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.ul, fmt = paste0('%#.', rnd, 'f'))),
                    c("", "", ""))
        ), stringsAsFactors = FALSE
    )
    names(.df) <- c(.x.name, "|", colnames(.tbl),
                    # "Risks",
                    "Pop. AR", "[95% Conf.", "Interval]")


    row.names(.df) <- NULL

    ## add dash lines
    addDashLines(.df, .vLine = 2)
}


## Calculate Efficacy
## Essential Medical Statistics Page 455
calcEff <- function(.tbl, .x.name, rnd = 3)
{
    .a <- .tbl[1, 1]
    .b <- .tbl[1, 2]
    .c <- .tbl[2, 1]
    .d <- .tbl[2, 2]

    .n1 <- (.a + .b)
    .p1 <- .a / .n1
    .n0 <- (.c + .d)
    .p0 <- .c / .n0


    .rr <- .p1 / .p0
    ## use delta method
    .se <- sqrt(((1/.a) - (1/.n1)) + ((1/.c) - (1/.n0)))

    .eff <- 1 - .rr
    .ll <- 1 - (.rr * exp(1.96 * .se))
    .ul <- 1 - (.rr / exp(1.96 * .se))


    ## Add p-value later
    .pvalue <- ""


    ## create data frame
    .df <- data.frame(
        cbind(rownames(.tbl), "|",
              .tbl,
              # rbind(sprintf(.p1, fmt = paste0('%#.', rnd, 'f')),
              #       sprintf(.p0, fmt = paste0('%#.', rnd, 'f'))),
              rbind(c(sprintf(.eff, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.ll, fmt = paste0('%#.', rnd, 'f')),
                      sprintf(.ul, fmt = paste0('%#.', rnd, 'f'))),
                    c("", "", ""))
        ), stringsAsFactors = FALSE
    )
    names(.df) <- c(.x.name, "|", colnames(.tbl),
                    # "Risks",
                    "Efficacy", "[95% Conf.", "Interval]")


    row.names(.df) <- NULL

    ## add dash lines
    addDashLines(.df, .vLine = 2)
}

tabMHRR <- function(.data, .x, .by, .strata, exp_value = NULL, case_value = NULL,
                    measure = "ratio", plot = TRUE, na.rm = FALSE, rnd = 1)
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
        splitStrataRR(.x, .by, .strata, .x.name, .by.name, exp_value, case_value,
                    measure, plot, na.rm, rnd)
    })

    .df
}

splitStrataRR <- function(.x, .by, .strata, .x.name, .by.name, exp_value = NULL,
                          case_value = NULL, measure, plot, na.rm, rnd)
{
    .strata.lvl <- unique(.strata)
    if (na.rm) {
        .strata.lvl <- .strata.lvl[!is.na(.strata.lvl)]
    }


    ## check NA
    .useNA <- ifelse(na.rm, "no", "ifany")

    ## calculate MHOR
    .tbl <- switch(
        toupper(measure),
        RATIO = lapply(.strata.lvl, function (z) {
            calcMHRR(.strata, z, .x, .x.name, .by, exp_value, case_value, .useNA, rnd)
        }),
        DIFF = lapply(.strata.lvl, function (z) {
            calcMHRD(.strata, z, .x, .x.name, .by, exp_value, case_value, .useNA, rnd)
        })
    )


    ## check exposure value
    .tbl1 <- do.call(rbind, lapply(.tbl, function(z) {
        if (is.null(exp_value)) {
            z[1, ]
        } else {
            z[z[, .x.name] == exp_value, ]
        }
    }))

    ## calculate MHOR and 95% CI
    .derive <- .tbl1[, c("mhrr.num", "mhrr.denom", "se.num", "se.denom",
                         "chi.num", "chi.denom", "a")]
    .derive <- sapply(.derive, function(z) sum(z))

    .mhrr <- .derive["mhrr.num"] / .derive["mhrr.denom"]

    .se <- ifelse(toupper(measure) == "RATIO",
                  sqrt(.derive["se.num"] / .derive["se.denom"]),
                  sqrt(.derive["se.num"]) / .derive["se.denom"])
    .ll <- ifelse(toupper(measure) == "RATIO",
                  exp(log(.mhrr) - (1.96 * .se)),
                  .mhrr - (1.96 * .se))
    .ul <- ifelse(toupper(measure) == "RATIO",
                  exp(log(.mhrr) + (1.96 * .se)),
                  .mhrr + (1.96 * .se))


    .x2 <- (.derive["a"] - .derive["chi.num"])^2 / .derive["chi.denom"]
    .chi <- tryCatch({
        suppressWarnings(pchisq(.x2, df = 1, lower.tail = FALSE))
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


    .txt <- ifelse(toupper(measure) == "RATIO", "RR", "RD")
    .df <- rbind(.df,
                 c("Pooled", "estimate", "of", "MH", .txt,
                   sprintf(c(.mhrr, .ll, .ul, .chi), fmt = paste0('%#.', rnd, 'f'))))

    row.names(.df) <- NULL


    ## plot OR and 95% CI
    if (plot) {
        .txt <- ifelse(toupper(measure) == "RATIO", "Risk Ratio", "Risk Diff.")
        .rr <- as.numeric(c(.tbl1[[.txt]], .mhrr))
        .ll <- as.numeric(c(.tbl1[["[95% Conf."]], .ll))
        .ul <- as.numeric(c(.tbl1[["Interval]"]], .ul))

        .txt <- ifelse(toupper(measure) == "RATIO", "MHRR", "MHRD")
        names(.rr) <- c(.tbl1[["Strata"]], .txt)

        .txt <- ifelse(toupper(measure) == "RATIO", "Risk Ratios", "Risk Differences")
        plotRisks(.rr, .ll, .ul, .x.name, .by.name, .txt)
    }

    .df
}
calcMHRR <- function(.strata, .strata.lvl, .x, .x.name, .by, exp_value, case_value,
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

    .n1 <- (.a + .b)
    .p1 <- .a / .n1
    .n0 <- (.c + .d)
    .p0 <- .c / .n0

    .rr <- .p1 / .p0
    ## use delta method
    .se <- sqrt((1/.a) - (1/.n1) + (1/.c) - (1/.n0))
    .ll <- .rr / exp(1.96 * .se)
    .ul <- .rr * exp(1.96 * .se)

    ## Test of null hypothesis
    .z <- log(.rr) / .se
    .pvalue <- tryCatch({
        suppressWarnings(2*pnorm(-abs(.z)))
    }, error = function(err) {
        return(NA)
    })


    rr <- sprintf(c(.rr, .ll, .ul, .pvalue), fmt = paste0('%#.', rnd, 'f'))

    .n <- sum(.tbl)
    .mhrr.num <- .a * .n0 / .n
    .mhrr.denom <- .c * .n1 / .n
    .se.num <- ((.a + .c) * (.n1 * .n0 / (.n^2))) - (.a * .c / .n)
    .se.denom <- .mhrr.num * .mhrr.denom
    .chi.num <- (.n1 * (.a + .c)) / .n
    .chi.denom <- .n1 * .n0 * ((.a + .c) * (.b + .d) / ((.n^2) * (.n - 1)))


    ## create result data
    .df <- data.frame(cbind(.strata.lvl, x.name = row.names(.tbl), "|",
                            .tbl, rbind(rr, rr)),
                      cbind(.mhrr.num, .mhrr.denom, .se.num, .se.denom,
                            .chi.num, .chi.denom,
                            .a),
                      stringsAsFactors = FALSE)
    names(.df) <- c("Strata", .x.name, "|", colnames(.tbl),
                    "Risk Ratio", "[95% Conf.", "Interval]", "P>|z|",
                    "mhrr.num", "mhrr.denom", "se.num", "se.denom",
                    "chi.num", "chi.denom", "a")

    row.names(.df) <- NULL

    .df
}


# A comparison of two methods of estimating a common risk difference in a stratified
# analysis of a multicenter clinical trial
# Thomas W.O'GormanPhDaRobert F.WoolsonPhDbMichael P.JonesPhDb
## doi.org/10.1016/0197-2456(94)90017-5
calcMHRD <- function(.strata, .strata.lvl, .x, .x.name, .by, exp_value, case_value,
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

    .n1 <- (.a + .b)
    .p1 <- .a / .n1
    .n0 <- (.c + .d)
    .p0 <- .c / .n0


    .rd <- .p1 - .p0
    .se <- sqrt((.p1 * (1 - .p1) / .n1) +  (.p0 * (1 - .p0) / .n0))
    .ll <- .rd - (1.96 * .se)
    .ul <- .rd + (1.96 * .se)

    ## Test that the difference between two proportions is zero
    .p <- (.c + .a) / (.n0 + .n1)
    .z <- .rd / sqrt(.p * (1 - .p) * ((1/.n1) + (1/.n0)))
    .pvalue <- tryCatch({
        suppressWarnings(2*pnorm(-abs(.z)))
    }, error = function(err) {
        return(NA)
    })

    rd <- sprintf(c(.rd, .ll, .ul, .pvalue), fmt = paste0('%#.', rnd, 'f'))

    .n <- sum(.tbl)

    ## Gerstman's Epidemoilogy kept simple, page 330
    # .mhrr.num <- ((.a * .n0) - (.c * .n1)) / .n
    # .mhrr.denom <- .n0 * .n1 / .n

    # .se.num <- (.n1 * .n0 / .n)^2 * ((.a * .d / (.n1^2) * (.n1 - 1)) +
    #                                      (.c * .b / (.n0^2) * (.n0 - 1)))
    # .se.denom <- .n1 * .n0 / .n

    ## A Comparison of Two Methods of Estimating
    # a Common Risk Difference in a Stratified
    # Analysis of a Multicenter Clinical Trial
    # Thomas W. O'Gorman, PhD, Robert F. Woolson, PhD,
    # and Michael P. Jones, PhD
    # ControlledClinicalTrials15:13,5--153(1994)
    ## using CMH weights
    .w.cmh <- (.n1 * .n0 / .n)
    .mhrr.num <- .w.cmh * .rd
    .mhrr.denom <- .w.cmh

    .se.num <- ((.a * .b * (.n0)^3) + (.c * .d * (.n1)^3)) / (.n1 * .n0 * (.n^2))
    .se.denom <- .w.cmh

    .chi.num <- (.n1 * (.a + .c)) / .n
    .chi.denom <- .n1 * .n0 * ((.a + .c) * (.b + .d) / ((.n^2) * (.n - 1)))


    ## create result data
    .df <- data.frame(cbind(.strata.lvl, x.name = row.names(.tbl), "|",
                            .tbl, rbind(rd, rd)),
                      cbind(.mhrr.num, .mhrr.denom, .se.num, .se.denom,
                            .chi.num, .chi.denom,
                            .a),
                      stringsAsFactors = FALSE)
    names(.df) <- c("Strata", .x.name, "|", colnames(.tbl),
                    "Risk Diff.", "[95% Conf.", "Interval]", "P>|z|",
                    "mhrr.num", "mhrr.denom", "se.num", "se.denom",
                    "chi.num", "chi.denom", "a")

    row.names(.df) <- NULL

    .df
}

