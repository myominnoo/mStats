#' @title Calculating measures of Risk including Risk Ratio
#'
#' @description
#' \code{mhrr()} calculates different measures of risk including risk
#' ratios (RR) as well as
#' Mantel-Haenszel pooled estimates.
#'
#' @param data Dataset
#' @param exp exposure or independent variables
#' @param case case or dependent variables (outcomes)
#' @param strata if specified, MH OR is calculated.
#' @param exp_value value for exposure as reference
#' @param case_value value for outcome as reference
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#'
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
#' among exposed and population are calculated when RR is greated than or
#' equal to 1.
#' If RR is less than 1, preventable fractions,  \code{Prev. Frac. Exp}
#' and  \code{Attr. Frac. Pop} are calculated.
#'
#' It produces a table with Risk Ratio, 95% CI as well as
#' p-value. If \code{strata} is specified, `Mantel-Haenzsel` Pooled
#' estimates of `Risk Ratio` is generated along with Chi-squared test for
#' homogeneity.
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
#' \deqn{RR_MH = \sum{Ak x N0k / Nk} / \sum{Ck x N1k / Nk}}
#'
#' \deqn{SE(log RR_MH) = \sqrt{ \sum{D1k x N1k x N0k / Nk^2 - Ak x Ck / Nk} / \sum{Ak x N0k / Nk} x \sum{Ck x N1k / Nk}}}
#'
#'
#' \deqn{Lower Limit of CI = exp(log RR_MH - 1.96 x SE(log RR_MH)) }
#'
#' \deqn{Upper Limit of CI = exp(log RR_MH + 1.96 x SE(log RR_MH)) }
#'
#'
#' ## Mantel–Haenszel test statistic
#'
#' A test of association (H0: RR_MH = 1 cohort studies; H0: RR_MH = 1 case–control studies)
#' is carried out with the Mantel–Haenszel test statistic using chi-squared distribution:
#'
#' \deqn{x2_MH = (\sum{Ak} - \sum{N1k x M1k / Nk})^2 / \sum{N1k x N0k x M1k x M0k / Nk^2 x (Nk - 1)}}
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
#' A test of association (H0: RD_CMH = 1 cohort studies; H0: RD_CMH = 1 case–control studies)
#' is carried out with the Mantel–Haenszel test statistic using chi-squared distribution:
#'
#' \deqn{x2_MH = (\sum{Ak} - \sum{N1k x M1k / Nk})^2 / \sum{N1k x N0k x M1k x M0k / Nk^2 x (Nk - 1)}}
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
#' @references
#'
#'
#' \enumerate{
#'     \item  Betty R. Kirkwood, Jonathan A.C. Sterne (2006, ISBN:978–0–86542–871–3)
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
#' )
#'
#' ## label variable and dataset
#' lung <- labelVar(lung, smoking="Yes or No", cancer="Yes or no")
#' lung <- labelData(lung, "Follow up lung cancer study")
#'
#' ## check dataset
#' codebook(lung)
#'
#' ## calculate RR
#' mhrr(lung, smoking, cancer, exp_value = "Smokers", case_value = "Yes")
#'
#'
#'
#' ## Simpson's paradox
#' ## Burt Gerstman's Epidemiology, page 326, table 14.1
#' simpson <- expandTables("1" = c(1000, 9000, 50, 950),
#'                         "2" = c(95, 5, 5000, 5000),
#'                         exp_name = "trt",
#'                         exp_lvl = c("new", "standard"),
#'                         case_name = "case",
#'                         case_lvl = c("alive", "dead"),
#'                         strata_name = "clinic")
#'
#' ## calculate RR
#' mhrr(simpson, trt, case, exp_value = "new", case_value = "alive")
#'
#' ## calculate MH RR
#' mhrr(simpson, trt, case, clinic)
#'
#' @export
mhrr <- function(data, exp , case, strata = NULL, exp_value = NULL,
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

  ## if strata is not specified, calculate RR. Otherwise, calculate MHRR
  if (length(strata) == 0) {

    ## change row and col orders
    .tbl <- rowColOrder(.tbl, exp_value, case_value)
    ## split tables if nrow > 2
    .tbl <- splitTables(.tbl, .exp.value = exp_value)

    ## calculate OR
    .df <- do.call(
      rbind,
      lapply(.tbl, function(z) {
        calcRR(z, exp, rnd)
      })
    )

    ## create label
    .lbl <- paste0(" Measure of Association : Risk Ratio for '", case, "'")
  } else {
    ## exp var must be binary for MHOR.
    if (nrow(.tbl) > 2) {
      stop(paste0("'", exp, "' must be binary."), call. = FALSE)
    }

    ## create single vector for strata
    .strata <- .data[[strata]]

    ## calculate MHOR
    .df <- calcMHRR(.exp, .case, .strata, exp_value, case_value, rnd)

    ## calculate crude OR
    .crude <- calcRR(table(.exp, .case, useNA = "no"), exp, rnd)[7, -2]
    .crude[1, 1] <- "Crude"

    ## add crude OR to MHOR table
    .df[nrow(.df)-3, ] <- .crude
    names(.df)[1] <- strata

    ## add dash lines
    .df <- addDashLines(.df, .vline = 2)

    ## create label
    .lbl <- paste0("                Mantel-Haenszel (M-H) Risk Ratio")
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


calcRR <- function(.tbl, exp, rnd)
{
  ## create row and column totals and odds.
  .tblr <- cbind(.tbl, Total = rowSums(.tbl))
  .tblc <- rbind(.tblr, Total = colSums(.tblr))
  .tblf <- cbind(
    .tblc,
    Risks = sprintf(.tblc[, 1] / .tblc[, 3], fmt = paste0("%#.", rnd, "f"))
  )

  ## calculate odds ratio, 95% CI, and p-value
  .a <- .tbl[1, 1]
  .b <- .tbl[1, 2]
  .c <- .tbl[2, 1]
  .d <- .tbl[2, 2]

  .n <- sum(.tbl)


  ## calculate OR
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
  .or <- c("Odds Ratio", sprintf(c(.or, .confint, .chi$p.value),
                                 fmt = paste0("%#.", rnd, "f")))

  ## calculate Risk Ratio
  .n1 <- .a + .b
  .n0 <- .c + .d
  .p1 <- .a / .n1
  .p0 <- .c / .n0
  .rr <- .p1 / .p0
  .rr.se <- sqrt((1 / .a) - (1 / .n1) + (1 / .c) - (1 / .n0))
  .rr.confint <- exp(c(log(.rr) - (1.96 * .rr.se),
                       log(.rr) + (1.96 * .rr.se)))
  .rr_ <- c("Risk Ratio", sprintf(c(.rr, .rr.confint, .chi$p.value),
                                  fmt = paste0("%#.", rnd, "f")))


  ## calculate Risk difference
  .rd <- .p1 - .p0
  .rd.se <- sqrt((.p1 * (1 - .p1) / .n1) +  (.p0 * (1 - .p0) / .n0))
  .rd.confint <- c(.rd - (1.96 * .rd.se), .rd + (1.96 * .rd.se))
  .p <- (.a + .c) / .n
  .rd.z <- .rd / sqrt(.p * (1 - .p) * (1/.n1 + 1/.n0))
  .rd.p <- pnorm(.rd.z, lower.tail = FALSE)
  .rd <- c("Risk Difference", sprintf(c(.rd, .rd.confint, .rd.p),
                                      fmt = paste0("%#.", rnd, "f")))



  ## calculate attributable or prevented fraction
  ## STATA 14 MANUAL, page 554
  ## Unstratified cumulative incidence data (cs and csi)
  if (.rr >= 1) {
    .afe <- (.rr - 1) / .rr
    .afe_confint <- (.rr.confint - 1) / .rr.confint
    .afp <- .afe * .a / .n1
    # .afp <- ((.a + .c) / .n) - (.c / .n0)

    .afe <- c("Attr. Frac. Exp",
              sprintf(c(.afe, .afe_confint), fmt = paste0('%#.', rnd, 'f')),
              "")
    .afp <- c("Attr. Frac. Pop",
              sprintf(.afp, fmt = paste0('%#.', rnd, 'f')),
              "", "", "")
  } else {
    .afe <- 1 - .rr
    .afe_confint <- 1 - .rr.confint
    .afp <- .afe * ( (.a + .c) / .n)
    .afe <- c("Prev. Frac. Exp",
              sprintf(c(.afe, .afe_confint[2], .afe_confint[1]),
                      fmt = paste0('%#.', rnd, 'f')),
              "")
    .afp <- c("Prev. Frac. Pop",
              sprintf(.afp, fmt = paste0('%#.', rnd, 'f')),
              "", "", "")
  }

  ## combine all in one dataframe
  .df <- data.frame(row.names(.tblf), .tblf)

  ## get other estimates
  .est <- data.frame(rbind(.rr_, .or, .rd, .afe, .afp))
  names(.est) <- names(.df) <- c(exp, colnames(.tblf))

  ## combine into final dataframe
  .df <- rbind(.df,
               c(" ", "Estimates", "[95% Conf.", "Interval]", "Pr>chi2"),
               .est)
  row.names(.df) <- NULL

  ## add dash lines
  .df <- addDashLines(.df)
  .df <- rbind(.df[2:4, ], .df[1, ], .df[5:10, ])
  .df <- addDashLines(.df, .vline = 2)
  row.names(.df) <- NULL

  return(.df)
}
calcMHRR <- function(.exp, .case, .strata, exp_value, case_value, rnd)
{

  ## make three tables
  .tbl <- table(.exp, .case, .strata, useNA = "no")
  .lvl_len <- dim(.tbl)[3]

  ## calculate MHRR and homogeneity test
  .t <- do.call(
    rbind,
    lapply(1:.lvl_len, function(z) {
      ## change row and col orders
      .t <- rowColOrder(.tbl[,,z], exp_value, case_value)
      .strata_name <- dimnames(.tbl)$.strata[z]
      a <- .t[1, 1]
      b <- .t[1, 2]
      c <- .t[2, 1]
      d <- .t[2, 2]

      n <- sum(.t)
      n1 <- a + b
      n0 <- c + d
      m1 <- a + c
      m0 <- b + d

      ## MHRR ==> (Rothman 2002 p 148 and 152, equation 8-2):
      q <- a * n0 / n
      r <- c * n1 / n

      ## MHRR confidence interval
      u <- ((m1 * n1 * n0) / n^2) - ((a * c) / n)
      v <- (a * n0) / n
      x <- (c * n1) / n

      ##
      ## Woolf test of homogeneity of risk ratios (Jewell 2004, page 154).
      # First work out the Woolf estimate of the adjusted risk ratio
      ## based on Jewell (2004, page 134):
      rr <- log((a / (a + b)) / (c / (c + d)))
      var <- (b / (a * (a + b))) + (d / (c * (c + d)))
      w <- 1 / var
      wrr <- w * rr

      ## calculate stratum-specific RR
      ## cacluate OR
      res <- calcRR(.t, "tocombine", rnd)[7, -2]
      res[1, 1] <- .strata_name

      ## return result
      .df <- data.frame(res, q, r, u, v, x, rr, w, wrr)
      names(.df)[1:5] <- c("", "Point Estimate",
                           "[95% Conf.", "Interval]", "Pr>chi2")
      .df
    })
  )

  .mhrr <- sum(.t$q) / sum(.t$r)
  .mhrr.se <- sqrt(sum(.t$u) / (sum(.t$v) * sum(.t$x)))
  .mhrr.confint <- exp(c(log(.mhrr) - (1.96 * .mhrr.se),
                         log(.mhrr) + (1.96 * .mhrr.se)))

  ## calculate chisquare test for MHRR
  .mhrr.p <- tryCatch({
    suppressWarnings(mantelhaen.test(.tbl, correct = FALSE)$p.value)
  }, error = function(err) {
    return(NA)
  })

  .mhrr <- c("M-H Combined",
             sprintf(c(.mhrr, .mhrr.confint, .mhrr.p),
                     fmt = paste0("%#.", rnd, "f")))


  ## test of homogeneity
  lnRR <- sum(.t$wrr) / sum(.t$w)
  # Equation 10.3 from Jewell (2004):
  .chi <- sum(.t$w * (.t$rr - lnRR)^2)
  .pvalue <- tryCatch({
    suppressWarnings(pchisq(.chi, df = .lvl_len - 1, lower.tail = FALSE))
  }, error = function(err) {
    return(NA)
  })

  ## combine estimates
  .t <- .t[, 1:5]
  .df <- data.frame(
    rbind(
      c("", "", "", "", ""),
      .mhrr,
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
