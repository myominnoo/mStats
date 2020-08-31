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
    stop(paste0("`", .data_name, "` must be a data.frame"),
         call. = FALSE)
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
  .a <- as.numeric(.tbl[1, 1])
  .b <- as.numeric(.tbl[1, 2])
  .c <- as.numeric(.tbl[2, 1])
  .d <- as.numeric(.tbl[2, 2])

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
      a <- as.numeric(.t[1, 1])
      b <- as.numeric(.t[1, 2])
      c <- as.numeric(.t[2, 1])
      d <- as.numeric(.t[2, 2])

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
