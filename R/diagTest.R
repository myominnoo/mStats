#' @title Statistics of Diagnostic Tests
#'
#' @description
#' \code{screening} reports statistics of diagnostic tests
#'
#' @param x a factor object or a table
#' @param y an optional factor object
#' @param p disease prevalence
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#'
#' @details
#' The screening tests are based on Bayes' Theorem. These tests help clinicians to
#' correctly predict the presence or absence of a particular disease from the
#' knowlege of test results (positive or negative) and/or the status of presenting
#' symptoms (present or absent) or information regarding the likelihood of positive
#' and negative test results and the likelihood of the presence or absence of a
#' particular symptom in patients with and without a particular disease.
#'
#' \strong{Statistics of diagnostic tests:}
#'
#' Sensitivity: True Positive (TP) rate among diseased (D+) = TP / D+
#'
#' Specificity: True Negative  (TN) rate among non-diseased (D-) = TN / D-
#'
#' Positive predictive value (PPV):
#' probability of D+ when all positives (AP) = D+ / AP
#'
#' Negative predictive value (NPV):
#' probability of D- when all negatives (AN) = D+ / AN
#'
#' Likelihood ratio (LR) = To summarize information about a diagnostic test
#' LR of positive result (LR+) = (Sensitivity) / (1 - Specificity)
#' LR of negative result (LR-) = (1 - Sensitivity) / (Specificity)
#'
#' \strong{Calculating confidence intervals:}
#'
#' 95% Confidence intervals were calculated by using Wilson Score Interval as
#' well as its continuity correction. The method was developed by Edwin Bidwell
#' Wilson in 1927. This interval has good properties even for a small sample.
#' Just lik Pearson's chi-squared test and Yates' continuity correction.
#'
#' Several other methods have been developed such as Clopper–Pearson interval and
#' Agresti–Coull interval. But Wilson score interval methods (with or without
#' continuity correction) have been shown to be the most accurate and the
#' most robust. For further details, see references.
#'
#'
#' \strong{Reference:}
#' \enumerate{
#'     \item Biostatistics A Foundation for Analysis in the Health Sciences
#'     (10th Edition). Chapter 3.5 BAYES’ THEOREM, SCREENING TESTS, SENSITIVITY,
#'     SPECIFICITY
#'     \item Avijit Hazra, Nithya Gogtay. Biostatistics Series Module 7: The
#'     Statistics of Diagnostic Tests. Indian J Dermatol. 2017 Jan-Feb; 62(1):
#'     18-24. doi: 10.4103/0019-5154.198047
#'     \item Brown, Lawrence D.; Cai, T. Tony; DasGupta, Anirban (2001). "Interval
#'     Estimation for a Binomial Proportion". Statistical Science. 16 (2): 101–133.
#'     \item Wallis, Sean A. (2013). "Binomial confidence intervals and contingency
#'     tests: mathematical fundamentals and the evaluation of alternative methods"
#'     .Journal of Quantitative Linguistics. 20 (3): 178–208.
#'     \item Newcombe, R. G. (1998). "Two-sided confidence intervals for the single
#'     proportion: comparison of seven methods". Statistics in Medicine. 17 (8):
#'     857–872. doi:10.1002/(SICI)1097-0258(19980430)17:8<857::AID-SIM777>3.0.CO;2-E.
#'     PMID 9595616.
#' }
#'
#' @import graphics
#' @seealso \code{\link{strate}}
#' @keywords diagnostic tests, screening tests, sensitivity, specificity, ppv, npv
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#' #### Biostatistics A Foundation for Analysis in the Health Sciences (10th Edition)
#' # numbers taken from Example 3.5.1
#'
#' diagTest(as.table(matrix(c(436, 5, 14, 495), ncol = 2, byrow = TRUE)))
#' diagTest(as.table(matrix(c(436, 5, 14, 495), ncol = 2, byrow = TRUE)),
#'          p = .113)
#'
#' #### Avijit Hazra, Nithya Gogtay. Biostatistics Series Module 7: The
#'     Statistics of Diagnostic Tests. Indian J Dermatol. 2017 Jan-Feb; 62(1):
#'     18-24. doi: 10.4103/0019-5154.198047
#'
#' diagTest(as.table(matrix(c(2, 18, 1, 182), ncol = 2, byrow = TRUE)))
#' diagTest(as.table(matrix(c(2, 18, 1, 182), ncol = 2, byrow = TRUE)),
#'          p = .10)
#' diagTest(as.table(matrix(c(30, 35, 23, 12), ncol = 2, byrow = TRUE)))
#' diagTest(as.table(matrix(c(30, 35, 23, 12), ncol = 2, byrow = TRUE)),
#'          p = .10)
#'
#' #### Just an example to demonstrate
#' diagTest(infert$case, infert$spontaneous)
#' }


#' @export
diagTest <- function(x, y = NULL, p = NULL, rnd = 2)
{
  if (is.table(x)) {
    x <- structure(x, class = "table")
  } else {
    x <- structure(x, class = "numeric")
  }
  UseMethod("diagTest", x)
}


#' @rdname diagTest
#' @export
diagTest.default <- function(x, y = NULL, p = NULL, rnd = 2) {
  stop("... Wrong data type ...")
}


#' @rdname diagTest
#' @export
diagTest.table <- function(x, y = NULL, p = NULL, rnd = 2)
{
  t <- x
  t.dimnames <- names(dimnames(t))
  t <- rbind(t, Total = colSums(t))
  t <- cbind(t, Total = rowSums(t))
  if (paste0(row.names(t), collapse = "") == "ABTotal") {
    row.names(t) <- c("Positive (+)", "Negative (-)", "Total")
    colnames(t) <- c("Present (+)", "Absent (-)", "Total")
    names(dimnames(t)) <- c("Test", "Disease")
  } else {
    names(dimnames(t)) <- t.dimnames
  }

  a <- t[1,1]
  b <- t[1,2]
  c <- t[2,1]
  d <- t[2,2]
  n <- sum(a, b, c, d)

  SEN <- a / (a + c)
  SPE <- d / (b + d)

  p.sample <- (a + c) / n
  if (is.null(p)) {
    p <- p.sample
    q <- (b + d) / n
    p.pop <- NULL
  } else {
    p.pop <- p
    q <- 1 - p
  }
  PPV <- (SEN * p) / ( (SEN * p) + (b / (b + d) * q) )
  NPV <- (SPE * q) / ( (SPE * q) + (c / (a + c) * p) )

  e <- c(SEN, SPE, PPV, NPV) * 100
  e <- sprintf(e, fmt = paste0('%#.', rnd, 'f'))

  SEN.ci <- ciCollapse(
    scoreCI(SEN, (a + c), 1.96) * 100, rnd = rnd)
  SEN.ci.c <- ciCollapse(
    scoreCI(SEN, (a + c), 1.96, correct = TRUE) * 100, rnd = rnd)
  SPE.ci <- ciCollapse(
    scoreCI(SPE, (b + d), 1.96) * 100, rnd = rnd)
  SPE.ci.c <- ciCollapse(
    scoreCI(SPE, (b + d), 1.96, correct = TRUE) * 100, rnd = rnd)
  PPV.ci <- ciCollapse(
    scoreCI(PPV, (a + b), 1.96) * 100, rnd = rnd)
  PPV.ci.c <- ciCollapse(
    scoreCI(PPV, (a + b), 1.96, correct = TRUE) * 100, rnd = rnd)
  NPV.ci <- ciCollapse(
    scoreCI(NPV, (c + d), 1.96) * 100, rnd = rnd)
  NPV.ci.c <- ciCollapse(
    scoreCI(NPV, (c + d), 1.96, correct = TRUE) * 100, rnd = rnd)

  ci <- matrix(c(SEN.ci, SEN.ci.c, SPE.ci, SPE.ci.c,
                 PPV.ci, PPV.ci.c, NPV.ci, NPV.ci.c),
               ncol = 2, byrow = TRUE)

  f <- as.data.frame(cbind(e, ci), stringsAsFactors = FALSE)

  LR.p <- sprintf(SEN / (1 - SPE), fmt = paste0('%#.', rnd, 'f'))
  LR.n <- sprintf((1 - SEN) / SPE, fmt = paste0('%#.', rnd, 'f'))

  #### post-test probability
  odds <- p / (1 - p)

  odds.LR.p <- odds * SEN / (1 - SPE)
  odds.LR.n <- odds * (1 - SEN) / SPE

  posttest.p <- sprintf(odds.LR.p / (1 + odds.LR.p) * 100,
                        fmt = paste0('%#.', rnd, 'f'))
  posttest.n <- sprintf(odds.LR.n / (1 + odds.LR.n) * 100,
                        fmt = paste0('%#.', rnd, 'f'))

  #### final combination
  f <- rbind(f, c(LR.p, "",""), c(LR.n, "", ""),
             c(posttest.p, "",""), c(posttest.n, "",""))

  row.names(f) <- c("Sensitivity", "Specificity", "PPV", "NPV",
                    "LR (+)", "LR (-)",
                    "PTP (LR+)", "PTP (LR-)")
  colnames(f) <- c("Estimate (%)", "95% CI (Wilson Score)",
                   "95% CI (Score Correction)")


  #### Printing
  printDeco("=", 72)
  cat(paste0("Cross-Tabulation of Screening Test ",
             "and Disease Status\n\n"))
  print(t)
  cat(paste0("\n    Sample Prevalence: ",
             sprintf(p.sample * 100, fmt = paste0('%#.', rnd, 'f')),
             " %",
             "\nPopulation Prevalence: ",
             ifelse(is.null(p.pop), "NOT GIVEN",
                    paste0(sprintf(p * 100, fmt = paste0('%#.', rnd, 'f')),
                           " %")), "\n"))
  printDeco("~", 72)
  cat(paste0("\t\t    Statistics of Diagnostic Tests\n"))
  printDeco("~", 72)
  print(f)
  printDeco("~", 72)
  printMsg(paste0("Note: used '", ifelse(is.null(p.pop),
                                         "Sample", "Population"),
                  "' prevalence in the calculation of PPV and NPV."))
  printMsg(paste0("PPV or NPV: Positive or Negative Predictive Value"))
  printMsg(paste0("LR (+) or (-): Likelihood Ratio of a positive or ",
                  "negative result"))
  printMsg(paste0("PTP (LR+): Post-Test Probability of disease ",
                  "given (+) test"))
  printMsg(paste0("PTP (LR-): Post-Test Probability of non-disease ",
                  "given (-) test"))
  printDeco("=", 72)

  invisible(f)
}


#' @rdname diagTest
#' @export
diagTest.numeric <- function(x, y = NULL, p = NULL, rnd = 2)
{
  x.name <- deparse(substitute(x))
  y.name <- deparse(substitute(y))
  t <- table(x, y)
  t <- t(t)

  a <- t[1,1]
  b <- t[1,2]
  c <- t[2,1]
  d <- t[2,2]
  n <- sum(a, b, c, d)

  p.sample <- (a + c) / n
  if (is.null(p)) {
    p <- p.sample
    q <- (b + d) / n
    p.pop <- NULL
  } else {
    p.pop <- p
    q <- 1 - p
  }

  if (ncol(t) != 2)
    stop("... x must have two levels ...")

  t.nrow <- nrow(t)
  t.rnames <- row.names(t)
  y.dim <- deparse(substitute(x))
  x.dim <- deparse(substitute(y))
  if (t.nrow > 2) {
    l <- lapply(1:t.nrow, function(z) {
      v <- (rbind(t[z, ], colSums(t[-z, ])))
      colnames(v) <- colnames(t)
      row.names(v) <- c(t.rnames[z], paste0(t.rnames[-z], collapse = "|"))
      names(dimnames(v)) <- c(x.dim, y.dim)
      v
    })

    f <- list()
    for (i in 1:t.nrow) {
      sink(tempfile())
      f[[i]] <- diagTest(as.table(l[[i]]), p = p, rnd = rnd)
      sink()
    }

    #### ROC Curves
    roc <- NULL
    for (i in 1:t.nrow) {
      roc.ss <- as.numeric(as.data.frame(f[[i]])[1:2, 1])
      roc <- rbind(roc, roc.ss)
    }
    roc <- as.data.frame(roc / 100)
    row.names(roc) <- row.names(t)
    roc <- rbind(p0 = c(0, 1), roc, p1 = c(1, 0))
    roc <- roc[order(roc[,1]), ]
    row.names(roc)[c(1, nrow(roc))] <- c("(0,0)", "(1,1)")
    roc.y <- roc[, 1]
    roc.y.len <- length(roc.y)
    roc.x <- 1 - roc[, 2]
    roc.x.len <- length(roc.x)

    par(las = 1, mar = c(5, 7, 5, 6))
    plot(roc.x, roc.y, xlim = c(0, 1), ylim = c(0, 1), type = "b", pch = 19,
         col = "blue", lwd = 2,
         xlab = "False Positive Rate (FPR)",
         ylab = "True Positive Rate (TPR)",
         main = paste0("Receiver Operating Curve (ROC)",
                       "\nCut-points: ", y.name, "\nDisease: ", x.name),
         cex.main = 1.2)
    abline(0, 1, col = "red")
    text(roc.x[1:(roc.x.len-1)], roc.y[1:(roc.y.len-1)],
         labels = row.names(roc)[1:(roc.x.len-1)], cex = 0.8, pos = 3)
    text(roc.x[roc.x.len], roc.y[roc.y.len],
         labels = row.names(roc)[roc.x.len], cex = 0.8, pos = 1)

    t <- rbind(t, colSums(t))
    t <- cbind(t, rowSums(t))
    row.names(t)[nrow(t)] <- "Total"
    colnames(t)[ncol(t)] <- "Total"
    names(dimnames(t)) <- c(x.dim, y.dim)

    ### Printing
    printDeco("=", 72)
    cat(paste0("Cross-Tabulations of ", x.dim, " and ", y.dim, "\n\n"))
    print(t)
    cat(paste0("\n    Sample Prevalence: ",
               sprintf(p.sample * 100, fmt = paste0('%#.', rnd, 'f')),
               " %",
               "\nPopulation Prevalence: ",
               ifelse(is.null(p.pop), "NOT GIVEN",
                      paste0(sprintf(p * 100, fmt = paste0('%#.', rnd, 'f')),
                             " %")), "\n"))
    printDeco("~", 72)
    cat(paste0("      Different Cut-off points and Statistics of ",
               "Diagnostic Tests\n"))
    printDeco("~", 72)
    for (i in 1:t.nrow) {
      print(l[[i]])
      printDeco(" ... ", 14)
      print(f[[i]])
      printDeco("-", 72)
    }
    printDeco("~", 72)
    printMsg(paste0("Note: used '", ifelse(is.null(p.pop),
                                           "Sample", "Population"),
                    "' prevalence in the calculation of PPV and NPV."))
    printMsg(paste0("PPV or NPV: Positive or Negative Predictive Value"))
    printMsg(paste0("LR (+) or (-): Likelihood Ratio of a positive or ",
                    "negative result"))
    printMsg(paste0("PTP (LR+): Post-Test Probability of disease ",
                    "given (+) test"))
    printMsg(paste0("PTP (LR-): Post-Test Probability of non-disease ",
                    "given (-) test"))
    printDeco("=", 72)

  } else {
    l <- t
    colnames(l) <- colnames(t)
    row.names(l) <- t.rnames
    names(dimnames(l)) <- c(x.dim, y.dim)
    f <- diagTest(l, p = p, rnd = rnd)

  }

  invisible(list(l, f))
}
