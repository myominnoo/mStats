#' @title Report parameters from regression models
#'
#' @description
#' \code{fModelOutput} display corresponding estimates of different models along
#' with 95% confidence intervals and p-values.
#'
#' @param model object glm or lm type
#' @param rnd rounding digits
#' @param print.table logical value to display formatted outputs
#' @param ... optional arguments
#' @details
#'
#' \code{fModelOutput}
#'
#' reports parameters from regression models. Currently supporting model is
#' logistic models. Other types will be incoporated in future works.
#'
#' In unadjusted analysis, it reports unajdusted odds ratios, 95% Confidence
#' interval and Wald's p-value.
#'
#' In adjusted analysis, both unadjusted and adjusted parameters are reported.
#'
#' The reports are in well-formatted texts which can be readily copied into
#' spreadsheet programs or format in word file.
#'
#' \strong{Please be reminded:}
#'
#' This is an ongoing work. In case of comments or suggestions, please contact me
#' at \email{dr.myominnoo@@gmail.com}.
#'
#' \strong{Reference: }
#' \enumerate{
#'   \item LOGIT REGRESSION | R DATA ANALYSIS EXAMPLES. UCLA: Statistical Consulting
#'   Group. from https://stats.idre.ucla.edu/r/dae/logit-regression/ (accessed
#'   September 27, 2019)
#' }
#'
#' @seealso \code{\link{testModelFit}}
#' @keywords model output, coefficients display
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#' ## example from IRDE website:
#' ## https://stats.idre.ucla.edu/r/dae/logit-regression/
#' mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
#' codebook(mydata)
#' tab(admit, mydata)
#' tab(rank, mydata)
#'
#' logit.gre <- glm(admit ~ gre, data = mydata, family = "binomial")
#' summary(logit.gre)
#' fModelOutput(logit.gre)
#' testModelFit(logit.gre)
#'
#' logit.multi <- glm(admit ~ gre + gpa + factor(rank), data = mydata, family = "binomial")
#' summary(logit.multi)
#'
#' fModelOutput(logit.multi) # generates parameters
#' testModelFit(logit.multi) # test overall significant of the model
#' }

#' @export
fModelOutput <- function(model, rnd = 1, print.table = TRUE)
{
  m.sum <- summary(model)
  raw <- coef(m.sum)
  m.call <- as.character(m.sum$call$formula)[-1]
  vars <- gsub(" ", "", unlist(strsplit(m.call, "+", fixed = TRUE)))

  if (length(vars) > 2) {
    model <- structure(model, class = "adjusted")
  } else {
    model <- structure(model, class = "unAdjusted")
  }
  UseMethod("fModelOutput", model)
}



#' @rdname fModelOutput
#' @export
fModelOutput.default <- function(...)
{
  stop("... Wrong model type ...")
}


#' @rdname fModelOutput
#' @export
fModelOutput.unAdjusted <- function(model, rnd = 1,
                                    print.table = TRUE)
{
  m.sum <- summary(model)
  raw <- coef(m.sum)
  if (nrow(raw) < 2)
    stop("... NULL model cannot be evaluated ...")

  e <- sprintf(exp(coef(m.sum)[, 1]), fmt = paste0('%#.', rnd, 'f'))
  ci <- suppressMessages(exp(confint(model)))
  ll <- sprintf(ci[, 1], fmt = paste0('%#.', rnd, 'f'))
  ul <- sprintf(ci[, 2], fmt = paste0('%#.', rnd, 'f'))
  p <- sprintf(coef(m.sum)[,4], fmt = paste0('%#.', 5, 'f'))
  t <- paste0(e, " (", paste(ll, ul, sep = " - "), ")")
  t <- as.data.frame(cbind(t, p))
  row.names(t) <- row.names(raw)
  t <- t[!(row.names(t) %in% "(Intercept)"), ]

  t.dis <- as.data.frame(cbind("|", t, "|"))
  colnames(t.dis) <- c("+", "------ uOR (95% CI)", "Pr(>|z|)", "------ +")

  if (print.table) {
    printText(t.dis, paste0("Output of Logistic Regression\n",
                            "Number of obs: " , length(model$y)),
              split = "Number of obs: ")
    printMsg(paste0("Log-likelihood: ",
                    sprintf(logLik(model), fmt = paste0('%#.', rnd, 'f'))))
    printMsg(paste0("AIC: ", sprintf(AIC(model), fmt = paste0('%#.', rnd, 'f'))))
  }

  invisible(t)
}



#' @rdname fModelOutput
#' @export
fModelOutput.adjusted <- function(model, rnd = 1, print.table = TRUE)
{
  m.sum <- summary(model)
  raw <- coef(m.sum)
  m.call <- as.character(m.sum$call$formula)[-1]
  vars <- gsub(" ", "", unlist(strsplit(m.call, "+", fixed = TRUE)))
  vars.ex <- vars[-1]

  t.uOR <- do.call(
    rbind, lapply(1:length(vars.ex), function(z) {
      texts <- paste0("glm(", vars[1], " ~ ", vars.ex[z],
                      ", family = ", m.sum$call$family,
                      ", data = ", m.sum$call$data, ")")
      v <- fModelOutput.unAdjusted(eval(parse(text = texts)),
                                   print.table = FALSE)
    })
  )

  m.sum <- summary(model)
  raw <- coef(m.sum)

  e <- sprintf(exp(coef(m.sum)[, 1]), fmt = paste0('%#.', rnd, 'f'))
  ci <- suppressMessages(exp(confint(model)))
  ll <- sprintf(ci[, 1], fmt = paste0('%#.', rnd, 'f'))
  ul <- sprintf(ci[, 2], fmt = paste0('%#.', rnd, 'f'))
  p <- sprintf(coef(m.sum)[,4], fmt = paste0('%#.', 5, 'f'))
  t <- paste0(e, " (", paste(ll, ul, sep = " - "), ")")
  t <- as.data.frame(cbind(t, p))
  row.names(t) <- row.names(raw)
  colnames(t) <- c("aOR (95% CI)", "Pr(>|z|)")
  t.aOR <- t[!(row.names(t) %in% "(Intercept)"), ]


  p.uOR <- as.numeric(as.character(t.uOR[,2]))
  p.aOR <- as.numeric(as.character(t.aOR[,2]))
  p.uOR <- ifelse(p.uOR < 0.001, "***",
                  ifelse(p.uOR < 0.01, "**",
                         ifelse(p.uOR < 0.05, "*", ".")))
  p.aOR <- ifelse(p.aOR < 0.001, "***",
                  ifelse(p.aOR < 0.01, "**",
                         ifelse(p.aOR < 0.05, "*", ".")))

  f.uOR <- as.data.frame(cbind("|", t.uOR, p.uOR, "|"))
  f.aOR <- as.data.frame(cbind("|", t.aOR, p.aOR, "|"))
  colnames(f.uOR) <- c("+", "--- uOR (95% CI)", " Pr(>|z|)", "---", "+")
  colnames(f.aOR) <- c("+", "--- aOR (95% CI)", " Pr(>|z|)", "---", "+")

  f <- list(UnAjustedOR = f.uOR, AjustedOR = f.aOR)

  if (print.table) {
    printText(f.uOR, paste0("Output of unadjusted Logistic Regression\n",
                            "Number of obs: " , length(model$y)),
              split = "Number of obs: ")
    printText(f.aOR, paste0("Output of Adjusted Logistic Regression\n",
                            "Number of obs: " , length(model$y)),
              split = "Number of obs: ")
    printMsg(paste0("Log-likelihood: ",
                    sprintf(logLik(model), fmt = paste0('%#.', rnd, 'f'))))
    printMsg(paste0("AIC: ", sprintf(AIC(model), fmt = paste0('%#.', rnd, 'f'))))
  }

  invisible(f)
}
