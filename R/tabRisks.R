#' @title Calculating Risks and Relative Risks
#'
#' @description
#' \code{tabRisks()} cross-tabulates two variables and
#' reports risks of failure \code{by} among exposed and
#' unexposed levels of explanatory variable \code{...}.
#' It is used in cross-sectional studies.
#'
#' @param data Dataset
#' @param ... Variable or multiple variables
#' Colon separator \code{:} can be used to specify multiple variables.
#' @param by Varaiable for cross-tabulation
#' @param exp_value value for exposure as reference
#' @param case_value value for outcome as reference
#' @param plot logical value to display plots of rates across a categorical
#' variable
#' @param na.rm A logical value to specify missing values, `NA` in the table
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#'
#' @details
#' Risks are sometimes called proportions, incidence proportions or
#' prevalence.
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
#'
#' \enumerate{
#'     \item  Betty R. Kirkwood, Jonathan A.C. Sterne (2006, ISBN:978–0–86542–871–3)
#'     \item B. Burt Gerstman (2013, ISBN:978-1-4443-3608-5)
#'     \item Douglas G Altman (2005, ISBN:0 7279 1375 1)
#' }
#'
#'
#' @import graphics
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
#' ## use infert data
#' data(infert)
#'
#' tabRisks(infert, education, by = case, case_value = 1)
#' tabRisks(infert, induced, by = case)
#'
#' tabRisks(infert, education, induced, by = case)
#'
#' @export
tabRisks <- function(data, ... , by, exp_value = NULL, case_value = NULL, plot = TRUE,
                     na.rm = FALSE, rnd = 3)
{
    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    ## assign data into .data for further evaluation
    .data <- data

    ## get variable names within three dots to search for duplicates
    ## <<<< Change this ----- <<<<<<<<
    .vars <- as.character(enquos(.args, c("data", "by", "exp_value", "case_value",
                                          "plot", "na.rm", "rnd")))

    ## check colon, and check data types if the whole dataset
    .vars <- checkEnquos(.data, .vars, .types = "tab")


    ## if by not specify, stop
    by <- as.character(.args$by)
    if (length(by) == 0) {
        stop(" ... Specify 'by' ... ")
    }

    .df <- lapply(.vars, function(z) {
        tabrisk(.data, z, by, exp_value, case_value, plot, na.rm, rnd)
    })


    ## constructs labels
    ## add label for by: cross-tabulation
    .lbl <- sapply(.vars, function(z) attr(.data[[z]], "label"))

    ## Print tabulation
    sapply(1:length(.vars), function(z) {
        printText2(.df[[z]],
                   paste0("Estimates of Risks of '", .args$by, "'"),
                   .printDF = TRUE)
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

    invisible(.df)
}



# Helpers -----------------------------------------------------------------



tabrisk <- function(.data, .x, .by, exp_value = NULL, case_value = NULL, plot = TRUE,
                    na.rm = FALSE, rnd = 1)
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


    ## calculate total
    .tbl <- data.frame(rbind(.tbl, Total = colSums(.tbl)),
                       stringsAsFactors = FALSE)

    ## calculate risks
    .n <- rowSums(.tbl)
    .p <- .tbl[, 1] / .n
    .q <- 1 - .p
    .se <- sqrt(.p * .q / .n)
    .ll <- .p - (1.96 * .se)
    .ul <- .p + (1.96 * .se)

    ## create data frame
    .df <- data.frame(x.name = rownames(.tbl), "|" = "|",
                      .tbl,
                      Odds = cbind(sprintf(.p, fmt = paste0('%#.', rnd, 'f')),
                                   sprintf(.ll, fmt = paste0('%#.', rnd, 'f')),
                                   sprintf(.ul, fmt = paste0('%#.', rnd, 'f'))),
                      stringsAsFactors = FALSE
    )
    names(.df)[1] <- .x.name
    names(.df)[c(2, 5:7)] <- c("|", "Risks", "[95% Conf.", "Interval]")

    ## separate total row
    .df.total <- .df["Total", ]

    ## add dash lines
    .df <- addDashLines(.df[-nrow(.df), ], .vLine = 2)

    ## add total row
    .df <- rbind(.df, .df.total)

    row.names(.df) <- NULL


    ## plotting
    if (plot) {
        plotRisks(.p, .ll, .ul, .x.name, .by.name, "Risks")
    }

    return(.df)
}

