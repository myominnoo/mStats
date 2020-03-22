#' @title Calculating Odds
#'
#' @description
#' \code{tabOdds} generates cross-tabulation between two variables and
#' display odds of failure \code{var_case} among exposure variable
#' \code{var_exp}. It is used in case-control studies.
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
#'
#' @details
#' A table tabulating odds and corresponding 95\% confidence interval
#' is generated.
#'
#' \strong{Formula for calculating Odds}
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
#'
#'    \item Altman, Statistics with confidence
#' }
#'
#'
#' @import graphics
#'
#'
#' @concept
#'
#' odds odds ratio frequency table statistics descriptive
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
#' #' ## Example from Altman Statistics with Confidence, Chapter 7, Page 62
#' abo <- expandTables(c(54, 60, 89, 245),
#'                        exp_name = "study",
#'                        exp_lvl = c("cases", "controls"),
#'                        case_name = "state",
#'                        case_lvl = c("Yes", "No")) %>%
#'     labelVar(c(study, state),
#'              c("study group", "ABO non-secretor state")) %>%
#'     labelData(paste0("ABO non-secretor state for 114 patients with",
#'                      " spondyloarthropathies and 334 controls"))
#'
#'
#' ## check dataset
#' codebook(abo)
#'
#'
#' ## tabulate odds
#' tabOdds(abo, study, by = state)
#'
#' ## change case value to "Yes"
#' tabOdds(abo, study, by = state, case_value = "Yes")
#'
#' ## without plot
#' tabOdds(abo, study, by = state, case_value = "Yes", plot = FALSE)
#' }
#'
#' @export
tabOdds <- function(data, ... , by, exp_value = NULL, case_value = NULL, plot = TRUE,
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
        tabodds(.data, z, by, exp_value, case_value, plot, na.rm, rnd)
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

tabodds <- function(.data, .x, .by, exp_value = NULL, case_value = NULL, plot = TRUE,
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

    ## calculate odds
    .odds <- .tbl[, 1] / .tbl[, 2]
    .se <- sqrt((1/sum(.tbl[, 1])) + (1/sum(.tbl[, 2])))
    .ll <- .odds / exp(1.96 * .se)
    .ul <- .odds * exp(1.96 * .se)


    ## create data frame
    .df <- data.frame(x.name = rownames(.tbl), "|" = "|",
                      .tbl,
                      Odds = cbind(sprintf(.odds, fmt = paste0('%#.', rnd, 'f')),
                                   sprintf(.ll, fmt = paste0('%#.', rnd, 'f')),
                                   sprintf(.ul, fmt = paste0('%#.', rnd, 'f'))),
                      stringsAsFactors = FALSE
    )
    names(.df)[1] <- .x.name
    names(.df)[c(2, 5:7)] <- c("|", "Odds", "[95% Conf.", "Interval]")

    ## separate total row
    .df.total <- .df["Total", ]

    ## add dash lines
    .df <- addDashLines(.df[-nrow(.df), ], .vLine = 2)

    ## add total row
    .df <- rbind(.df, .df.total)

    row.names(.df) <- NULL


    ## plotting
    if (plot) {
        names(.odds) <- row.names(.tbl)
        plotRisks(.odds, .ll, .ul, .x.name, .by.name, "Odds")
    }

    return(.df)
}
