#' @title Tabulation
#'
#' @description
#'
#' \code{tab()} generates one-way or two-way tabulation of variables.
#'
#' @param data Dataset
#' @param ... Variable or multiple variables
#' Colon separator \code{:} can be used to specify multiple variables.
#' @param by Varaiable for cross-tabulation
#' @param row.pct `TRUE`, `FALSE` or `NULL`:
#'
#' \code{TRUE} shows row percentages.
#'
#' \code{FALSE} shows column percentages.
#'
#' \code{NULL} shows no percentages.
#'
#' @param na.rm A logical value to specify missing values,
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#'
#' @details
#'
#' \strong{One-way tabulation}
#'
#' If \code{by} is not specified, \code{tab} generates one-way tabulation of
#' a variable or multiple variables. \code{...} accepts multiple variables and
#' produces corresponding tabulations.
#'
#' Tabulation is displayed in \code{Freq.} (frequency), \code{Percent.}
#' (Relative Frequency) and \code{Cum.Percent.} (Cumulative Relative frequency).
#'
#' \preformatted{tab(data, var1)}
#'
#' \preformatted{tab(data, var1, var2, var3:var5, var10)}
#'
#'
#' \strong{Two-way tabulation}
#'
#' Specifying \code{by} produces two-way tables. P-values from
#' Chi-squared and Fisher's Exact tests are also shown.
#'
#'
#' \strong{Data type}
#'
#' Tabulation of the whole dataset requires variables to be in either of
#' these data types: `character`, `factor`, `order factor`, `logical`.
#'
#'
#' ## if `...` is not specified, tabulation of the whole dataset is produced.
#' \preformatted{tab(data)}
#'
#'
#'
#' \strong{Using colon `:` spearator}
#'
#' Colon separator \code{:} can be used to indicate sequence of variables.
#'
#'
#' \preformatted{tab(data, var1, var2, var3:var5, var10)}
#'
#'
#' @return
#'
#' tabulation as \code{list}
#'
#'
#'
#' @references
#'
#' Betty R. Kirkwood, Jonathan A.C. Sterne (2006, ISBN:978–0–86542–871–3)
#'
#' @import stats
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
#' ## single variable
#' tab(infert, parity)
#'
#' ## multiple variables
#' tab(infert, parity, induced, case:pooled.stratum)
#'
#' ## tabulate the whole dataset
#' tab(infert)
#'
#'
#'
#' ## cross-tabulation
#' tab(infert, parity, by = case)
#' tab(infert, parity, by = case, row.pct = FALSE)
#' tab(infert, parity, by = case, row.pct = NULL)
#'
#'
#' ## multiple variable
#' tab(infert, age, parity:spontaneous, education, by = case)
#'
#' @export
tab <- function(data, ... , by = NULL, row.pct = TRUE, na.rm = FALSE, rnd = 1)
{

    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    ## assign data into .data for further evaluation
    .data <- data


    ## get variable names within three dots to search for duplicates
    .vars <- as.character(enquos(.args, c("data", "by", "row.pct", "na.rm", "rnd")))

    ## check colon, and check data types if the whole dataset
    .vars <- checkEnquos(.data, .vars, .types = "tab")


    ## one-way tabulation
    ## add two-way here
    by <- as.character(.args$by)
    by <- ifelse(length(by) == 0, "NULL", by)
    if (by == "NULL") {
        .df <- lapply(.vars, function(z) {
            tab1(.data, z, na.rm, rnd)
        })
        .tbl.txt <- "One-way Tabulation"
    } else {
        .df <- lapply(.vars, function(z) {
            tab2(.data, z, by, row.pct, na.rm, rnd)
        })
        .tbl.txt <- paste0("Cross-Tabulation by '", .args$by, "'")
    }



    ## constructs labels
    ## add label for by: cross-tabulation
    .lbl <- sapply(.vars, function(z) attr(.data[[z]], "label"))

    ## Print tabulation
    sapply(1:length(.vars), function(z) {
        printText2(.df[[z]], .tbl.txt, .printDF = TRUE)
        if (.lbl[z] != "NULL") {
            printMsg("Labels")
            printMsg(paste0(.vars[z], ": ", .lbl[z]))
        }
    })


    ## print by label
    getnPrintLabel(.data, .args$by)

    invisible(.df)
}




# Helpers -----------------------------------------------------------------


tab1 <- function(data, x, na.rm = FALSE, rnd = 1)
{
    ## assign as .data and .x for further evaluation
    .data <- data
    .x.name <- x
    .x <- data[[x]]


    ## check NA
    .useNA <- ifelse(na.rm, "no", "ifany")

    ## create table
    .tbl <- table(.x, useNA = .useNA)

    .total <- sum(.tbl)
    .df <- data.frame(
        cbind(names(.tbl),
              "|",
              .tbl,
              sprintf(.tbl / .total * 100, fmt = paste0('%#.', rnd, 'f')),
              sprintf(cumsum(.tbl) / .total * 100, fmt = paste0('%#.', rnd, 'f'))),
        stringsAsFactors = FALSE
    )
    names(.df) <- c(.x.name, "|", "Freq.", "Percent.", "Cum.Percent.")

    .df <- addDashLines(.df, .vLine = 2)
    .df[nrow(.df) + 1, ] <- c("Total", "|", .total, 100, 100)
    row.names(.df) <- NULL

    return(.df)
}

tab2 <- function(data, x, by, row.pct = TRUE, na.rm = FALSE, rnd = 1)
{
    ## assign as .data and .x for further evaluation
    .data <- data
    .x.name <- x
    .x <- data[[x]]
    .by.name <- by
    .by <- data[[by]]


    ## check NA
    .useNA <- ifelse(na.rm, "no", "ifany")

    ## check row pct
    # if TRUE, row percentage
    # If FALSE, coloumn percentage, if NULL, no percentage
    row.pct <- ifelse(is.null(row.pct), "none",
                      ifelse(row.pct, "row",
                             ifelse(!row.pct, "column", NULL)))


    ## create tables
    .tbl <- table(.x, .by, useNA = .useNA)
    .tbl.rowSum <- rowSums(.tbl)
    .tbl <- cbind(.tbl, Total = .tbl.rowSum)
    .tbl.colSum <- colSums(.tbl)
    .tbl.all <- rbind(.tbl, Total = .tbl.colSum)
    colnames(.tbl.all) <- c(paste0(.by.name, "_", colnames(.tbl)[-ncol(.tbl)]),
                            "Total")


    ## get column percentages and add to .tbl.all
    .tbl.col <- .tbl.all
    for (i in 1:ncol(.tbl.all)) {
        .tbl.col[, i] <-  sprintf(.tbl.all[, i] / .tbl.all[nrow(.tbl.all), i] * 100,
                                  fmt = paste0('%#.', rnd, 'f'))
    }
    .tbl.col <- do.call(cbind,
                        lapply(1:ncol(.tbl.all), function(z) {
                            cbind(.tbl.all[, z], .tbl.col[, z])
                        }))


    ## get row percentages and add to .tbl.all
    .tbl.row <- do.call(rbind,
                        lapply(1:nrow(.tbl.all), function(z) {
                            .row <- unlist(.tbl.all[z, ])
                            .total <- .row[length(.row)]
                            sprintf(.row / .total * 100,
                                    fmt = paste0('%#.', rnd, 'f'))
                        }))
    .tbl.row <- do.call(cbind,
                        lapply(1:ncol(.tbl.all), function(z) {
                            cbind(.tbl.all[, z], .tbl.row[, z])
                        }))


    ## add variable category
    .tbl.all <- data.frame(row.names(.tbl.all), "|", .tbl.all,
                           stringsAsFactors = FALSE)
    .tbl.col <- data.frame(row.names(.tbl.all), "|", .tbl.col,
                           stringsAsFactors = FALSE)
    .tbl.row <- data.frame(row.names(.tbl.all), "|", .tbl.row,
                           stringsAsFactors = FALSE)

    ## add headers
    names(.tbl.all)[1:2] <- names(.tbl.col)[1:2] <-
        names(.tbl.row)[1:2] <- c(.x.name, "|")


    colnames(.tbl.col)[3:ncol(.tbl.col)] <- c(
        do.call(c, lapply(colnames(.tbl)[-ncol(.tbl)], function(z) c(z, "(c%)"))),
        "Total", "(%)"
    )

    colnames(.tbl.row)[3:ncol(.tbl.row)] <- c(
        do.call(c, lapply(colnames(.tbl)[-ncol(.tbl)], function(z) c(z, "(r%)"))),
        "Total", "(%)"
    )


    .df <- switch(row.pct,
                  none = .tbl.all,
                  row = .tbl.row,
                  column = .tbl.col)

    ## add p-values
    if (na.rm) {
        .data <- data.frame(x = .x, by = .by, stringsAsFactors = FALSE)
        .data <- na.omit(.data)
        .x <- .data$x
        .by <- .data$by
    }


    ## get pvalue from chi square and fisher tests
    pvalue <- tryCatch({
        suppressWarnings(chisq.test(.x, .by, correct = FALSE)$p.value)
    }, error = function(cnd) {
        return(NA)
    })

    pvalue <- c(
        pvalue,
        tryCatch({
            suppressWarnings(fisher.test(.x, .by, simulate.p.value = FALSE)$p.value)
        }, error = function(cnd) {
            return(NA)
        })
    )

    pvalue <- sprintf(pvalue, fmt = '%#.3f')

    ## add pvalue back to .df
    .df$p1 <- c(pvalue[1], rep("", nrow(.df) - 1))
    .df$p2 <- c(pvalue[2], rep("", nrow(.df) - 1))
    names(.df)[(ncol(.df)-1):ncol(.df)] <- c("ChiSquare", "Exact")


    ## get total line
    .df.total <- .df[nrow(.df), ]

    ## add dash lines
    .df <- addDashLines(.df[-nrow(.df), ], .vLine = 2)
    .df <- rbind(.df, .df.total)
    row.names(.df) <- NULL

    return(.df)
}
