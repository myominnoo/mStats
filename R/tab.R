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
#' Tabulation is displayed in \code{Freq} (frequency), \code{Percent}
#' (Relative Frequency) and \code{Cum.} (Cumulative Relative frequency).
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
    ## match call arguments
    .args <- as.list(match.call())

    ## copy data to .data
    .data <- data

    ## get names of dataset and headings
    .data_name <- deparse(substitute(data))
    .vars_names <- names(.data)

    ## if input is not a data.frame, stop
    if (!is.data.frame(.data)) {
        stop("`.data` must be a data.frame", call. = FALSE)
    }


    ## get variable names within three dots to search for duplicates
    .vars <- enquotes(.args, c("data", "by", "row.pct", "na.rm", "rnd"))


    ## check colon, and check data types if the whole dataset
    .vars <- checkEnquos(.data, .vars, "tab")

    ## if no variable is available, stop
    if (length(.vars) == 0) {
        stop("No categorical variables are found for tabulation.",
             call. = FALSE)
    }


    ## if by variable is empty, do one-way tabulation
    ## if not, do two-way tabulation
    by <- as.character(.args$by)
    by <- ifelse(length(by) == 0, "NULL", by)
    if (by == "NULL") {
        .df <- lapply(.vars, function(z) {
            checkVarName(.data, z)
            tab1(.data, z, na.rm, rnd)
        })
        .txt <- "One-way Tabulation"
    } else {
        .df <- lapply(.vars, function(z) {
            checkVarName(.data, z)
            tab2(.data, z, by, row.pct, na.rm, rnd)
        })
        .txt <- paste0("   Cross-Tabulation : '", .args$by, "'")
    }

    ## create base df to put all into one big dataframe
    ## get maximum nchar value of all variables' names
    .nchar_var_max <- max(nchar(.vars), na.rm = TRUE)
    .nchar_sub_max <- max(sapply(.df, function(z) {
        max(nchar(z[, 1]), na.rm = TRUE)
    }), na.rm = TRUE)

    .df <- do.call(
        rbind,
        lapply(.df, function(z) {
            .df <- z
            .nrow <- nrow(.df)
            .var_name <- names(.df)[1]
            .df <- .df[-c(1, .nrow-1, .nrow), ]

            ## create data.frame to put all tables together
            .df <- cbind(data.frame(
                Var = c(.var_name, rep("", nrow(.df) - 1))),
                .df
            )
            ## change into consistent headings to combine them
            names(.df)[1:2] <- c(
                paste0("V", paste0(rep("1", .nchar_var_max - 1), collapse = "")),
                paste0("V", paste0(rep("1", .nchar_sub_max - 1), collapse = ""))
            )

            ## add dash lines
            .df <- addDashLines(.df)[-1, ]
            names(.df)[1:2] <- c("Variables", "Category")

            return(.df)
        })
    )

    .df_names <- names(.df)
    ## add dash lines again
    .df <- addDashLines(.df[-nrow(.df), -3], .vline = 3)
    names(.df) <- .df_names

    ## get total row
    if (by == "NULL") {
        .df_t <- tab1(.data, .vars[1], na.rm, rnd)
        .df_t <- cbind("", .df_t[nrow(.df_t), ])
    } else {
        .df_t <- tab2(.data, .vars[1], by, row.pct, na.rm, rnd)
        .df_t <- cbind("", .df_t[nrow(.df_t), ])
    }

    names(.df_t)[1:2] <- c("Variables", "Category")

    ## combine all with total row
    .df <- rbind(.df[-1, ], .df_t)
    .df <- addDashLines(.df[, -3], .vline = 3)
    names(.df) <- names(.df_t)

    ## remove row names
    row.names(.df) <- NULL

    ## add label for further processing
    attr(.df, "label") <- "tabulation"

    ## constructs labels
    ## add label for by: cross-tabulation
    .lbl <- sapply(.vars, function(z) attr(.data[[z]], "label"))

    ## Print tabulation and labels
    printDF(.df, .txt)
    sapply(1:length(.vars), function(z) {
        printLabel(.data, .vars[z])
    })

    ## print label for by variable
    printLabel(.data, .args$by)

    invisible(.df)
}

# Helpers functions -------------------------------------------------------

tab1 <- function(data, x, na.rm = FALSE, rnd = 1)
{
    ## copy data to .data
    .data <- data
    ## create single vector .x
    .x <- .data[[x]]

    ## check NA
    .useNA <- ifelse(na.rm, "no", "ifany")

    ## create tabulation table
    .freq <- table(.x, useNA = .useNA)
    .pct <- sprintf(prop.table(.freq) * 100,
                    fmt = paste0("%#.", rnd, "f"))
    .cumpct <- sprintf(cumsum(.pct),
                       fmt = paste0("%#.", rnd, "f"))
    .freq <- c(.freq, Total = sum(.freq, na.rm = TRUE))

    ## combine all statistics
    .df <- data.frame(cbind(x = names(.freq),
                            Freq =  .freq,
                            Percent = c(.pct, 100),
                            Cum. = c(.cumpct, 100)))
    ## add var name to tabulation table
    names(.df)[1] <- x

    ## add dash lines and remove row names
    .df <- addDashLines(.df, .vline = 2)
    .df <- rbind(.df[-(nrow(.df) - 1), ], .df[nrow(.df)-1, ])
    row.names(.df) <- NULL

    return(.df)
}
tab2 <- function(data, x, by, row.pct = TRUE, na.rm = FALSE, rnd = 1)
{
    ## copy data to .data
    .data <- data
    ## create single vector .x
    .x <- .data[[x]]
    .by <- .data[[by]]

    ## check NA
    .useNA <- ifelse(na.rm, "no", "ifany")

    ## check row percentage condition
    # if TRUE, row percentage
    # If FALSE, coloumn percentage, if NULL, no percentage
    row.pct <- ifelse(is.null(row.pct), "none",
                      ifelse(row.pct, "row",
                             ifelse(!row.pct, "column", NULL)))

    ## create table
    ## add rows and then columns totals
    .tbl <- table(.x, .by, useNA = .useNA)
    .tbl_r <- cbind(.tbl, Total = rowSums(.tbl))
    .tbl_f <- rbind(.tbl_r, Total = colSums(.tbl_r))

    ## get column names
    ## if missing values, replace with <NA>
    .tbl_colname <- colnames(.tbl_f)
    .tbl_colname[is.na(.tbl_colname)] <- "<NA>"

    ## calculate row percentage
    .prop_r <- rbind(prop.table(.tbl, 1), prop.table(colSums(.tbl)))
    .prop_r <- cbind(.prop_r, rowSums(.prop_r))

    ## calculate column percentage
    .prop_c <- cbind(prop.table(.tbl, 2), prop.table(rowSums(.tbl)))
    .prop_c <- rbind(.prop_c, colSums(.prop_c))


    ## create table with row percentages
    .tbl_r <- do.call(
        cbind,
        lapply(1:ncol(.tbl_f), function(z) {
            .dum <- cbind(.tbl_f[, z],
                          sprintf(.prop_r[, z] * 100,
                                  fmt = paste0("%#.", rnd, "f")))
            .dum <- data.frame(.dum)
            colnames(.dum) <- c(.tbl_colname[z], "r(%)")
            .dum
        })
    )

    ## create table with row percentages
    .tbl_c <- do.call(
        cbind,
        lapply(1:ncol(.tbl_f), function(z) {
            .dum <- cbind(.tbl_f[, z],
                          sprintf(.prop_c[, z] * 100,
                                  fmt = paste0("%#.", rnd, "f")))
            .dum <- data.frame(.dum)
            colnames(.dum) <- c(.tbl_colname[z], "c(%)")
            .dum
        })
    )

    ## get corresponding table based on row.pct
    .df <- switch(row.pct,
                  none = .tbl_f,
                  row = .tbl_r,
                  column = .tbl_c)
    ## add levels of variable
    .df <- cbind(data.frame(x = rownames(.tbl_f)), .df)
    names(.df)[1] <- x



    ## calculate p-values
    ## if need to remove NA, create a data.frame and then omit NA
    if (na.rm) {
        .data <- data.frame(x = .x, by = .by)
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
            suppressWarnings(
                fisher.test(.x, .by, simulate.p.value = TRUE)$p.value)
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
    .df <- addDashLines(.df, .vline = 2)
    .df <- rbind(.df[-(nrow(.df) - 1), ], .df[nrow(.df)-1, ])
    row.names(.df) <- NULL

    return(.df)
}
