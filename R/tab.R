#' Tabulate or cross-tabulate
#'
#' @description
#'
#' \Sexpr[results=rd]{lifecycle::badge("stable")}
#'
#' `tab()` generates one-way or two-way tables of summary statistics.
#'
#'
#' ## One-way tabulation
#'
#' If `by` is not specified, one-way table of frequencies is generated.
#'
#' * `Freq.`   - frequency
#' * `Percent` - relative frequency
#' * `Cum.`    - cumulative relative frequency
#'
#' ## Two-way or n-way tabulation
#'
#' Specifying `by` generates two-way or n-way table of summary statistics
#' By default, row percentages are presented with count data.
#'
#' Value of `row.pct`:
#'
#' * `TRUE`  - relative frequency within its row of each cell
#' * `FALSE` - report relative frequency within its column of each cell
#' * `NULL`  - cumulative relative frequency
#'
#' ## Tabulating the whole dataset
#'
#' This is helpful when the dataset has been processed and finalized.
#' The final dataset can be tabulated without specifying any variables.
#' This automatically filters and generates tables of frequencies
#' for variables that are of type `character`, `factor`, `order factor`, or `logical`.
#'
#'
#'
#' @inheritParams codebook
#' @param ... One or more unquoted variables separated by commas.
#' Variable names can be used as if they were positions in the data
#' frame, so expressions like `x:y` can be used to select a range of
#' variables.
#' @param by Stratification Variable
#' @param row.pct This allows you to control how relative frequency are
#' calculated or none at all. The value can be:
#'
#' * `TRUE` reports relative frequency within its row.
#' * `FALSE` reports relative frequency within its column.
#' * `NULL` hides relative frequency.
#'
#' @param na.rm logical value: if `TRUE`, it removes observations with missing values.
#' @param chi2 logical value
#'
#' * `TRUE` calcuates p-value from chi-square test of association.
#' * `FALSE` calculates p-value from Fisher's exact test.
#' * `NULL` hides p-value from either tests.
#'
#' @param digits specify rounding of numbers.
#'
#'
#' @section Examples:
#'
#' Tabulate variable by name:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' tab(infert, education)
#' ```
#'
#' Tabulate multiple variables by separating them with commas:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' tab(infert, education, parity, induced)
#' ```
#'
#' The `:` operator selects a range of consecutive variables:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' tab(infert, education, parity:spontaneous)
#' ```
#'
#' Tabulate variables with a stratification variable `by`:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' tab(infert, education, parity:spontaneous, by = case)
#' ```
#'
#' Tabulate all categorical variables without specifying any variables:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' tab(infert)
#' ```
#'
#' @family statistical analysis
#' @importFrom stats addmargins chisq.test fisher.test na.omit
#' @export
tab <- function(data, ... , by = NULL, row.pct = TRUE, na.rm = FALSE,
                chi2 = TRUE, digits = 1) {
  vars_type <- c("factor", "character", "orderedfactor", "logical")
  data_name <- deparse(substitute(data))
  .check_dataframe(data, data_name)

  args      <- as.list(match.call())
  vars_name <- .dots(args, c("data", "by", "row.pct", "na.rm", "chi2", "digits"))
  vars_name <- .check_dots(data, vars_name)

  ## stop if nothing can be found
  if (length(vars_name) == 0) {
    vars_name <- names(data)
    vars_type <- .get_vars_type(data, vars_name) %in% vars_type
    vars_name <- vars_name[vars_type]
    if (length(vars_name) == 0) {
      stop(paste0(
        "'", data_name, "' does not contain variables for tabulation."
      ), call. = FALSE)
    }
  }

  ## Tabulation
  y_name <- as.character(args$by)
  if (length(y_name) == 0) {
    out <- do.call(rbind, lapply(vars_name, tab1, data, na.rm, digits))
    output <- .format_tab(out)
    message("  One-way tabulation")
  } else {
    .check_vars(data, y_name)
    out <- do.call(
      rbind,
      lapply(vars_name, tab2, data, y_name, row.pct, na.rm, chi2, digits))
    output <- .format_tab(out)
    output <- .add_header(output, c("Variable", "Level", "\\|"), y_name, "Pr")
    message("  Two-way tabulation")
  }

  print.data.frame(output, row.names = FALSE, max = 1e9)
  .print_vars_label(data, c(vars_name, y_name))

  attr(out, "dataname") <- data_name

  invisible(out)
}





# helpers -----------------------------------------------------------------


tab1 <- function(x, data, na.rm = FALSE, digits = 1) {
  x_name <- x
  x      <- data[[x]]

  useNA <- ifelse(na.rm, "no", "ifany")

  ## create tabulation table
  f      <- table(x, useNA = useNA)
  pct    <- prop.table(f) * 100
  cumpct <- sprintf(c(cumsum(pct), 100),
                    fmt = paste0("%#.", digits, "f"))
  pct    <- sprintf(c(pct, 100), fmt = paste0("%#.", digits, "f"))
  ft     <- c(f, Total = sum(f, na.rm = TRUE))

  out        <- data.frame(cbind(names(ft), ft, pct, cumpct))
  names(out) <- c("Level", "Freq.", "Percent", "Cum.")

  row.names(out) <- NULL
  out <- rbind(out[0, ], c(rep("", 4)), out)
  out <- cbind(Variable = c(x_name, rep("", nrow(out) - 1)), out)
  attr(out, "type") <- "tab1"

  return(out)
}

tab2 <- function(x, data, by, row.pct = TRUE, na.rm = FALSE,
                 chi2 = TRUE, digits = 1) {
  x_name <- x
  y_name <- by
  x <- data[[x]]
  y <- data[[by]]

  useNA <- ifelse(na.rm, "no", "ifany")

  ## Create confusion matrices of raw, row and col percentages
  tbl <- table(x, y, useNA = useNA)

  ## Create row and col names of the tables
  row_name <- c(row.names(tbl), "Total")
  col_name <- c(colnames(tbl), "Total")
  row_name[is.na(row_name)] <- "<NA>"

  ## Create count table
  tbl_count <- addmargins(tbl)

  if (is.null(row.pct)) {
    row.names(tbl_count)   <- row_name
    colnames(tbl_count)    <- col_name
    out <- as.data.frame(cbind(row_name, tbl_count))
    names(out) <- c("Level", col_name)
  } else {
    if (row.pct) {

      tbl_pct <- addmargins(prop.table(tbl, 1) * 100)
      tbl_pct[nrow(tbl_pct), ] <-
        tbl_count[nrow(tbl_count), ] / sum(tbl) * 100
      tbl_pct <- as.data.frame(cbind(tbl_pct))
      lapply(1:ncol(tbl_pct), function(z) {
        tbl_pct[, z] <<- sprintf(tbl_pct[, z], fmt = paste0("%#.", digits, "f"))
      })

      pct_name <- c(rbind(col_name, rep("row(%)", length(col_name))))

      ## Order count and pct col
      col_order   <- c(1, order(c(2 * (1:ncol(tbl_count) - 1) + 1,
                                  2 * 1:ncol(tbl_pct))) + 1)

      tbl_count <- as.data.frame(cbind(row_name, tbl_count))
      out <- cbind(tbl_count, tbl_pct)[, col_order]
      names(out) <- c("Level", pct_name)
    } else if (!row.pct) {
      tbl_pct <- addmargins(prop.table(tbl, 2) * 100)
      tbl_pct[, ncol(tbl_pct)] <-
        tbl_count[, ncol(tbl_count)] / sum(tbl) * 100
      tbl_pct <- as.data.frame(cbind(tbl_pct))
      lapply(1:ncol(tbl_pct), function(z) {
        tbl_pct[, z] <<- sprintf(tbl_pct[, z], fmt = paste0("%#.", digits, "f"))
      })

      pct_name <- c(rbind(col_name, rep("col(%)", length(col_name))))

      ## Order count and pct col
      col_order   <- c(1, order(c(2 * (1:ncol(tbl_count) - 1) + 1,
                                  2 * 1:ncol(tbl_pct))) + 1)

      tbl_count <- as.data.frame(cbind(row_name, tbl_count))
      out <- cbind(tbl_count, tbl_pct)[, col_order]
      names(out) <- c("Level", pct_name)
    }
  }


  ## calculate p-values
  ## if need to remove NA, create a data.frame and then omit NA
  if (na.rm) {
    .data <- data.frame(x, y)
    .data <- na.omit(.data)
    x <- .data$x
    y <- .data$y
  }

  ## get pvalue from chi square and fisher tests
  if (!is.null(chi2)) {
    if (chi2) {
      pvalue <- tryCatch({
        suppressWarnings(chisq.test(x, y, correct = FALSE)$p.value)
      }, error = function(cnd) {
        return(NA)
      })
      pvalue <- sprintf(pvalue, fmt = '%#.3f')
      test_name <- "Pr(chi2)"
    } else {
      pvalue <- tryCatch({
        suppressWarnings(fisher.test(x, y, simulate.p.value = TRUE)$p.value)
      }, error = function(cnd) {
        return(NA)
      })
      pvalue <- sprintf(pvalue, fmt = '%#.3f')
      test_name <- "Pr(exact)"
    }
    ## add pvalue back to out
    out$p1 <- c(pvalue[1], rep("", nrow(out) - 1))
    names(out)[ncol(out)] <- test_name
  }

  row.names(out) <- NULL
  out <- rbind(out[0, ], "", out)
  out <- cbind(Variable = c(x_name, rep("", nrow(out) - 1)), out)
  attr(out, "type") <- "tab2"

  return(out)
}
