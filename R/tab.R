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
#' Value of `pct`:
#'
#' * `row`  - relative frequency within its row of each cell
#' * `col` - report relative frequency within its column of each cell
#' * `none`  - cumulative relative frequency
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
#' @param pct This allows you to control how relative frequency are
#' calculated or none at all. The value can be:
#'
#' * `row` reports relative frequency within its row.
#' * `col` reports relative frequency within its column.
#' * `none` hides relative frequency.
#'
#' @param na.rm logical value: if `TRUE`, it removes observations with missing values.
#' @param test name of test
#'
#' * `chi` calcuates p-value from chi-square test of association.
#' * `exact` calculates p-value from Fisher's exact test.
#' * `none` hides p-value from either tests.
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
tab <- function(data, ... , by = NULL, pct = c("row", "col", "none"),
                na.rm = FALSE, test = c("chi", "exact", "none"), digits = 1) {
  vars_type <- c("factor", "character", "orderedfactor", "logical")
  data_name <- deparse(substitute(data))
  .check_dataframe(data, data_name)

  args      <- as.list(match.call())
  vars_name <- .dots(args, c("data", "by", "pct", "na.rm", "test", "digits"))
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
      lapply(vars_name, tab2, data, y_name, pct, na.rm, test, digits))
    output <- .format_tab(out)
    message("  Two-way tabulation")
    .print_header(output, y_name)
  }

  print.data.frame(output, row.names = FALSE, max = 1e9)
  .print_vars_label(data, c(vars_name, y_name))

  attr(out, "vars") <- vars_name
  attr(out, "dataname") <- data_name
  out <- list(table = out, data = data)
  class(out) <- attr(out$table, "type")

  invisible(out)
}




# helpers -----------------------------------------------------------------



tab1 <- function(x, data, na.rm = FALSE, digits = 1) {
  x_name <- x
  x      <- data[[x]]

  useNA <- ifelse(na.rm, "no", "ifany")

  ## create tabulation table
  f      <- base::table(x, useNA = useNA)
  pct    <- prop.table(f) * 100
  cumpct <- sprintf(c(cumsum(pct), 100),
                    fmt = paste0("%#.", digits, "f"))
  pct    <- sprintf(c(pct, 100), fmt = paste0("%#.", digits, "f"))
  ft     <- c(f, Total = sum(f, na.rm = TRUE))

  out        <- data.frame(cbind(names(ft), ft, pct, cumpct))
  names(out) <- c("level", "Freq.", "Percent", "Cum.")

  ## add row names
  row.names(out) <- paste0(x_name, ".", out$level)
  out <- rbind(out[0, ], c(rep("", 4)), out)
  out <- cbind(variable = c(x_name, rep("", nrow(out) - 1)), out)
  attr(out, "type") <- "tab1"

  return(out)
}

tab2 <- function(x, data, by, pct, na.rm, test, digits) {
  x_name <- x
  y_name <- by
  x <- data[[x]]
  y <- data[[by]]

  useNA <- ifelse(na.rm, "no", "ifany")

  ## Create confusion matrices of raw, row and col percentages
  tbl <- base::table(x, y, useNA = useNA)

  ## Create row and col names of the tables
  row_name <- c(row.names(tbl), "Total")
  col_name <- c(colnames(tbl), "Total")
  row_name[is.na(row_name)] <- "<NA>"

  ## Create count table
  tbl_count <- addmargins(tbl)

  if (length(pct) > 1) pct <- pct[1]
  if (pct == "none") {
    row.names(tbl_count)   <- row_name
    colnames(tbl_count)    <- col_name
    out <- as.data.frame(cbind(row_name, tbl_count))
    names(out) <- c("level", col_name)
  } else {
    if (pct == "row") {

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
      names(out) <- c("level", pct_name)
    } else if (pct == "col") {
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
      names(out) <- c("level", pct_name)
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
  if (length(test) > 1) test <- test[1]
  if (test != "none") {
    if (test == "chi") {
      pvalue <- tryCatch({
        suppressWarnings(chisq.test(x, y, correct = FALSE)$p.value)
      }, error = function(cnd) {
        return(NA)
      })
      pvalue <- sprintf(pvalue, fmt = '%#.3f')
      test_name <- "Pr[chi2]"
    } else if (test == "exact") {
      pvalue <- tryCatch({
        suppressWarnings(fisher.test(x, y, simulate.p.value = TRUE)$p.value)
      }, error = function(cnd) {
        return(NA)
      })
      pvalue <- sprintf(pvalue, fmt = '%#.3f')
      test_name <- "Pr[exact]"
    }

    ## add pvalue back to out
    out$p1 <- c(pvalue[1], rep("", nrow(out) - 1))
    names(out)[ncol(out)] <- test_name
  }

  out <- rbind(out[0, ], "", out)
  row.names(out) <- paste0(x_name, ".", out$level)
  out <- cbind(variable = c(x_name, rep("", nrow(out) - 1)), out)

  attr(out, "pct")  <- pct
  attr(out, "test") <- test
  attr(out, "by")   <- y_name
  attr(out, "type") <- "tab2"

  return(out)
}

