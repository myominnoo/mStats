#' Tabulate or cross-tabulate
#'
#' @description
#'
#' \Sexpr[results=rd]{lifecycle::badge("stable")}
#'
#' `tab()` attaches a text label to each variable specified.
#' If a label is provided without a variable being specified, it will be
#' attached to the dataset.
#'
#' @inheritParams codebook
#' @param ... One or more unquoted expressions separated by commas.
#' Variable names can be used as if they were positions in the data
#' frame, so expressions like `x:y` can be used to select a range of
#' variables.
#' @param by Stratification Variable
#' @param row.pct This allows you to control how relative frequency are
#' calculated or none at all. The value can be:
#'
#'   * `TRUE` reports relative frequency within its row.
#'   * `FALSE` reports relative frequency within its column.
#'   * `NULL` to hide relative frequency.
#'
#' @param na.rm logical: if `TRUE`, it removes observations with missing values.
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
#' Tabulate all categorical variables without specifying any variables:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' tab(infert)
#' ```
#'
#' Tabulate variables with a stratification variable `by`:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' tab(infert, education, parity:spontaneous, by = case)
#' ```
#'
#'
#' @family statistical analysis
#' @importFrom stats addmargins chisq.test fisher.test na.omit
#' @export
tab <- function(data, ... , by = NULL, row.pct = TRUE, na.rm = FALSE, digits = 1) {
  vars_type <- c("factor", "character", "orderedfactor", "logical")
  data_name <- deparse(substitute(data))
  .check_dataframe(data, data_name)

  args      <- as.list(match.call())
  vars_name <- .enquote(args, c("data", "by", "row.pct", "na.rm", "digits"))
  vars_name <- .unquote(data, vars_name)

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
    out_raw <- do.call(rbind, lapply(vars_name, tab1, data, na.rm, digits))
    out     <- .formatOutput(out_raw)

    message(" One-way tabulation")
  } else {
    .check_vars(data, y_name)
    out_raw <- do.call(rbind, lapply(vars_name, tab2, data, y_name, row.pct, na.rm, digits))
    out     <- .formatOutput(out_raw)

    message(" Two-way tabulation")
    .print_header(out, y_name)
  }

  print.data.frame(out, row.names = FALSE, max = 1e9)
  .print_vars_label(data, c(vars_name, y_name))

  output <- list(data = data, output = out_raw)
  class(output) <- "tab"

  invisible(output)
}


# Helpers -----------------------------------------------------------------

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
tab2 <- function(x, data, by, row.pct = TRUE, na.rm = FALSE, digits = 1) {
  x_name <- x
  y_name <- by
  x <- data[[x]]
  y <- data[[by]]

  useNA <- ifelse(na.rm, "no", "ifany")

  ## Create confusion matrices of raw, row and col percentages
  tbl <- table(x, y, useNA = useNA)
  tbl_count <- addmargins(tbl)

  tbl_pct_row <- addmargins(prop.table(tbl, 1) * 100)
  tbl_pct_row[nrow(tbl_pct_row), ] <-
    tbl_count[nrow(tbl_count), ] / sum(tbl) * 100
  tbl_pct_row <- as.data.frame(cbind(tbl_pct_row))
  lapply(1:ncol(tbl_pct_row), function(z) {
    tbl_pct_row[, z] <<- sprintf(tbl_pct_row[, z], fmt = paste0("%#.", digits, "f"))
  })

  tbl_pct_col <- addmargins(prop.table(tbl, 2) * 100)
  tbl_pct_col[, ncol(tbl_pct_col)] <-
    tbl_count[, ncol(tbl_count)] / sum(tbl) * 100
  tbl_pct_col <- as.data.frame(cbind(tbl_pct_col))
  lapply(1:ncol(tbl_pct_col), function(z) {
    tbl_pct_col[, z] <<- sprintf(tbl_pct_col[, z], fmt = paste0("%#.", digits, "f"))
  })

  ## row and col names of the tables
  row_name <- c(row.names(tbl), "Total")
  col_name <- c(colnames(tbl), "Total")

  row_pct_name <- c(rbind(col_name, rep("r(%)", length(col_name))))
  col_pct_name <- c(rbind(col_name, rep("c(%)", length(col_name))))

  row.names(tbl_count)   <- row_name
  colnames(tbl_count)    <- col_name
  row_name[is.na(row_name)] <- "<NA>"

  row.names(tbl_pct_row) <- row_name
  row.names(tbl_pct_col) <- row_name

  row_order   <- c(1, order(c(2 * (1:ncol(tbl_count) - 1) + 1,
                              2 * 1:ncol(tbl_pct_row))) + 1)

  tbl_count <- as.data.frame(cbind(row.names(tbl_count), tbl_count))
  tbl_pct_row <- cbind(tbl_count, tbl_pct_row)[, row_order]
  names(tbl_pct_row) <- c("Level", row_pct_name)
  tbl_pct_col <- cbind(tbl_count, tbl_pct_col)[, row_order]
  colnames(tbl_pct_col) <- c("Level", col_pct_name)

  ## Choose which table to print and return
  if (is.null(row.pct)) {
    out <- tbl_count
  } else if (row.pct) {
    out <- tbl_pct_row
  } else {
    out <- tbl_pct_col
  }
  row.names(out) <- NULL

  ## calculate p-values
  ## if need to remove NA, create a data.frame and then omit NA
  if (na.rm) {
    .data <- data.frame(x, y)
    .data <- na.omit(.data)
    x <- .data$x
    y <- .data$y
  }

  ## get pvalue from chi square and fisher tests
  pvalue <- tryCatch({
    suppressWarnings(chisq.test(x, y, correct = FALSE)$p.value)
  }, error = function(cnd) {
    return(NA)
  })
  pvalue <- c(
    pvalue,
    tryCatch({
      suppressWarnings(
        fisher.test(x, y, simulate.p.value = TRUE)$p.value)
    }, error = function(cnd) {
      return(NA)
    })
  )
  pvalue <- sprintf(pvalue, fmt = '%#.3f')
  ## add pvalue back to out
  out$p1 <- c(pvalue[1], rep("", nrow(out) - 1))
  out$p2 <- c(pvalue[2], rep("", nrow(out) - 1))
  names(out)[(ncol(out)-1):ncol(out)] <- c("chi2", "exact")


  row.names(out) <- NULL
  out <- rbind(out[0, ], c(rep("", 4)), out)
  out <- cbind(Variable = c(x_name, rep("", nrow(out) - 1)), out)
  attr(out, "type") <- "tab2"

  return(out)
}
