
#' Collect tables from functions of `mStats`
#'
#' @description
#'
#' \Sexpr[results=rd]{lifecycle::badge("experimental")}
#'
#' `collect()` retrieves data from outputs of `mStats` functions and
#' creates a well-formatted output that can be:
#'
#' * exported to `csv` or `xlsx` format for further processing.
#' * wraped in rmarkdown with `flextable` package to produce
#'   publication-ready tables.
#'
#'
#' @param data An output from `mStats`functions
#' @family reporting
#' @section Examples:
#'
#' ## Summary measures for categorical variables
#'
#' Create summary table for frequency measure:
#'
#' ```
#' table1 <- tab(infert, education, parity, case)
#' collect(table1)
#' ```
#'
#' Create summary table stratified by a stratification variable `by`:
#'
#' ```
#' table2 <- tab(infert, education, parity, by = case)
#' collect(table2)
#' ```
#'
#' @export
collect <- function(data) {
  UseMethod("collect", data)
}


# helpers -----------------------------------------------------------------


#' @rdname collect
#' @export
collect.tab1 <- function(data) {
  tbl <- data$table

  ## get variable's label
  vars_name <- tbl$variable[tbl$variable != ""]
  vars_label <- .get_vars_label(data$data, vars_name)
  vars_label <- ifelse(vars_label == "", vars_name, vars_label)

  ## replace NA with missing level
  tbl$level[is.na(tbl$level)] <- "missing"
  ## create a new variable with label and levels
  tbl$Variables <- ifelse(tbl$variable == "", tbl$level, tbl$variable)
  tbl$variable[tbl$variable != ""] <- vars_label
  tbl$`n (%)` <- ifelse(tbl$Freq. != "",
                        paste0(tbl$Freq., " (", tbl$Percent, ")"),
                        tbl$Freq.)
  ## check which row has `Total`
  ttl_chk <- tbl$level == "Total"
  total <- tbl[nrow(tbl), ]
  tbl <- tbl[!ttl_chk, ]
  tbl <- rbind(total, tbl)
  row.names(tbl)[tbl$variable != ""] <- tbl$variable[tbl$variable != ""]
  row.names(tbl)[1] <- "Total"

  # remove other columns
  tbl <- tbl[, !(names(tbl) %in% c("variable", "level", "Freq.",
                                   "Percent", "Cum."))]

  attr(tbl, "vars") <- attr(data$table, "vars")
  attr(tbl, "style") <- attr(data$table, "type")

  return(tbl)
}


#' @rdname collect
#' @export
collect.tab2 <- function(data) {
  tbl <- data$table

  ## get variable's label
  vars_chk <- tbl$variable != ""
  vars_name <- tbl$variable[vars_chk]
  vars_label <- .get_vars_label(data$data, vars_name)
  vars_label <- ifelse(vars_label == "", vars_name, vars_label)


  ## replace NA with missing level
  tbl$level[is.na(tbl$level)] <- "missing"
  ## create a new variable with label and levels
  tbl$Variables <- ifelse(tbl$variable == "", tbl$level, tbl$variable)
  tbl$Variables[vars_chk] <- vars_label

  ## get statistics in the middle
  stats <- tbl[, grep("variable|Variables|level|Pr\\[", names(tbl), invert = TRUE)]
  if (attr(tbl, "pct") == "none") {
    stats <- cbind(Total = stats[, ncol(stats)], stats[, -ncol(stats)])
  } else {
    ycol <- ncol(stats)
    r <- seq(1, ycol, 2)
    p <- r + 1
    z <- do.call(
      data.frame,
      lapply(1:length(r), function(i) {
        rw <- paste0(stats[, r[i]], " (", stats[, p[i]], ")")
        rw[rw == " ()"] <- ""
        rw
      })
    )
    names(z) <- names(stats)[r]
    stats <- cbind(Total = z[, ncol(z)], z[, -ncol(z)])
    names(stats) <- paste0(names(stats), "\n n (%)")
  }

  ## combine all
  out <- cbind(Variables = tbl$Variables, stats)
  ## get p-value column
  if (attr(tbl, "test") != "none") {
    p_name <- names(tbl)[grepl("Pr\\[", names(tbl))]
    pvalue <- tbl[, p_name]
    out <- cbind(out, pvalue)
    names(out)[ncol(out)] <- p_name
  }

  row.names(out) <- row.names(tbl)
  row.names(out)[tbl$variable != ""] <- tbl$variable[tbl$variable != ""]

  ## check which row has `Total`
  ttl_chk <- tbl$level == "Total"
  total <- out[nrow(tbl), ]
  out <- out[!ttl_chk, ]
  out <- rbind(total, out)
  row.names(out)[1] <- "Total"

  attr(out, "test") <- attr(tbl, "test")
  attr(out, "by") <- attr(tbl, "by")
  attr(out, "vars") <- attr(tbl, "vars")
  attr(out, "style") <- attr(tbl, "type")

  return(out)
}
