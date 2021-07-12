#' Describe data contents
#'
#' @description
#'
#' \Sexpr[results=rd]{lifecycle::badge("stable")}
#'
#' `codebook()` displays a compact overview of the data frame.
#' It includes important characteristics of data contents including
#' number of variables, variable names, label, type and missing values.
#'
#' @param data A data.frame
#'
#' @section Examples:
#'
#' ```
#' codebook(iris)
#' codebook(infert)
#' ```
#'
#' @family data management
#' @export
codebook <- function(data) {
  data_name  <- deparse(substitute(data))
  data_label <- attr(data, "label")
  data_notes <- attr(data, "notes")
  if (!is.null(data_notes)) {
    if (grepl("EpiData", data_notes[2])) {
      data_notes <- paste0("\t", data_notes, ". ", collapse = "\n")
    } else {
      data_notes <- paste0("\t", data_notes[2]:1, ". ", data_notes[-2],
                           collapse = "\n")
    }
  }

  vars_num   <- ncol(data)
  vars_name  <- names(data)
  vars_label <- .get_vars_label(data, vars_name, 35)
  vars_type  <- .get_vars_type(data, vars_name)

  obs     <- nrow(data)
  na      <- sapply(data, function(z) length(z[is.na(z)]))
  obs_na  <- paste0(na, " / ", obs)

  out        <- data.frame(1:vars_num, vars_name, vars_label, vars_type, obs_na)
  names(out) <- c("#", "variable name", "label", "type", "missing")

  output <- .add_hv_lines(out, 1, 3)
  message("   vars: ", vars_num, "\t\t", data_label, "\n",
          "    obs: ", obs)
  print.data.frame(output, right = FALSE, row.names = FALSE, max = 1e9)
  if (!is.null(data_notes)) {
    message("  notes:\n", data_notes)
  }

  invisible(out)
}



#' @rdname codebook
#' @export
cb <- function(data) {
  codebook(data)
}


#' Describe a summary of data contents
#'
#' @description
#'
#' \Sexpr[results=rd]{lifecycle::badge("stable")}
#'
#' `describe()` displays a summary of data contents.
#' It includes important characteristics including
#' number of variables, variable names, label, number of unique values,
#' missing values, mean, minimum and maximum.
#'
#' @inheritParams codebook
#'
#' @section Examples:
#'
#' ```
#' describe(iris)
#' describe(infert)
#' ```
#'
#' @family data management
#' @export
describe <- function(data) {
  vars_type  <- c("numeric", "double", "integer", "logical")
  data_name <- deparse(substitute(data))
  .check_dataframe(data, data_name)
  data_label <- attr(data, "label")

  vars_name  <- names(data)
  vars_type  <- .get_vars_type(data, vars_name) %in% vars_type
  vars_name  <- vars_name[vars_type]
  if (length(vars_name) == 0) {
    stop(paste0(
      "'", data_name, "' does not contain variables for summary measures."
    ), call. = FALSE)
  }

  data <- data[, vars_name]

  vars_label <- .get_vars_label(data, vars_name, 25)
  vars_num   <- ncol(data)

  obs   <- nrow(data)
  na    <- sapply(data, function(z) length(z[is.na(z)]))
  uni   <- sapply(data, function(z) length(unique(z)))
  stats <- do.call(
    rbind,
    lapply(data, function(z) {
      s <- c(mean(z, na.rm = TRUE), min(z, na.rm = TRUE), max(z, na.rm = TRUE))
      sprintf(s, fmt = '%#.1f')
    })
  )


  out <- data.frame(1:vars_num, vars_name, obs, na, uni, stats, vars_label)
  names(out) <- c("#", "Variable", "Obs", "<NA>", "Unique",
                  "Mean", "Min", "Max", "Label")

  output <- .add_hv_lines(out, 1, 3)
  print.data.frame(output, right = TRUE, row.names = FALSE, max = 1e9)

  invisible(out)
}


#' @rdname describe
#' @export
des <- function(data) {
  describe(data)
}
