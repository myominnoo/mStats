#' Compact publication-ready tables
#'
#' @description
#' `mStats` provides several S3 functions for [summary()] to format the
#' output and prepare for publication-ready tables.
#'
#' @inheritParams base::merge
#' @family reporting
#' @section Examples:
#'
#' ## Summary measures for categorical variables
#'
#' Create summary table for frequency measure:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' table1 <- tab(infert, education, parity)
#'
#' table1_sum <- summary(table1)
#'
#' table2 <- tab(infert, education, parity, by = case)
#'
#' table2_sum <- summary(table2)
#'
#' compact(table1_sum, table2_sum)
#' ```
#'
#' @export
compact <- function(x, y) {
  args   <- as.list(match.call())
  x_type <- attr(x, "type")
  y_type <- attr(y, "type")

  if (any(is.null(x_type) | is.null(x_type))) {
    stop("`x` and `y` must be outputs from `tab()`", call. = FALSE)
  }

  check <- x_type == y_type
  if (check) {
    output <- rbind(x, y[-1, ])
  } else {
    vars_x <- x$Variable[x$Variable != ""]
    vars_y <- y$Variable[y$Variable != ""]

    if (length(vars_x) != length(vars_y)) {
      stop("Variables in `x` and `y` must be equal.", call. = FALSE)
    } else if (any(vars_x != vars_y)) {
      stop("Variables in `x` and `y` must be the same in order.", call. = FALSE)
    }
    if (x_type == "table1") {
      output <- cbind(x, y[, -c(1:3)])
    } else {
      output <- cbind(y, x[, -c(1:3)])
    }
  }

  output
}
