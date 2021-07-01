#' Report, tag or drop duplicate observations
#'
#' @description
#'
#' \Sexpr[results=rd]{lifecycle::badge("stable")}
#'
#' `duplicates()` reports, tags or drops duplicate observations.
#' Duplicates are observations that have identical values.
#' They can be searched either on all variables if no `variable` is specified
#' or on specified `variables`.
#'
#' @inheritParams encode
#' @param drop `TRUE` deletes all duplicate observations but the first
#' occurence of each group of duplicated observations. `FALSE` tags all duplicate
#' observations with a new variable `dup_`.
#' @param report `TRUE` reports a table showing the following:
#'
#' * Copies  - Number of observations that occur as one or more copies
#' * Surplus - Indicates that they are the second (third, ...) copy of the
#'  first of each group of duplicates.
#'
#' @section Examples:
#'
#' Here we find duplicate observations based on all variables
#' ```
#' iris1 <- duplicates(iris)
#' head(iris1)
#'
#' ## we remove all duplicates except the first observations.
#' iris2 <- duplicates(iris, drop = TRUE)
#' head(iris2)
#'
#' ## we turn off the report table
#' iris3 <- duplicates(iris, drop = TRUE, report = FALSE)
#' head(iris3)
#'
#' ```
#'
#' Below we find duplicates based on `Species`.
#'
#' ```
#' iris4 <- duplicates(iris, Species)
#' head(iris4)
#'
#'
#' ## we remove all duplicates except the first observations.
#' iris5 <- duplicates(iris, Species, drop = TRUE)
#' head(iris5)
#'
#'
#' ## we turn off the report table
#' iris6 <- duplicates(iris, Species, drop = TRUE, report = FALSE)
#' head(iris6)
#'
#' ```
#'
#' @family data management
#' @export
duplicates <- function(data, ... , drop = FALSE, report = TRUE) {
  vars_type <- c("factor", "character", "orderedfactor", "logical")
  data_name <- deparse(substitute(data))
  .check_dataframe(data, data_name)

  args      <- as.list(match.call())
  vars_name <- .dots(args, c("data", "drop", "report"))
  vars_name <- .check_dots(data, vars_name)

  if (length(vars_name) == 0) {
    vars_name <- names(data)
    txt       <- "all variables"
  } else {
    txt  <- paste(vars_name, collapse = ", ")
  }

  vars <- as.data.frame(data[, vars_name])
  id   <- apply(vars, 1, paste, collapse = " ")
  n_   <- ave(id, id, FUN = seq_along)
  N_   <- n_ <- as.numeric(n_)
  N_   <- sapply(id, function(z) {
    t <- N_[z == id]
    t[length(t)]
  })

  data$dup_ <- as.numeric(N_) - 1
  attr(data$dup_, "label") <- "< # of duplicate obs. >"

  ## create table and use the categories to calculate surplus number
  dup_table      <- table(N_)
  dup_table_name <- as.numeric(names(dup_table))
  dup_non <- sapply(dup_table_name, function(z) {
    dup_id <- id[N_ == z]
    length(dup_id[!duplicated(dup_id)])
  })

  ## create final table for duplication report
  out        <- data.frame(cbind(dup_table_name, dup_table,
                                 dup_table - dup_non))
  names(out) <- c("Copies", "Observations", "Surplus")
  out        <- .add_hv_lines(out, 1, 2)

  if (report) {
    message("  Duplicates in terms of ", txt, "\n",
            "  (Total obs: ", nrow(data), ")")
    print.data.frame(out, row.names = FALSE, max = 1e9)
  }

  ## remove the duplicate observations
  if (drop) {
    check_dup <- n_ == 1
    data <- data[check_dup, ]
    data <- data[, !(names(data) %in% "dup_")]
    message(" (", sum(!check_dup), " observations deleted)")
  }

  return(data)
}
