#' Create new variable
#'
#' @description
#'
#' \Sexpr[results=rd]{lifecycle::badge("stable")}
#'
#' `generate()` creates a new variable.  The contents of the variable are
#' specified by `value`. If `value` is not specified, it is
#' set to `NA`. The `value` is attached as [label] of the `new` variable.
#'
#' @inheritParams codebook
#' @param var Name of the new variable
#' @param value Content of the new variable
#'
#' @family data management
#' @export
#' @section Examples:
#'
#' Here we generate a new variable `age_cat`.
#' The value is derived from age using [ifelse].
#' ```
#' infert <- generate(infert, age_cat, ifelse(age >= 40, "40+", "<40"))
#' codebook(infert)
#' ```
generate <- function(data, var, value = NA) {
  data_name <- deparse(substitute(data))
  .check_dataframe(data, data_name)

  args     <- as.list(match.call())
  var_name <- as.character(args$var)
  # ## if variable already exisits, then stop
  if (var_name %in% names(data)) {
    stop(paste0("'", var_name, "' already defined."), call. = FALSE)
  }

  expr <- paste0(deparse(substitute(value)), collapse = "")
  tryCatch({
    data$var <- eval(parse(text = paste0("with(data, ", expr, ")")))
  }, error = function(msg){
    stop(paste0("\n'", expr, "' cannot be evaluated.", "\n"), call. = TRUE)
    return(NA)
  })
  attr(data$var, "label") <- expr

  na_num  <- sum(is.na(data$var))
  row_num <- length(data$var)
  if (any(na_num)) {
    message("  (", row_num - na_num, " valid & ", na_num, " missing values generated)")
  } else {
    message("  (", row_num - na_num, " valid values generated)")
  }

  names(data)[ncol(data)] <- var_name
  return(data)
}
