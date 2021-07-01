#' Change contents of an existing variable
#'
#' @description
#'
#' \Sexpr[results=rd]{lifecycle::badge("stable")}
#'
#' `replace()` alters the contents of a variable based on the `condition`
#' you specify. If not specified, all contents of the variable is set to
#' `value`.
#'
#' @inheritParams generate
#' @param condition Conditions can be expressed. By default, it is set to
#' no condition `NULL`.
#'
#' @family data management
#' @export
#' @section Examples:
#'
#' Here we show a few examples of how to replace values in variables.
#'
#' ```
#' ## replacing mpg with standardized values of mpg
#' mtcars <- replace(mtcars, mpg, NA, mpg >= 10 & mpg <= 20)
#' mtcars <- replace(mtcars, mpg, mpg / mean(mpg))
#' codebook(mtcars)
#'
#'
#' ## replacing education levels with one value
#' infert <- replace(infert, education, "6+yrs",
#'         education == "6-11yrs" | education == "12+ yrs")
#' table(infert$education)
#' ```
replace <- function(data, var, value, condition = NULL) {
  data_name <- deparse(substitute(data))
  .check_dataframe(data, data_name)

  args      <- as.list(match.call())
  var_name  <- as.character(args$var)
  var_label <- .get_vars_label(data, var_name)
  .check_vars(data, var_name)
  # if var is a factor, convert to factor again corresponding levels
  check <- is.factor(data[[var_name]])
  if (check) {
    data[[var_name]] <- as.character(data[[var_name]])
  }

  value <- paste0(deparse(substitute(value)), collapse = "")

  expr <- paste0(deparse(substitute(condition)), collapse = "")
  expr <- paste0("[", expr, "]")
  if (expr == "[NULL]") {
    expr <- ""
  }

  expr_text <- paste0(var_name, expr, " <- ", value)
  tryCatch({
    var <- eval(parse(text = paste0("within(data, ", expr_text, ")")))
  } , error = function(msg){
    stop(paste0("\n'", expr_text, "' cannot be evaluated.", "\n"), call. = TRUE)
    return(NA)
  })
  var <- var[[var_name]]

  # if var is a factor, convert to factor again corresponding levels
  if (check) var <- as.factor(var)

  ## assign label back to the var
  num_changed <- data[[var_name]] == var
  num_changed <- sum(!num_changed, na.rm = TRUE) + sum(is.na(num_changed))
  message("  (", num_changed, " values replaced)")

  attr(var, "label") <- ifelse(var_label == "", expr_text, var_label)
  data[[var_name]] <- var

  return(data)
}
