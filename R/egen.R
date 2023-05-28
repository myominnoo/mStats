#' Convert a continuous variable into groups
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated because we realized that it's
#' a special case of the \code{\link{cut}} function.
#'
#'
#' @param data data.frame
#' @param var existing variable
#' @param at either a number or a numeric vector
#' @param label Labels for the groups
#' @param new_var Name of the new variable
#' @param ... Additional arguments to be passed to \code{\link{cut}}
#'
#' @export
#' @examples
#' data <- data.frame(x = 1:10)
#' egen(data, x, at = c(3, 7), label = c("low", "medium", "high"))
#' egen(data, x, at = c(3, 7), label = c("low", "medium", "high"), new_var = "group")
#'
#'
#' @importFrom rlang :=
#'
#' @seealso
#' \code{\link{cut}}
#'
#' @keywords deprecated
#' @family data manipulation
egen <- function(data, var, at = NULL, label = NULL, new_var = NULL, ...) {
	# deprecated function
	lifecycle::deprecate_warn("3.4.0", "egen()", "mStats::cut()")

	new_var <- rlang::expr_name(substitute(new_var))
	if (new_var == "NULL") {
		data <- dplyr::mutate(data, {{var}} := cut({{var}}, at, label, ...))
	} else {
		data <- dplyr::mutate(data, {{new_var}} := cut({{var}}, at, label, ...))
	}
	return(data)
}
