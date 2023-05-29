
#' Append datasets
#'
#' \Sexpr[results=rd]{lifecycle::badge("deprecated")}
#'
#' `append` stacks multiple datasets.
#'
#' @param ... Data frames to combine.
#'
#' @return A data frame
#'
#' @family Data Management
#' @examples
#' append(airquality, mtcars)
#'
#' @export
append <- function(...)
{
	# deprecated function
	lifecycle::deprecate_warn("3.5.0", "append()", "dplyr::bind_rows()")
	tryCatch({
		dplyr::bind_rows(...)
	}, error = function(err) {
		message(err)
	})
}
