#' Count from `n` to `N`
#'
#' \Sexpr[results=rd]{lifecycle::badge("stable")}
#'
#' `n_()` and `N_()` are functions used for indexing observations or
#' generating sequences of numbers.
#'
#' - `n_()` generates a running counter within a group of variables and
#' represents the number of the current observation.
#' - `N_()` provides the total count within each group of variables.
#'
#' @inheritParams dplyr::pick
#'
#' @return A numeric vector representing the count from `n` to `N`.
#'
#' @family Data Management
#' @examples
#'
#' # Example with a custom dataset
#' df <- data.frame(
#'   x = c(1, 1, 2, 2, 2, 3, 4, 4, 4, 4),
#'   y = letters[1:10]
#' )
#'
#' library(dplyr)
#'
#' # Generate a running counter for each observation within the "x" group using mutate()
#' mutate(df, n = n_(x))
#'
#' # Generate a running counter for each observation for all columns using mutate()
#' mutate(df, n = n_(everything()))
#'
#' # Generate the total count of observations using summarise()
#' reframe(df, n = n_(x))
#'
#' # Generate the total count of observations within the "x" group using summarise()
#' mutate(df, N = N_(everything()))
#' mutate(df, N = N_(x))
#' reframe(df, N = N_(x))
#'
#' # iris dataset
#' mutate(iris, n = n_(everything()))
#' mutate(iris, N = N_(everything()))
#' @name count_functions
NULL


#' @rdname count_functions
#' @export
n_ <- function(...)
{
	vec <- dplyr::pick(...) |>
		tidyr::unite("x", ...) |>
		dplyr::pull("x")
	x <- stats::ave(1:length(vec), vec, FUN = base::seq_along)
	names(x) <- base::duplicated(vec)
	return(x)
}


#' @rdname count_functions
#' @export
N_ <- function(...)
{
	vec <- dplyr::pick(...) |>
		tidyr::unite("x", ...) |>
		dplyr::pull("x")
	x <- stats::ave(1:length(vec), vec, FUN = base::length)
	names(x) <- base::duplicated(vec)
	return(x)
}
