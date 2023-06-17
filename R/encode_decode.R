#' Apply functions to multiple variables in a data frame
#'
#' The `coder()` function applies a set of functions to multiple variables in a data frame using `dplyr`'s `mutate()` and `across()` functions.
#'
#' @param .data The input data frame.
#' @param .fns A set of functions to be applied to the variables in the data frame.
#'
#' @return A modified data frame with the functions applied to the variables.
#'
#'
#' @examples
#' # Apply the `mean()` function to multiple variables in the `mtcars` data frame
#' coder(mtcars, mean)
#'
#' @export
coder <- function(.data, .fns) {
	dplyr::mutate(.data, dplyr::across(dplyr::everything(), .fns = .fns))
}

#' Encode variables in a data frame
#'
#' The `encode()` function encodes selected variables in a data frame. It converts the variables to numeric type, sets labels from the original data, and returns the modified data frame.
#'
#' @param .data The input data frame.
#' @param ... The variables to be encoded.
#'
#' @return A modified data frame with the selected variables encoded.
#'
#' @examples
#' # Encode selected variables in the `mtcars` data frame
#' encode(mtcars, mpg, cyl)
#'
#' @export
encode <- function(.data, ...) {
	df_change <- .data |>
		dplyr::select(...) |>
		# Change to numeric type
		coder(as.numeric) |>
		# Set labels from original data
		labelled::copy_labels_from(from = .data, .strict = FALSE)

	.data |>
		dplyr::select(!dplyr::all_of(names(df_change))) |>
		dplyr::bind_cols(df_change) |>
		dplyr::select(dplyr::all_of(names(.data)))
}

#' Decode variables in a data frame
#'
#' The `decode()` function decodes selected variables in a data frame. It converts the variables to character type, sets labels from the original data, and returns the modified data frame.
#'
#' @param .data The input data frame.
#' @param ... The variables to be decoded.
#'
#' @return A modified data frame with the selected variables decoded.
#'
#' @examples
#' # Decode selected variables in the `mtcars` data frame
#' decode(mtcars, mpg, cyl)
#'
#' @export
decode <- function(.data, ...) {
	df_change <- .data |>
		dplyr::select(...) |>
		# Change to character type
		coder(as.character) |>
		# Set labels from original data
		labelled::copy_labels_from(from = .data, .strict = FALSE)

	.data |>
		dplyr::select(!dplyr::all_of(names(df_change))) |>
		dplyr::bind_cols(df_change) |>
		dplyr::select(dplyr::all_of(names(.data)))
}
