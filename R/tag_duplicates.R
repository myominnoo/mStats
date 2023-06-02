
#' Tag Duplicates
#'
#' \Sexpr[results=rd]{lifecycle::badge("stable")}
#'
#' This function identifies and tags duplicate observations based on specified variables.
#'
#' @param ... Columns to use for identifying duplicates.
#' @param .indicators logical to return three indicator columns: `.n_`, `.N_`, and `.dup_`.
#'
#' @return A tibble with three columns: `.n_`, `.N_`, and `.dup_`.
#'   - `.n_` represents the running counter within each group of variables,
#'   indicating the number of the current observation.
#'   - `.N_` represents the total number of observations within each group of variables.
#'   - `.dup_` is a logical column indicating
#'   whether the observation is a duplicate (TRUE) or not (FALSE).
#'
#' @details
#' This function mimics the functionality of Stata's `duplicates` command in R.
#' It calculates the number of duplicates and provides a report of duplicates
#' based on the specified variables. The function utilizes the `[n_]` and `[N_]` functions
#' for counting and grouping the observations.
#'
#' @family Data Management
#' @examples
#'
#' library(dplyr)
#'
#' # Example with a custom dataset
#' data <- data.frame(
#'   x = c(1, 1, 2, 2, 3, 4, 4, 5),
#'   y = letters[1:8]
#' )
#'
#' # Identify and tag duplicates based on the "x" variable
#' data %>% mutate(tag_duplicates(x))
#'
#' # Identify and tag duplicates based on multiple variables
#' data %>% mutate(tag_duplicates(x, y))
#'
#' # Identify and tag duplicates based on all variables
#' data %>% mutate(tag_duplicates(everything()))
#'
#' \dontrun{
#' ## STATA example
#' dupxmpl <- haven::read_dta("https://www.stata-press.com/data/r18/dupxmpl.dta")
#' dupxmpl |> mutate(tag_duplicates(everything()))
#' }
#'
#' @export
tag_duplicates <- function(..., .indicators = FALSE)
{
	.n_ <- n_(...)
	.N_ <- N_(...)
	# add this for visible binding - global variables [warning] during package check
	Freq <- surplus <- copies <- observations <- NULL

	vars_name <- as.character(match.call())[-1] |>
		paste(collapse = ", ")
	vars_name <- ifelse(vars_name == "everything()", "all variables", vars_name)
	# if > width - 24, truncate label
	console_width <- abs(getOption("width") - 30)
	vars_name <- ifelse(nchar(vars_name) > console_width,
											paste0(substr(vars_name, 1, console_width), "..."), vars_name)

	# cli::cat_line(crayon::magenta("$ Duplicates in terms of",
	# 															crayon::bold(vars_name)))
	cli::cat_line(crayon::magenta("$", crayon::bold("Report of duplicates")))
	cli::cat_line(crayon::magenta("  in terms of", crayon::italic(vars_name)))

	# calculate surplus
	.n_tab <- table(.n_) |>
		data.frame(row.names = NULL) |>
		dplyr::select(copies = .n_, surplus = Freq)
	# combine surplus with copies and observations
	table(.N_) |>
		data.frame(row.names = NULL) |>
		dplyr::select(copies = .N_, observations = Freq) |>
		dplyr::left_join(.n_tab, by = "copies") |>
		dplyr::mutate(surplus = observations - surplus) |>
		dplyr::mutate(surplus = ifelse(copies == 1, 0, surplus)) |>
		print.data.frame(row.names = FALSE)

	if (.indicators) {
		tibble::tibble(.n_, .N_, .dup_ = as.logical(names(.n_)))
	}
}
