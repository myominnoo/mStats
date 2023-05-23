
#' Describe data contents
#'
#' @description
#'
#' `r lifecycle::badge('stable')`
#'
#' `codebook()` offers a concise overview of your data,
#' enabling easy examination of its structure, including variables,
#' labels, data types, missing counts, and unique value counts.
#' Simplify your data exploration with `codebook()`.
#'
#' @param data A data frame.
#' @param width Width of output: defaults to the setting of the
#'   `width` [option][pillar_options] (if finite)
#'   or the width of the console.
#'
#' @return `data` original `data` is (invisibly) returned,
#' allowing `codebook()` to be used within a data pipe line.
#' @export
#' @examples
#' codebook(mtcars)
#'
#' @examplesIf rlang::is_installed("nycflights13")
#' codebook(nycflights13::flights)
#'
codebook <- function(data, width = NULL) {
	UseMethod("codebook")
}

#' @export
codebook.default <- function(data, width = NULL) {
	invisible(data)
}

#' @export
codebook.data.frame <- function(data, width = NULL)
{
	if (!is.null(width) && !is.finite(width)) {
		rlang::abort("`width` must be finite.")
	}
	if (is.null(width)) {
		width <- getOption("width")
	} else {
		width <- if (is.finite(width)) width else getOption("width")
	}

	.data_name <- rlang::expr_text(substitute(data))
	.data_name <- ifelse(.data_name == ".", "<Piped Data>", .data_name)
	cli::cat_line(crayon::magenta("$ dataset:", crayon::bold(.data_name)))

	row_n <- nrow(data)
	col_n <- ncol(data)
	cli::cat_line(crayon::magenta("$ Row:", row_n))
	cli::cat_line(crayon::magenta("$ Col:", col_n))

	var_names <- format_pillar_title(names(data))
	var_types <- purrr::map(data, pillar::type_sum) |>
		purrr::map_chr(\(x) paste0("<", format(x), ">")) |>
		format_pillar_title()
	miss <- purrr::map_int(data, \(x) sum(is.na(x)))
	complete <- 1 - (miss / row_n)
	unique <- purrr::map_int(data, \(x) length(unique(x)))

	df <- data.frame(
		`#` = seq_len(col_n),
		Name = var_names,
		Type = var_types,
		Miss = miss,
		Complete = scales::label_comma(accuracy = .01)(complete),
		Unique = unique,
		row.names = NULL,
		check.names = FALSE
	)

	## TODO: to replace with label() function
	var_labels <- labelled::var_label(data, unlist = TRUE)
	var_labels <- ifelse(var_labels == "", "<NULL>", paste0(
		substr(var_labels, 1, width - pillar::get_max_extent(df) - 5), "..."
	))
	df |>
		cbind(label = var_labels) |>
		print.data.frame(right = FALSE, row.names = FALSE)
	invisible(data)
}


format_pillar_title <- function(x)
{
	pillar::new_pillar_title(x) |> format()
}
