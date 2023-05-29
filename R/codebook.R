
#' Generate a codebook
#'
#' @description
#'
#' `r lifecycle::badge('stable')`
#'
#' The `codebook` function generates a codebook for the given dataset. It provides a summary
#' of the dataset's structure and characteristics, including variable names, types, missing
#' values, completeness percentages, unique value counts, and variable labels (if available).
#'
#' @param data The dataset for which the codebook is to be generated.
#'
#' @return The input dataset is returned invisibly,
#' allowing `codebook()` to be used within a data pipe line.
#'
#' @family Data Management
#' @examples
#' codebook(mtcars)
#'
#' codebook(iris)
#'
#' labelled::var_label(iris) <- c(
#' 	"sepal length", "sepal width", "petal length",
#' 	"petal width", "species"
#' )
#' codebook(iris)
#'
#' @export
codebook <- function(data) {
	UseMethod("codebook")
}

#' @export
codebook.default <- function(data) {
	pillar::glimpse(data)
	invisible(data)
}

#' @export
codebook.data.frame <- function(data)
{
	.data_name <- rlang::expr_text(substitute(data))
	.data_name <- ifelse(.data_name == ".", "<Piped Data>", .data_name)
	cli::cat_line(crayon::magenta("$ dataset:", crayon::bold(.data_name)))

	row_n <- nrow(data)
	col_n <- ncol(data)
	cli::cat_line(crayon::magenta("$ Row:", row_n))
	cli::cat_line(crayon::magenta("$ Col:", col_n))

	var_names <- pillar::new_pillar_title(names(data)) |> format()
	var_types <- purrr::map(data, pillar::type_sum) |>
		purrr::map_chr(\(x) paste0("<", format(x), ">")) |>
		pillar::new_pillar_title() |>
		format()
	miss <- purrr::map_int(data, \(x) sum(is.na(x)))
	complete <- 1 - (miss / row_n)
	unique <- purrr::map_int(data, \(x) length(unique(x)))

	df <- data.frame(
		name = var_names,
		type = var_types,
		miss = miss,
		complete = scales::label_comma(accuracy = .01)(complete),
		unique = unique,
		row.names = NULL
	)

	## TODO: to replace with label() function
	var_labels <- labelled::var_label(data, unlist = TRUE)
	# var_labels <- ifelse(var_labels == "", "<NULL>", var_labels)
	# compare df width with console width. if < 30, truncate labels
	width_diff <- getOption("width") - sum(pillar::get_extent(df[1, ]))
	if (width_diff < 30)
		var_labels <- ifelse(nchar(var_labels) > width_diff,
												 paste0(substr(var_labels, 1, 10), "..."), var_labels)

	# print tables
	cbind(df, label = var_labels) |>
		data.frame(row.names = NULL) |>
		print.data.frame(right = FALSE, row.names = TRUE)

	# return data invisibly
	invisible(data)
}

