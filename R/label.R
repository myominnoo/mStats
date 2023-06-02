#' Attach labels to data and variables
#'
#' @description
#'
#' `r lifecycle::badge('stable')`
#'
#' This function manipulates labels.
#' It supports different classes of objects, including default objects, data frames, and other types.
#'
#' @param x The object to which the label will be added or modified.
#' @param label A character string specifying the label to be assigned to the variable.
#'
#' @return The modified object with the updated label.
#'
#' @details
#'
#' When used with `dplyr`'s `[mutate]` function,
#'  this function allows for easy labeling of variables within a data frame.
#'
#' If used with a data frame, the function labels the dataset itself, and
#' the label can be checked using the `[codebook]` function.
#'
#'
#' @examples
#'
#' library(dplyr)
#'
#' iris |>
#' 	 mutate(Species = label(Species, 'Species of iris flower')) |>
#' 	 codebook()
#'
#' iris |>
#' 	 label("Iris dataset") |>
#' 	 codebook()
#'
#' @export
label <- function(x, label = NULL)
{
	UseMethod("label")
}

#' @export
label.default <- function(x, label = NULL)
{
	labelled::var_label(x) <- label
	return(x)
}

#' @export
label.data.frame <- function(x, label = NULL)
{
	attr(x, "label") <- label
	return(x)
}
