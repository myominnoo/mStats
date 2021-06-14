#' Label variables and dataset
#'
#' @description
#'
#' \Sexpr[results=rd]{lifecycle::badge("stable")}
#'
#' `label()` generates one-way or two-way tabulation of variables.
#' If no variables are specified, all the variables will be tabulated
#' if they are of `character`, `factor`, `order factor`, and `logical`.
#'
#' @inheritParams codebook
#' @param ... One or more unquoted expressions separated by equal sign `=`
#'
#' @family data management
#' @export
#' @section Examples:
#'
#' Here we show the usage for labeling dataset.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' iris <- label(iris, "Edgar Anderson's Iris Data")
#'
#' codebook(iris)
#' ```
#'
#'
#' Here is how to label multiple variables.
#' ```{r, comment = "#>", collapse = TRUE}
#' iris <- label(iris, Sepal.Length = "Length of Sepal",
#'               Petal.Length = "Length of Petal",
#'               Species = "Type of species")
#'
#' codebook(iris)
#' ```
label <- function(data, ... ) {
  data_name <- deparse(substitute(data))
  .check_dataframe(data, data_name)

  args        <- as.list(match.call())
  vars_labels <- args[-c(1:2)]
  vars_labels <- unlist(vars_labels)
  vars_name   <- names(vars_labels)

  if (length(vars_name) == 1 & vars_name[1] == "") {
    names(vars_labels)  <- data_name
    attr(data, "label") <- vars_labels

    message("  (`", data_name, "` labeled as `", vars_labels, "`)")
  } else {
    .check_vars(data, vars_name)
    lapply(vars_name, function(z) {
      attr(data[, z], "label") <<- vars_labels[[z]]

      message("  (`", z, "` labeled as `", vars_labels[[z]], "`)")
    })
  }

  return(data)
}
