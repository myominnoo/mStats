#' Label variables and dataset
#'
#' @description
#'
#' \Sexpr[results=rd]{lifecycle::badge("stable")}
#'
#' `label()` manipulates labels for variables and dataset.
#' If no label is specified, any existing variable label is removed.
#'
#' @inheritParams codebook
#' @param ... One or more expressions:
#' for labeling dataset, one quoted expression.
#'
#' For labeling variables, one or more unquoted expressions for variables
#' and corresponding quoted labels separated by equal sign `=`.
#'
#' `var1` = `"label1"`, `var2` = `"label2"`, etc.
#'
#' @section Examples:
#'
#'
#' ```
#' ## label dataset
#' iris <- label(iris, "Edgar Anderson's Iris Data")
#'
#' ## label variables
#' iris <- label(iris,
#'               Sepal.Length = "Length of Sepal",
#'               Sepal.Width = "Width of Sepal",
#'               Petal.Length = "Length of Petal",
#'               Petal.Width = "Width of Petal",
#'               Species = "Species of flower")
#'
#' codebook(iris)
#' ```
#'
#' @family data management
#' @export
#'
label <- function(data, ... ) {
  data_name <- deparse(substitute(data))
  .check_dataframe(data, data_name)

  args        <- as.list(match.call())
  vars_labels <- args[-c(1:2)]
  vars_labels <- unlist(vars_labels)
  vars_name   <- names(vars_labels)

  if (length(vars_name) == 0) {
    stop("`variables & corresponding labels` must be specified", call. = FALSE)
  } else if (length(vars_name) == 1 & vars_name[1] == "") {
    names(vars_labels)  <- data_name
    attr(data, "label") <- vars_labels

    message("  (`", data_name, "` labeled as `", vars_labels, "`)")
  } else {
    .check_vars(data, vars_name)
    lapply(vars_name, function(z) {
      attr(data[[z]], "label") <<- vars_labels[[z]]

      message("  (`", z, "` labeled as `", vars_labels[[z]], "`)")
    })
  }

  return(data)
}
