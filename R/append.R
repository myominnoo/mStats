#' Append datasets
#'
#' @description
#'
#' \Sexpr[results=rd]{lifecycle::badge("stable")}
#'
#' `append()` combines multiple datasets to the end of the first dataset.
#' If variables are not the same or not in the same order, `append()`
#' automatically creates variables and put them into the first dataset.
#'
#' @inheritParams codebook
#' @param ... multiple datasets
#'
#' @section Examples:
#'
#' Mutliple datasets can be appended into `iris`, even if they do not share
#' the same variables.
#'
#' ```
#' ## multiple datasets without the same variables
#' d <- append(iris, infert, cars)
#' des(d)
#' ```
#'
#' @family data management
#' @export
append <- function(data, ... ) {
  data_name <- deparse(substitute(data))
  .check_dataframe(data, data_name)

  args <- as.list(match.call())
  y_name <- args[-c(1:2)]
  x_vars_name <- names(data)
  y_vars_name <- unlist(sapply(y_name, function(z) names(eval(z))))
  y_vars_name <- c(x_vars_name, y_vars_name)

  all_vars_name <- y_vars_name[!duplicated(y_vars_name)]
  data[, setdiff(all_vars_name, x_vars_name)] <- NA
  data <- data[, all_vars_name]

  output <- do.call(
    rbind,
    lapply(y_name, function(z) {
      y <- eval(z)
      vars_diff <- setdiff(all_vars_name, names(y))
      y[, vars_diff] <- NA
      message("  (`", z, "` appended to `", data_name, "`)")
      y[, all_vars_name]
    })
  )

  output <- rbind(data, output)

  return(output)
}
