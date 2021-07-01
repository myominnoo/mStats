#' Duplicate observations
#'
#' @description
#'
#' \Sexpr[results=rd]{lifecycle::badge("stable")}
#'
#' `expand2()` duplicates observations to have `n` copies of observations.
#'
#' @inheritParams codebook
#' @param copies `n` copies of observations
#' @param n_n Row index to duplicate. By default, the value is `1:nrow(data)`.
#'
#' @family data management
#' @export
#' @section Examples:
#'
#' ```
#' set.seed(123)
#' test <- data.frame(id = 1:5, age = rnorm(5, 30, 10))
#' test <- expand2(test, copies = 2, n_n = 1:3)
#'
#' ## check the duplicates
#' duplicates(test)
#' ```
expand2 <- function(data, copies = 2, n_n = 1:nrow(data))
{
  data_name <- deparse(substitute(data))
  .check_dataframe(data, data_name)

  data_expand <- do.call(
    rbind,
    lapply(1:(copies - 1), function(z) {
      data[n_n, ]
    })
  )

  out <- rbind(data, data_expand)

  message("  (", nrow(data_expand), " observations created)")

  return(out)
}
