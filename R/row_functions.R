#' Functions to manipulate rows
#'
#' @description
#'
#' \Sexpr[results=rd]{lifecycle::badge("stable")}
#'
#' `rowmiss()` gives the number of `missing` values in specified variables
#' for each observation (`row`).
#'
#' @param ... One or more unquoted variables separated by commas. `x:y` pattern
#' cannot be used.
#'
#' @importFrom stats ave
#'
#' @family data management
#' @export
#' @section Examples:
#'
#' ```
#' d <- data.frame(x = c(rep(1:3, each = 10), rep(NA, 10)),
#'                 y = rep(1:2, 20),
#'                 z = rep(c('male', 'female', 'missing', NA), each = 10))
#'
#' with(d, rowmiss(x, z))
#' with(d, rowmiss())
#' ```
#'
rowmiss <- function(...) {
  vars <- list(...)
  args <- as.character(match.call())[-1]
  if (length(vars) == 0) {
    vars_name <- ls(envir = sys.frame(-1))
    vars <- sapply(sapply(vars_name, as.symbol), eval, envir = sys.frame(-1))
    txt  <- "all variables"
  } else {
    vars_name <- args
    txt  <- paste(args, collapse = ", ")
  }
  data <- as.data.frame(vars, col.names = args)
  out <- apply(data, 1, function(r) sum(is.na(r)), simplify = TRUE)

  return(out)
}
