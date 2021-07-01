#' Encode variables into numeric type and vice versa
#'
#' @description
#'
#' \Sexpr[results=rd]{lifecycle::badge("stable")}
#'
#' `encode()` converts variables into `numeric` type.
#'
#' `decode()` converts variables into `character` type.
#'
#' @inheritParams codebook
#' @param ... One or more unquoted variables separated by commas.
#' Variable names can be used as if they were positions in the data
#' frame, so expressions like `x:y` can be used to select a range of
#' variables.
#'
#' @section Examples:
#'
#' ```
#' infert <- encode(infert, education, stratum)
#' codebook(infert)
#'
#' iris <- decode(iris)
#' codebook(iris)
#' ```
#'
#' @family data management
#' @export
encode <- function(data, ...) {
  data_name <- deparse(substitute(data))
  .check_dataframe(data, data_name)

  args <- as.list(match.call())
  vars_name <- .dots(args, c("data"))
  vars_name <- .check_dots(data, vars_name)

  if (length(vars_name) == 0) {
    vars_name <- names(data)
  }

  lapply(vars_name, function(var_name) {
    var <- data[[var_name]]
    var_label <- attr(var, "label")
    data[[var_name]] <<- as.numeric(var)
    attr(data[[var_name]], "label") <<- var_label
    message(paste0("  ('", var_name, "' replaced as `numeric`)"))
  })

  return(data)
}



#' @rdname encode
#' @export
decode <- function(data, ...) {
  data_name <- deparse(substitute(data))
  .check_dataframe(data, data_name)

  args <- as.list(match.call())
  vars_name <- .dots(args, c("data"))
  vars_name <- .check_dots(data, vars_name)

  if (length(vars_name) == 0) {
    vars_name <- names(data)
  }

  lapply(vars_name, function(var_name) {
    var <- data[[var_name]]
    var_label <- attr(var, "label")
    data[[var_name]] <<- as.character(var)
    attr(data[[var_name]], "label") <<- var_label
    message(paste0("  ('", var_name, "' replaced as `character`)"))
  })

  return(data)
}

