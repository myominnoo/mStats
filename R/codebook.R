#' Describe dataset
#'
#' @description
#' `codebook()` displays a compact overview of the data frame.
#' It will have important characteristics of the data frame including
#' type, label, observation number, and missing values as well as
#' percentage of missing values.
#'
#' @export
#' @param data A data.frame
#'
#' @section Examples:
#'
#' Here we show the usage using the dataset `iris`.
#'
#' ```{r, comment = "#>", collapse = FALSE}
#' codebook(iris)
#' ```
#'
codebook <- function(data) {
  data_name <- deparse(substitute(data))
  .check_dataframe(data, data_name)

  ## get information of data frame
  data_label <- attr(data, "label")

  vars_num   <- ncol(data)
  vars_name  <- names(data)
  vars_label <- .get_vars_label(data, vars_name)
  vars_type  <- .get_vars_type(data, vars_name)

  obs_all    <- nrow(data)
  obs_num    <- sapply(data, function(z) length(z[!is.na(z)]))
  obs_na_num <- sapply(data, function(z) length(z[is.na(z)]))
  obs_na_pct <- sprintf(obs_na_num / obs_all * 100, fmt = paste0("%#.1f"))

  out <- data.frame(1:vars_num, vars_name, vars_label, vars_type, obs_num,
                    obs_na_num, obs_na_pct)
  names(out) <- c("No", "Variable", "Label", "Type", "Obs", "NA", "NA(%)")
  out <- .add_hv_lines(out, 1, 3, "|")

  message("\t      Codebook", "\n",
          "   Dataset's Name : `", data_name, "`", "\n",
          "  Dataset's Label : ", data_label, "\n",
          "\t     Vars : ", vars_num, "\n",
          "\t      Obs : ", obs_all)
  print.data.frame(out, row.names = FALSE, max = 1e9)

  invisible(out)
}
