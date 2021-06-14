
#' Summary tables for publication
#'
#' @name summary
#' @rdname summary
#'
#' @description
#' `mStats` provides S3 functions for [summary()] to format the
#' outputs from `tab()` and prepare for publication-ready tables.
#'
#' @details
#'
#' `summary()` supports the following functions of `mStats`:
#'
#' to add more
#'
#' @inheritParams base::summary
#' @family reporting
#' @section Examples:
#'
#' ## Summary measures for categorical variables
#'
#' Create summary table for frequency measure:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' table1 <- tab(infert, education, parity, case)
#'
#' summary(table1)
#' ```
#'
#' Create summary table stratified by a stratification variable `by`:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' table2 <- tab(infert, education, parity, by = case)
#'
#' summary(table2)
#' ```
#'
NULL


#' @rdname summary
#'
#' @inheritParams base::summary
#' @export
summary.tab <- function(object, ... ) {
  data    <- object$data
  output  <- object$output
  type    <- attr(output, "type")

  check_vars <- output$Variable != ""
  vars_name  <- output$Variable[check_vars]
  vars_label <- .get_vars_label(data, vars_name)
  vars_label[vars_label == ""] <- vars_name[vars_label == ""]
  output$Variable[check_vars] <- vars_label

  check_total  <- grepl("Total", output$Level)
  output       <- rbind(output[which(check_total)[1], ], output[!check_total, ])
  output[1, 1:2] <- c("Total", "")

  output <- switch(
    type,
    tab1 = {
      output$Freq. <- paste0(output$Freq., " (", output$Percent, ")")
      output$Freq.[output$Percent == ""] <- ""
      output <- output[, c("Variable", "Level", "Freq.")]
      names(output)[3] <- "Total\n n (%)"

      attr(output, "type") <- "table1"
      output
    },
    tab2 = {
      col_out <- ncol(output)
      y       <- output[, 3:(col_out) - 2]
      col_y   <- ncol(y)

      r <- seq(3, col_y, 2)
      p <- seq(4, col_y, 2)
      z <- do.call(
        data.frame,
        lapply(1:length(r), function(z) {
          paste0(y[, r[z]], " (", y[, p[z]], ")")
        })
      )
      col_z <- ncol(z)
      z     <- rbind(z[0, ], cbind(z[, col_z], z[, -col_z]))
      names(z) <- paste0(
        names(output)[c(r[length(r)], r[-length(r)])], "\n n (%)"
      )
      z[z$`Total\n n (%)` == " ()", ] <- ""

      pvalue <- rbind(output[-1, c(col_out - 1, col_out)], "")
      names(pvalue) <- c("p-value\n (Chi2)", "p-value\n (Exact)")
      output <- cbind(output[, 1:2], z, pvalue)

      attr(output, "type") <- "table2"
      output
    }
  )

  return(output)
}
