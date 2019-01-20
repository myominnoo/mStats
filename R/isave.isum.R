#' @title Saving outputs from isum function
#'
#' @description
#' isave.isum is a specific function for the purpose of saving outputs to spreadsheet in
#' comma-separated value (CSV) format.
#'
#' @param x An R object, containing the outputs of isum function
#' @param file A character string naming a file
#' @param append logical. Only relevant if file is a character string. If TRUE, the output
#'  is appended to the file. If FALSE, any existing file of the name is destroyed.
#' @seealso ixtab, igroup
#' @keywords summarize, isum, quick outputs, write CSV
#' @examples
#' result <- isum(infert)
#' isave.isum(result, "mysave.csv")
#' isave.csv(result, "mysave.csv", append = TRUE)
#'
#'
#' result <- isum(iris)
#' isave.csv(result, "mysave.csv")
#' isave.csv(result, "mysave.csv", append = TRUE)

#' @export
isave.isum <- function(x, file, append = FALSE) {
  oldw <- getOption("warn")
  options(warn = -1)
  blank <- c("", "")
  write.table(blank, file, sep = ",", row.names = FALSE, col.names = FALSE,
              append = append)
  for (i in 1:length(x)) {
    write.table(names(x[i]), file, col.names = FALSE, row.names = FALSE, sep = ",",
                append = TRUE)
    write.table(x[[i]], file, col.names = TRUE, row.names = TRUE, sep = ",",
                append = TRUE)
    write.table(blank, file, sep = ",", row.names = FALSE, col.names = FALSE,
                append = TRUE)
  }
  options(warn = oldw)
}
