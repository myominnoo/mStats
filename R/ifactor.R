#' Factoring several variables simultaenously
#'
#' @description
#' The function ifactor() is used to encode several variables into factors into
#' pre-specified lables and replace the variables from the data frame
#'
#' @param x An R Object: This must be a numeric vector indicating the poistions
#' of each variable
#' @param data An R Oject to specify dataset. This is not optional.
#' @param lbl.list A list Object containing labels as strings. This must correspond
#' to the levels in each variable.
#' @seealso itab, igroup, isum
#' @keywords summarize, isum, basic statistics, quick summary
#' @export
#' @examples
#' str(infert)
#' names(infert)
#' lbl <- list(induced = c("1in", "2in", "3+in"),
#'             case = c("Yes", "No"),
#'             spontaneous = c("<1 spon", "2 spon", "3+ spon"))
#' infert1 <- ifactor(c(4, 5, 6), data = infert)
#' infert1 <- ifactor(c(4, 5, 6), data = infert, lbl.list = lbl)
#' str(infert1)
#' isum(infert1)

ifactor <- function(x, data = NULL, lbl.list = NA) {
  if(is.null(data)) {message("... data is missing ...")} else {
    for(i in 1:length(x)) {
      if(is.na(lbl.list[i])) {lbl <- ""} else {lbl <- lbl.list[[i]]}
      data[[x[i]]] <- factor(data[[x[i]]], labels = lbl)
    }
    return(data)
  }
}
