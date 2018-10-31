#' A Quick Summary function with Graph Display
#'
#' @description
#' isum() lets you do a quick summary on your data
#'
#' For the class "factor", it tabulates and gives frequency, percentages and
#'     cummulative percentages. The same is with the class "character".
#'
#' For the class "integer", it provides seven summary measures:
#'     mean, standard deviation, median, Q1, Q3, minimum and maximum.
#'
#' For the class "data.frame", it gives a list of all the variables based on
#'     their data types.
#'
#' @param ... Additional arguments
#' @seealso isum.factor, isum.numeric, isum.data.frame
#' @keywords summarize, isum, basic statistics, quick summary
#' @export
#' @examples
#' isum(iris)

isum.data.frame <- function(x, rnd = 1, na.rm = TRUE) {
  x <- data.frame(x)
  # create logical vector for date type
  v_date <- unlist(lapply(names(x), function(y) return(ifelse(is.date(x[,y]),
                                                              TRUE, FALSE))))
  v_num <- unlist(lapply(names(x), function(y) return(ifelse(is.numeric(x[,y]),
                                                             TRUE, FALSE))))
  v_freq <- !v_date & !v_num

  num <- do.call(rbind,
         lapply(names(x)[v_num],
                function(y) isum(x[,y], rnd, na.rm,
                                 plot.title = toString(y))))
  row.names(num) <- names(x)[v_num]
  freq <- lapply(names(x)[v_freq],
                 function(y) isum(x[,y], rnd, na.rm,
                                  plot.title = toString(y)))
  freq <- structure(freq, names = names(x)[v_freq])
  return(list(Num.summary = num,
    Freq.summary = freq,
    Additional.Info = paste0("... '", ncol(x[,v_date]),
                             "' variable(s) are of type date ...")))
}
