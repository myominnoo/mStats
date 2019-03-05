#' @title Coerces or test vectors to Date type
#'
#' @description
#' \code{formatDate} coerces objects to Date type.
#' \code{is.Date} indicates which vector is type of Date.
#'
#' @param x object to be coerced or tested
#' @param format for character vectors: Intuitively, "dmY" represents dd mm YYYY format.
#' In combination with value from \code{sep}, this can change to several date formats.
#' For example, "dmy" + "-" convert to "dd-mm-YYYY" format.
#' @param sep separator character for date components
#' @seealso \code{\link{dateSum}}
#' @keywords date coerces, date test
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' dates <- c("2019-01-15", "2019-01-20", "2019-01-21", "2019-01-22")
#' is.Date(dates)
#' formatDate(dates, "Ymd", "-")
#' y <- formatDate(dates, "Ymd", "-")
#' is.Date(y)
#' y
#' # dates in numeric format MS excel
#' x <- 42705:42710
#' y <- formatDate(x)
#' y

#' @export
formatDate <- function(x, format = "dmY", sep = "/") {
  if (is.character(x)) {
    f <- paste(paste0("%", unlist(strsplit(format, split = NULL, useBytes = T))),
               collapse = sep)
    as.Date(x, format = f)
  } else if (is.numeric(x)) {
    as.Date(x, origin = "1899-12-30")
  } else {
    stop("x must be a character or numeric.")
  }

}

#' @rdname formatDate
#' @export
is.Date <- function(x) {
  return(class(x) == 'Date')
}
