#' @title Coerces or test vectors to Date type
#'
#' @description
#' \code{toDate} coerces objects to Date type. 
#' \code{is.Date} indicates which vector is type of Date.
#'
#' @param x object to be coerced or tested
#' @param format a character value, representing the function of Date conversion or coercion.
#' \itemize{
#'   \item \strong{"a"} : Year-month-day
#'   \item \strong{"b"} : Year-day-month
#'   \item \strong{"c"} : day-month-Year
#'   \item \strong{"d"} : month-day-Year
#'   \item \strong{"e"} : Microsoft excel number whose origin is set to "1899-12-30".
#' }
#' @seealso \code{\link{dateSum}}
#' @keywords date coerces, date test
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' x <- c("2019-01-15", "2019-01-20", "2019-01-21", "2019-01-22")
#' is.Date(x)
#' toDate(x)
#' y <- toDate(x)
#' is.Date(y)
#' y
#' # dates in numeric format MS excel
#' x <- 42705:42710
#' y <- toDate(x, "e")
#' y

#' @export
toDate <- function(x, format = "a")
{
  switch(format,
         a = as.Date(x, format = "%Y-%m-%d"),
         b = as.Date(x, format = "%Y-%d-%m"),
         c = as.Date(x, format = "%d-%m-%Y"),
         d = as.Date(x, format = "%m-%d-%Y"),
         e = as.Date(x, origin = "1899-12-30"))
}

#' @rdname toDate
#' @export
is.Date <- function(x) {
  return(class(x) == 'Date')
}
