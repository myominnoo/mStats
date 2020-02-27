#' @title Format Dates
#'
#' @description
#' \code{formatDate} converts characters or numbers to dates.
#' \code{is.Date} indicates which elements are Dates.
#'
#' @param x a character or numeric object
#' @param format only for character vectors:
#' @param sep separator character for date components
#' @param century specify either 2000 or 1900 for two-digit years
#'
#' @details
#'
#' \code{dmy} represents \code{dd mm YYYY} format.
#' In combination with separators from \code{sep}, this can change to
#' several date formats.
#' For example, \code{dmy} + \code{-} convert to
#' \code{dd-mm-yyyy} format.
#'
#'
#' \strong{Possible conversions}
#'
#' \enumerate{
#'     \item \code{dmy} + \code{-} >>> \code{dd-mm-yyyy}
#'     \item \code{mdy} + \code{-} >>> \code{mm-dd-yyyy}
#'     \item \code{ymd} + \code{-} >>> \code{yyyy-mm-dd}
#'     \item \code{dmy} + \code{/} >>> \code{dd/mm/yyyy}
#'     \item \code{mdy} + \code{/} >>> \code{mm/dd/yyyy}
#'     \item \code{ymd} + \code{/} >>> \code{yyyy/mm/dd}
#' }
#'
#' \strong{Numeric conversions}
#'
#' Origin is set at \code{1899-12-30}.
#'
#' See \code{\link{as.Date}} for more date formats.
#'
#' @seealso
#'
#' \code{\link{generate}}, \code{\link{egen}}, \code{\link{labelVars}}
#'
#' @keywords date conversion
#'
#' @author
#'
#' For any feedback, please contact \code{Myo Minn Oo} via:
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' \dontrun{
#' ## texts in Date format
#' dates <- c("2019-01-15", "2019-01-20", "2019-01-21", "2019-01-22")
#' # check if it is a Date format
#' is.Date(dates)
#'
#' # formatting as Dates
#' formatDate(dates, "Ymd", "-")
#' y <- formatDate(dates, "Ymd", "-")
#'
#' # check if it is a Date format
#' is.Date(y)
#' y
#'
#'
#'
#' ## dates in numeric format MS excel
#' x <- 42705:42710
#' y <- formatDate(x)
#' y
#' }

#' @export
formatDate <- function(x, format = "dmY", sep = "/", century = NULL) {
  if (is.character(x)) {
    f <- paste(paste0("%", unlist(strsplit(format, split = NULL, useBytes = T))),
               collapse = sep)
    x <- as.Date(x, format = f)
    if (!is.null(century)) {
      y <- do.call(rbind, strsplit(as.character(x), split = "-", fixed = TRUE))[,1]
      m <- do.call(rbind, strsplit(as.character(x), split = "-", fixed = TRUE))[,2]
      d <- do.call(rbind, strsplit(as.character(x), split = "-", fixed = TRUE))[,3]
      if (century) {
        y <- (as.numeric(y) %% 100) + 2000
      } else {
        y <- (as.numeric(y) %% 100) + 1900
      }
      x <- as.Date(paste(y, m, d, sep = "-"), format = "%Y-%m-%d")
    }
  } else if (is.numeric(x)) {
    x <- as.Date(x, origin = "1899-12-30")
  } else {
    stop("x must be a character or numeric.")
  }
  return(x)
}

#' @rdname formatDate
#' @export
is.Date <- function(x) {
  return(class(x) == 'Date')
}


#' @title Year from a Date
#'
#' @description
#'
#' \code{year} retrieves year from a Date
#'
#' @param x Date Object
#'
#' @seealso
#'
#' \code{\link{formatDate}}, \code{\link{month}}, \code{\link{day}}
#'
#' @keywords year, date
#'
#' @author
#'
#' For any feedback, please contact \code{Myo Minn Oo} via:
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' dates <- formatDate(c("2019-01-15", "2019-01-20", "2019-01-21", "2019-01-22"),
#'                     "Ymd", "-")
#' year(dates)

#' @export
year <- function(x)
{
  if (!is.Date(x)) stop("x must be Date.")
  as.numeric(format(x, "%Y"))
}





#' @title Month from a Date
#'
#' @description
#'
#' \code{month} retrieves month from a Date
#'
#' @param x Date Object
#'
#' @seealso
#'
#' \code{\link{formatDate}}, \code{\link{year}}, \code{\link{day}}
#'
#' @keywords month, date
#'
#' @author
#'
#' For any feedback, please contact \code{Myo Minn Oo} via:
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' dates <- formatDate(c("2019-01-15", "2019-01-20", "2019-01-21", "2019-01-22"),
#'                     "Ymd", "-")
#' month(dates)

#' @export
month <- function(x)
{
  if (!is.Date(x)) stop("x must be Date.")
  as.numeric(format(x, "%m"))
}



#' @title Day from a Date
#'
#' @description
#'
#' \code{day} retrieves day from a Date
#'
#' @param x Date Object
#'
#' @seealso
#'
#' \code{\link{formatDate}}, \code{\link{year}}, \code{\link{day}}
#'
#' @keywords day, date
#'
#' @author
#'
#' For any feedback, please contact \code{Myo Minn Oo} via:
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' dates <- formatDate(c("2019-01-15", "2019-01-20", "2019-01-21", "2019-01-22"),
#'                     "Ymd", "-")
#' day(dates)

#' @export
day <- function(x)
{
  if (!is.Date(x)) stop("x must be Date.")
  as.numeric(format(x, "%d"))
}
