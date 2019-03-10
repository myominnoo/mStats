#' @title Coerces or test vectors to Date type
#'
#' @description
#' \code{formatDate} coerces objects to Date type.
#' \code{is.Date} indicates which vector is type of Date.
#'
#' @param x object to be coerced or tested
#' @param format for character vectors:
#' @param sep separator character for date components
#' @param thisCent specify either 2000 or 1900 for two-digit years
#' @details
#' Intuitively, "dmY" represents dd mm YYYY format.
#' In combination with value from \code{sep}, this can change to several date formats.
#' For example, "dmy" + "-" convert to "dd-mm-YYYY" format.
#'
#' See \code{\link{as.Date}} for more date formats.
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
formatDate <- function(x, format = "dmY", sep = "/", thisCent = NULL) {
  if (is.character(x)) {
    f <- paste(paste0("%", unlist(strsplit(format, split = NULL, useBytes = T))),
               collapse = sep)
    x <- as.Date(x, format = f)
    if (!is.null(thisCent)) {
      y <- do.call(rbind, strsplit(as.character(x), split = "-", fixed = TRUE))[,1]
      m <- do.call(rbind, strsplit(as.character(x), split = "-", fixed = TRUE))[,2]
      d <- do.call(rbind, strsplit(as.character(x), split = "-", fixed = TRUE))[,3]
      if (thisCent) {
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
#' \code{year} retrieves year from a Date
#' @param x Date Object
#' @seealso \code{\link{formatDate}}, \code{\link{month}}, \code{\link{day}}
#' @keywords year, date
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' dates <- formatDate(c("2019-01-15", "2019-01-20", "2019-01-21", "2019-01-22"), "Ymd", "-")
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
#' \code{month} retrieves month from a Date
#' @param x Date Object
#' @seealso \code{\link{formatDate}}, \code{\link{year}}, \code{\link{day}}
#' @keywords month, date
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' dates <- formatDate(c("2019-01-15", "2019-01-20", "2019-01-21", "2019-01-22"), "Ymd", "-")
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
#' \code{day} retrieves day from a Date
#' @param x Date Object
#' @seealso \code{\link{formatDate}}, \code{\link{year}}, \code{\link{day}}
#' @keywords day, date
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' dates <- formatDate(c("2019-01-15", "2019-01-20", "2019-01-21", "2019-01-22"), "Ymd", "-")
#' day(dates)

#' @export
day <- function(x)
{
  if (!is.Date(x)) stop("x must be Date.")
  as.numeric(format(x, "%d"))
}
