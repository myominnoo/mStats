#' @title Creates or Coerces Date Vectors
#'
#' @description
#' Creates or coerces objects of type "Date". is.Date is a general test of an object being interpretable as Dates.
#'
#' @param x object to be coerced or tested.
#' @param format a character value, representing the function of Date conversion or coercion.
#'
#' "a" - "%Y-%m-%d"
#'
#' "b" - "%Y-%d-%m"
#'
#' "c" - "%d-%m-%Y"
#'
#' "d" - "%m-%d-%Y"
#'
#' "e" - MS excel number format to Date: origin is set to "1899-12-30".
#'
#' @examples
#' x <- c("2019-01-15", "2019-01-20", "2019-01-21", "2019-01-22")
#' is.Date(x)
#' to.Date(x)
#' y <- to.Date(x)
#' is.Date(y)
#' y
#' # dates in numeric format MS excel
#' x <- 42705:42710
#' y <- to.Date(x, "e")
#' y

#' @export
is.Date <- function(x) {
  class(x) == "Date"
}

#' @rdname is.Date
#' @export
to.Date <- function(x, format = "a")
{
  switch(format,
         a = as.Date(x, format = "%Y-%m-%d"),
         b = as.Date(x, format = "%Y-%d-%m"),
         c = as.Date(x, format = "%d-%m-%Y"),
         d = as.Date(x, format = "%m-%d-%Y"),
         e = as.Date(x, origin = "1899-12-30"))
}
