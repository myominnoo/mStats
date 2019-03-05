#' @title Remove Duplicated Elements
#' @description
#' \code{dup.rm} determines which elements of a vector are duplicates and removes them.
#' @param x a numeric object
#' @param na.rm A logical value to specify missing values
#' @details
#' This becomes handy when a return value with duplicates removed is need. It checks if
#' any elements of vector are duplicated and remove them at the same time.
#' @seealso \code{\link{label}}
#' @keywords duplicate, remove duplicate
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' ## Example 1
#' a <- rep(c("A", "B", "C"), c(10, 20, 30))
#' a
#' dup.rm(a)
#'
#' ## Example 2
#' b <- rep(c("A", "B", "C", NA), c(10, 20, 30, 10))
#' b
#' dup.rm(b)
#' dup.rm(b, na.rm = TRUE)

#' @export
dup.rm <- function(x, na.rm = FALSE) {
  x <- x[duplicated(x) == FALSE]
  if (na.rm) x <- x[!is.na(x)]
  return(x)
}




#' @title Label levels of a categorical variable (Value Label)
#' @description
#' \code{label} function is handy to quickly change levels of either a factor, number,
#' logical or character vector.
#' @param x A vector
#' @param labels specify a character vector that correspond to the levels
#' @details
#' This function allows to quickly change vector into factor with defined levels or even
#' levels of a factor into new ones.
#' @seealso \code{\link{dup.rm}}
#' @keywords value label, label levels
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' data(infert)
#' str(infert)
#'
#' ## Example 1: Numeric vector
#' itab(case, data = infert)
#' case <- label(infert$case, c("No", "Yes"))
#' itab(case)
#'
#' ## Example 2: factor
#' itab(education, data = infert)
#' edu <- label(infert$education, c("low", "middle", "high"))
#' itab(edu)
#'
#' ## Example 3: character
#' sex <- rep(c("Male", "Female", NA), c(30, 60, 10))
#' str(sex)
#' itab(sex)
#' sex1 <- label(sex, c('f', 'm'))
#' str(sex1)
#' itab(sex1)
#'
#' ## Example 4: Logical
#' s <- sex == "Male"
#' str(s)
#' itab(s)
#' s1 <- label(s, c("male", "female"))
#' str(s1)
#' itab(s1)

#' @export
label <- function(x, labels)
{
  if (is.numeric(x)) {
    x <- factor(x, labels = labels)
  } else if (is.character(x)) {
    x <- factor(x, labels = labels, ordered = FALSE)
  } else if (is.factor(x)) {
    x <- factor(unclass(x), labels = labels)
  } else if (is.logical(x)) {
    x <- factor(2 - x, labels = labels, ordered = FALSE)
  } else stop('x can not be labelled.')
  return(x)
}

