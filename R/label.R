#' @title Label levels of a categorical variable (Value Label)
#' @description
#' \code{label} function is handy to quickly change levels of either a factor, number,
#' logical or character vector.
#' @param x A vector
#' @param labels specify a character vector that correspond to the levels
#' @details
#' This function allows to quickly change vector into factor with defined levels or even
#' levels of a factor into new ones.
#'
#' \strong{Numeric or factor}
#' For numeric or factor vector, levels takes the order from the output of
#' \code{\link{table}} function.
#'
#' \strong{Character}
#' For character, it takes alphabetical order and the levels have to be specified accordingly.
#'
#' \strong{Logical}
#' TRUE means 1 and FALSE 2 in a logical vector. Hence, levels shoud be corresponding to the
#' numeric representation of the vector.
#' @seealso \code{\link{gen}}
#' @keywords value label, label levels
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' str(infert)
#'
#' ## Example 1: Numeric vector
#' table(infert$case)
#' case <- label(infert$case, c("No", "Yes"))
#' table(case)
#'
#' ## Example 2: factor
#' table(infert$education)
#' edu <- label(infert$education, c("low", "middle", "high"))
#' table(edu)
#'
#' ## Example 3: character
#' sex <- rep(c("Male", "Female", NA), c(30, 60, 10))
#' sex; table(sex)
#' sex1 <- label(sex, c('f', 'm'))
#' sex1; table(sex1)
#' label(sex, c('f', 'm', 'missing'))
#'
#' ## Example 4: Logical
#' s <- sex == "Male"
#' s; table(s)
#' s1 <- label(s, c("male", "female"))
#' s1; table(s1)
#' label(s, c('male', 'female', 'missing'))

#' @export
label <- function(x, labels)
{
  if (any(is.na(unique(x))) & (length(unique(x)) == length(labels))) {
    exclude <- NULL
  } else {exclude <- NA}
  if (is.numeric(x) | is.character(x)) {
    x <- factor(x, labels = labels, exclude = exclude)
  } else if (is.factor(x)) {
    x <- factor(unclass(x), labels = labels, exclude = exclude)
  } else if (is.logical(x)) {
    x <- factor(2 - x, labels = labels, ordered = FALSE, exclude = exclude)
  } else stop('x can not be labelled.')
  return(x)
}
