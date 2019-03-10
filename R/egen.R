#' @title Transform Numeric to Factor
#'
#' @description
#' \code{egen} allows to transform a numeric vector to a factor vector easily.
#'
#' @param x numeric vector
#' @param cut either a single number or a numeric vector.
#' @param labels specify to name the factor levels.
#' @param na.rm A logical value to specify missing values
#' @details
#' \code{egen} allows easy conversion of a numeric vector to factor.
#'
#' \strong{Cut-off Intervals}
#'
#' If a single number is specified,
#' the range of the data is divided into equal cut-off points by that number. The last
#' piece will be always the same or smaller than the rest. In case of vector, the cut-off
#' points will be determined accordingly.
#'
#' \strong{Label Construction}
#' If none is specified, then the factor
#' level labels are constructed as \strong{variable name} plus \strong{cut-off intervals}.
#' @seealso \code{\link{gen}}, \code{\link{label}}, \code{\link{dup.rm}},
#' @keywords distribution, number summary, correlation
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' set.seed(1)
#' age <- round(c(rnorm(100, 45, 20), rep(NA, 20)),0)
#' summary(age)
#'
#' egen(age)
#' egen(age, cut = 20)
#' egen(age, cut = c(1, 20, 40))
#' egen(age, cut = c(1, 20, 40), labels = c("young", "middle", "old"))
#' egen(age, cut = c(1, 20, 40, 60, 100))
#'
#' ## remove missing value
#' egen(age, cut = 20, na.rm = TRUE)
#' egen(age, cut = c(1, 20, 40), na.rm = TRUE)
#' egen(age, cut = c(1, 20, 40), labels = c("young", "middle", "old"), na.rm = TRUE)
#' egen(age, cut = c(1, 20, 40, 60, 100), na.rm = TRUE)

#' @export
egen <- function(x, cut = NULL, labels = NULL, na.rm = FALSE)
{
  x.name <- deparse(substitute(x))
  if (na.rm) x <- x[!is.na(x)]

  if (is.null(cut)) {
    cut <- 10
    x.brk <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), cut)
    x.brk <- c(x.brk[1], x.brk[2:(length(x.brk)-1)] - 1, x.brk[length(x.brk)] - 1)
  }  else {
    if (length(cut) == 1) {
      x.brk <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), cut)
      x.brk <- c(x.brk[1], x.brk[2:(length(x.brk)-1)] - 1, x.brk[length(x.brk)] - 1)
    } else {
      x.brk <- cut
    }
  }

  x.max <- max(x, na.rm = TRUE)
  if (x.brk[length(x.brk)] < x.max) {
    x.brk <- c(x.brk, x.max)
  } else {
    x.brk <- x.brk[x.brk < x.max]
  }
  if (x.brk[length(x.brk)] != x.max) x.brk <- c(x.brk, x.max)

  if (is.null(labels)) {
    x.lbl.lwr <- c(x.brk[1], x.brk[-c(1, length(x.brk))] + 1)
    x.lbl.upr <- x.brk[-1]
    x.lbl <- paste0(x.name, ".", paste(x.lbl.lwr, x.lbl.upr, sep = "-"))
  } else {x.lbl <- labels}

  x <- cut(x, breaks = x.brk, labels = x.lbl, right = TRUE,
           include.lowest = TRUE)
  return(x)
}
