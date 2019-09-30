#' @title Duplicate observations within a dataframe
#'
#' @description
#' \code{expand2} generates duplicated observations within a dataframe.
#'
#' @param data a data frame object
#' @param n_n index or indexes specifying row numbers
#' @param copies desired number of copies
#' @param original a logical indicating whether to keep the original dataframe
#' @details
#'
#' \code{expand2}
#'
#' appends observations from the dataframe with n copies of the observations with
#' specified indexes of observations or all data.
#'
#' @seealso \code{\link{keep}}, \code{\link{generate}}, \code{\link{egen}}
#' @keywords duplicates, observations, expand, duplicate rows
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#'
#' #### Ref:
#' # HOW CAN I DETECT DUPLICATE OBSERVATIONS? | STATA FAQ
#' # from https://stats.idre.ucla.edu/stata/faq/how-can-i-detect-duplicate-observations-3/
#' # (accessed September 27, 2019).
#'
#' required(haven)
#' # install.packages("haven")
#' hsb2 <- read_dta("https://stats.idre.ucla.edu/stat/stata/notes/hsb2.dta")
#' str(hsb2)
#' hsb2.new <- expand2(hsb2, 1:5, 4)
#' hsb2.new <- expand2(hsb2, 1:5, 4, original = FALSE)
#' }


#' @export
expand2 <- function(data, n_n = NULL, copies = 2, original = TRUE)
{
  #### if n_n is empty, put number of all rows to n_n
  if (is.null(n_n)) {
    n_n <- nrow(data)
  }
  #### if there are more than one values in n_n, take the last value
  if (length(n_n) == 1) {
    n_n <- 1:n_n
  }
  t <- data[n_n, ]

  if (original) {
    f <- data
  } else {
    f <- NULL
  }
  for (i in 1:(copies)) {
    f <- rbind(f, t)
  }
  return(f)
}
