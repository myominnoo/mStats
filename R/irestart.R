#' @title Restart a new R session without quitting
#'
#' @description
#' irestart clears all in the search path and restart a new R session without having to quit R. It does
#' all the following:
#'
#' 1) detach user-loaded packages
#'
#' 2) detach attached data objects
#'
#' 3) clear Global Environment
#'
#' 4) clear Plots
#'
#' 5) clear Console
#'
#' 6) Restart R session
#'
#' @keywords Restart, R session
#' @examples
#' x <- rnorm(100)
#' y <- rnorm(100)
#' z <- x + y
#' hist(x)
#' hist(y)
#' hist(z)
#' irestart()

#' @export
irestart <- function() {
  # remove data from global environment and plots, clear console
  rm.env()
  # detach all user-loaded packages and data
  detachAll()
  # restart R session
  .rs.restartR()
}
