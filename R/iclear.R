#' @title Clear Global Environment, Plots and Console
#'
#' @description
#' \code{iclear} is a quick way to clear messy Global Environment, plots and console without interrupting the search path. If you want to detach all packages, see \code{\link{idetach}}.
#'
#' @keywords remove all, global environment, remove plots, clear console
#' @seealso \code{\link{idetach}}
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' x <- rnorm(100)
#' y <- rnorm(100)
#' z <- x + y
#' hist(x)
#' hist(y)
#' hist(z)
#' iclear()

#' @export
iclear <- function() {
  # Clear plots
  if(!is.null(dev.list())) dev.off()
  # Clear workspace
  rm(list = ls(envir = .GlobalEnv), pos = 1)
  # Clear console
  cat(rep("\n", 50))
}
