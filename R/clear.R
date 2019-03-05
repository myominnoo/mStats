#' @title Clear Global Environment, Plots and Console
#'
#' @description
#' \code{clear} is a quick way to clear messy Global Environment, plots and console 
#' without interrupting the search path. If you want to detach all user-loaded packages, 
#' see \code{\link{idetach}}.
#'
#' @keywords remove all, global environment, clear plots, clear console
#' @import grDevices
#' @seealso \code{\link{idetach}}
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' x <- rnorm(100)
#' plot(x)
#' \dontrun{
#'   clear()
#' }


#' @export
clear <- function() {
  while (!is.null(dev.list()))  dev.off()
  rm(list = ls(envir = .GlobalEnv), pos = 1)
  cat(rep("\n", 50))
}
