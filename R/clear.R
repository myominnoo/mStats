#' @title Clean Global Environment, Plots and Console
#'
#' @description
#' \code{clear} is a quick way to clear messy Global Environment, plots and console
#' without interrupting the search path.
#'
#' @keywords remove all, clean, global environment, plots, console
#' @import grDevices
#' @seealso \code{\link{ilog}}
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#' x <- rnorm(100)
#' plot(x)
#' clear()
#' }


#' @export
clear <- function() {
  while (!is.null(dev.list()))  dev.off()
  rm(list = ls(envir = .GlobalEnv), pos = 1)
  cat(rep("\n", 50))
}
