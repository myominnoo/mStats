#' @title Clear Global Environment, Plots and Console
#'
#' @description
#' iclear is a quick way of clearing messy global environment, plots and console without touching
#' the search path.
#'
#' @keywords remove all, global environment, remove plots, clear console
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
