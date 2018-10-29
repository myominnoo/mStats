#' Clear Workspace, Plots and Console
#'
#' ren() is a quick way of clearing messy workspace, plots and console.
#' @param NA No parameter required.
#' @keywords clear, workspace, plots, console
#' @export
#' @examples
#' ren()

ren <- function() {
  # Clear plots
  if(!is.null(dev.list())) dev.off()
  # Clear workspace
  rm(list = ls(envir = .GlobalEnv), pos = 1)
  # Clear console
  cat(rep("\n", 50))
}
