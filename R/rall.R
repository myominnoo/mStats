#' Restart R environment as fresh
#'
#' @description
#' rall() lets you clear deep in the search path:
#' 1) user-installed packages
#' 2) attached data objects
#' 3) Global Environment
#' 4) Plots
#' 5) Console
#' 6) Restart R session
#'
#' @param NA No parameter required.
#' @keywords detach, packages, data, search
#' @export
#' @examples
#' rall()

rall <- function() {
  # Clear plots
  if(!is.null(dev.list())) dev.off()
  # detaching user-installed packages and attached data
  detachAll()
  # restart R
  .rs.restartR()
  # Clear workspace
  rm(list = ls(envir = .GlobalEnv), pos = 1)
  # Clear console
  cat(rep("\n", 50))
}
