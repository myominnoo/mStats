#' @title Detach user-loaded packges in the search pathway
#'
#' @description
#' \code{idetach} searches and removes two types of user-loaded objects in the search pathway:
#' 
#' \enumerate{
#'   \item user-loaded packages
#'   \item attached data objects
#' }
#' @import foreign
#' @import MASS
#' @seealso \code{\link{clear}}
#' @keywords detach all, user-loaded packages
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#' library(foreign)
#' library(MASS)
#' search()
#' idetach()
#' search()
#' }

#' @export
idetach <- function() {
  # detaching user-installed packages
  basic.packages <- c("package:stats","package:graphics","package:grDevices",
                      "package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  # detaching attached data
  attached <- search()[!grepl("package", search())]
  attached <- attached[-match(c(".GlobalEnv", "tools:rstudio", "Autoloads"), attached)]
  if (length(attached)>0)  for (attach in attached) detach(attach, character.only=TRUE)
}
