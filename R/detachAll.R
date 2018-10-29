#' Detach all objects in the search pathway
#'
#' detachAll() lets you detach two types of objects in the search pathway:
#' 1) user-installed packages
#' 2) attached data objects
#' @param NA No parameter required.
#' @keywords detach, packages, data, search
#' @export
#' @examples
#' detachAll()

detachAll <- function() {
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
