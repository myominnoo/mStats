#' @title Detach all objects in search pathway
#'
#' @description
#' idetach removes two types of user-loaded objects in the search pathway:
#'
#' 1) user-loaded packages
#'
#' 2) attached data objects
#'
#' @keywords detach all, packages, data, search
#' @examples
#' library(foreign)
#' library(MASS)
#' search()
#' idetach()
#' search()

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
