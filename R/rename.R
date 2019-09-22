  #' @title Rename variable
  #'
  #' @description
  #' \code{rename} Rename variable
  #'
  #' @param old.var R object or list
  #' @param new.var R object or list
  #' @param data Data Frame
  #' @param ... Optional arguments
  #' @details
  #' \code{rename}
  #' changes the name of an existing variable old_varname to new_varname;
  #' the contents of the variable are unchanged.
  #'
  #' Groups of variables can be also renamed by specifying variables in a list.
  #'
  #' @keywords rename, change names
  #' @seealso \code{\link{ilog}}
  #' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
  #' Website: \url{https://myominnoo.github.io/})
  #' @examples
  #' iris1 <- rename(Species, plantType, iris)
  #' names(iris1)
  #'
  #' iris2 <- rename(list(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
  #'                 list(sl, sw, pl, pw), iris)
  #' names(iris2)

  #' @export
  rename <- function(old.var, new.var, data)
  {
    arguments <- as.list(match.call())
    old.var <- as.character(arguments$old.var)
    if (length(old.var) > 1) old.var <- list(old.var[-1])
    UseMethod("rename", old.var)
  }

  #' @rdname rename
  #' @export
  rename.default <- function(...)
  {
    warning(" .... Wrong data type ... ")
  }

  #' @rdname rename
  #' @export
  rename.character <- function(old.var, new.var, data)
  {
    arguments <- as.list(match.call())
    old.var <- as.character(arguments$old.var)
    new.var <- as.character(arguments$new.var)
    contain <- names(data) %in% old.var
    names(data)[contain] <- new.var
    cat(paste0("... renamed '", old.var, "' to '",
               new.var, "' ... \n"))
    invisible(data)
  }

  #' @rdname rename
  #' @export
  rename.list <- function(old.var, new.var, data)
  {
    arguments <- as.list(match.call())
    old.var <- as.character(arguments$old.var)[-1]
    new.var <- as.character(arguments$new.var)[-1]
    if (length(old.var) != length(new.var))
      stop(" ... name lists of old and new variables must have the same length ... ")
    contain <- names(data) %in% old.var
    names(data)[contain] <- new.var
    for (i in 1:length(old.var)) {
      cat(paste0("... renamed '", old.var[i], "' to '",
                 new.var[i], "' ... \n"))
    }
    invisible(data)
  }

