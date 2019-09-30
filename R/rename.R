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
#' \dontrun{
#' infert.new <- rename(infert, age, AGE)
#' str(infert.new)
#'
#' infert.new <- rename(infert, c(age, parity, induced, case),
#'                c(AGE, PARITY, INDUCED, CASE))
#' str(infert.new)
#' }


#' @export
rename <- function(data, old.var, new.var)
{
    arguments <- as.list(match.call())
    old.var.names <- (deparse(substitute(old.var)))
    old.var.names <- unlist(strsplit(gsub("^c\\(|\\)$", "", old.var.names), ","))

    var.len <- length(old.var.names)
    if (var.len > 1) old.var <- list() else old.var <- as.character()
    UseMethod("rename", old.var)
}



#' @rdname rename
#' @export
rename.default <- function(...)
{
    stop(" ... Wrong data type ... ")
}


#' @rdname rename
#' @export
rename.character <- function(data, old.var, new.var)
{
    arguments <- as.list(match.call())
    old.var <- as.character(arguments$old.var)
    new.var <- as.character(arguments$new.var)
    contain <- names(data) %in% old.var
    names(data)[contain] <- new.var
    printMsg(paste0("Variable: renamed '", old.var,
                    "' to '", new.var, "'"))
    return(data)
}


#' @rdname rename
#' @export
rename.list <- function(data, old.var, new.var)
{
    arguments <- as.list(match.call())
    old.var <- as.character(arguments$old.var)[-1]
    new.var <- as.character(arguments$new.var)[-1]
    vars <- names(data)

    var.found <- vars[vars %in% old.var]
    var.missed <- old.var[!(old.var %in% var.found)]

    if (length(var.missed) > 0)
        stop(paste0("\n... '", var.missed, "' not found ..."))

    len.old.var <- length(old.var)
    if (len.old.var != length(new.var)) {
        stop("... Old and new variables must have the same length ...")
    }

    names(data)[vars %in% old.var] <- new.var
    for (i in 1:len.old.var)
        printMsg(paste0("Variable: renamed '", old.var[i], "' to '", new.var[i], "'"))

    return(data)
}

