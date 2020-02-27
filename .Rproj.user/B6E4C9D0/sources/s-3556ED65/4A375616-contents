#' @title Rename variable
#'
#' @description
#' \code{rename()} changes names of variables.
#'
#' @param data dataset
#' @param var_old name of existing variable
#' @param var_new new name to be changed
#'
#' @details
#'
#' \code{rename()} changes the name of an existing variable,
#' \code{var_old} to a new name, \code{var_new};
#' the contents of the variable are unchanged.
#'
#' A group of variables can be also \code{renamed} by specifying the same
#' number of variables in both \code{var_old} and \code{var_new}
#'
#' @return
#'
#' \code{data.frame}
#'
#' @keywords rename, change names
#'
#' @seealso
#'
#' \code{\link{recode}}, \code{\link{generate}}, \code{\link{egen}}
#'
#' @author
#'
#' For any feedback, please contact \code{Myo Minn Oo} via:
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#' \dontrun{
#' ## using infert dataset
#' # renaming one variable
#' codebook(infert)
#' infert.new <- rename(infert, age, AGE)
#' codebook(infert.new)
#'
#' # renaming a group of variables
#' infert.new <- rename(infert,
#'                     c(age, parity, induced, case),
#'                     c(AGE, PARITY, INDUCED, CASE))
#' codebook(infert.new)
#'
#'
#'
#' ## IDRE UCLA Example 1
#' path <- "https://stats.idre.ucla.edu/stat/data/patient_pt1_stata_dm.dta"
#' hosp <- haven::read_dta(path)
#' codebook(hosp)
#'
#' # one variable
#' hosp <- rename(hosp, sex, SEX)
#' codebook(hosp)
#'
#' # multiple variables
#' hosp <- rename(hosp,
#'               c(wbc, rbc, bmi, test1, test2),
#'               c(WBC, RBC, BMI, TEST1, TEST2))
#' codebook(hosp)
#' }


#' @export
rename <- function(data, var_old, var_new)
{
    arguments <- as.list(match.call())
    var_old.names <- (deparse(substitute(var_old)))
    var_old.names <- unlist(strsplit(gsub("^c\\(|\\)$", "", var_old.names), ","))

    var.len <- length(var_old.names)
    if (var.len > 1) var_old <- list() else var_old <- as.character()
    UseMethod("rename", var_old)
}



#' @rdname rename
#' @export
rename.default <- function(...)
{
    stop(" ... Wrong data type ... ")
}


#' @rdname rename
#' @export
rename.character <- function(data, var_old, var_new)
{
    arguments <- as.list(match.call())
    var_old <- as.character(arguments$var_old)
    var_new <- as.character(arguments$var_new)
    contain <- names(data) %in% var_old
    names(data)[contain] <- var_new
    printMsg(paste0("Variable: renamed '", var_old,
                    "' to '", var_new, "'"))
    return(data)
}


#' @rdname rename
#' @export
rename.list <- function(data, var_old, var_new)
{
    arguments <- as.list(match.call())
    var_old <- as.character(arguments$var_old)[-1]
    var_new <- as.character(arguments$var_new)[-1]
    vars <- names(data)

    var.found <- vars[vars %in% var_old]
    var.missed <- var_old[!(var_old %in% var.found)]

    if (length(var.missed) > 0)
        stop(paste0("\n... '", var.missed, "' not found ..."))

    len.var_old <- length(var_old)
    if (len.var_old != length(var_new)) {
        stop("... Old and new variables must have the same length ...")
    }

    names(data)[vars %in% var_old] <- var_new
    for (i in 1:len.var_old)
        printMsg(paste0("Variable: renamed '", var_old[i], "' to '", var_new[i], "'"))

    return(data)
}

