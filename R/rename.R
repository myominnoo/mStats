#' @title Rename variable
#'
#' @description
#' \code{rename()} changes names of variables.
#'
#' @param data dataset
#' @param old_var name of existing variable
#' @param new_var new name to be changed
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
#' ## using infert dataset
#' data(infert)
#'
#' # renaming one variable
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
#' \dontrun{
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
rename <- function(data, old_var, new_var)
{
    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    ## check old vars if vector. If so, remove c() and convert to character.
    old_var <- deparse(substitute(old_var))
    if (grepl("^c\\(|\\)$", old_var)) {
        .vars.old <- unlist(strsplit(gsub("^c\\(|\\)$", "", old_var), ","))
    } else {
        .vars.old <- old_var
    }

    ## trim white spaces
    .vars.old <- trimws(.vars.old)

    ## Check if colon is there.
    ## if present, retrieve variables between the two variables
    if (any(grepl(":", .vars.old))) {
        .vars.old <- do.call(
            c,
            lapply(.vars.old, function(z) {
                .colon <- grepl(":", z)
                if (.colon) {
                    splitByColon(data, z, .colon)
                } else {
                    z
                }
            })
        )
    }

    ## check new vars if vector. If so, remove c() and convert to character.
    new_var <- deparse(substitute(new_var))
    if (grepl("^c\\(|\\)$", new_var)) {
        .vars.new <- unlist(strsplit(gsub("^c\\(|\\)$", "", new_var), ","))
    } else {
        .vars.new <- new_var
    }

    ## trim white spaces
    .vars.new <- trimws(.vars.new)

    ## Check if colon is there.
    ## if present, retrieve variables between the two variables
    if (any(grepl(":", .vars.new))) {
        .vars.new <- do.call(
            c,
            lapply(.vars.new, function(z) {
                .colon <- grepl(":", z)
                if (.colon) {
                    splitByColon(data, z, .colon)
                } else {
                    z
                }
            })
        )
    }

    ## old and new vectors must have the same length.
    .vars.old.len <- length(.vars.old)
    if (.vars.old.len != length(.vars.new))
        stop(" ... Old and new variables must have the same length! ... ")


    ## change the names of variables
    names(data)[names(data) %in% .vars.old] <- .vars.new

    ## Display message to nofity changes
    if (.vars.old.len == 1) {
        printMsg(paste0("Variable: renamed '", .vars.old, "' to '", .vars.new, "'"))
    } else {
        for (i in 1:.vars.old.len) {
            printMsg(paste0("Variable: renamed '", .vars.old[i], "' to '", .vars.new[i], "'"))
        }
    }

    return(data)
}
