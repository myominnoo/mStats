#' @title Recode contents of a variable
#' @description
#' \code{recode} easily manipulates contents of a new variable
#' of a data.frame
#'
#' @param data dataset
#' @param var name of a new variable
#' @param values_old vector
#' @param values_new vector (length of 1 or same with values_old)
#' @details
#'
#' \code{recode}
#' changes the values of variables including categorical variables
#' according to the rules specified below.
#'
#' In case of factor, \code{recode} first converts the vector into character,
#' recodes and then revert back to factor.
#'
#' If data is specified, it returns the whole dataframe with recoded variables.
#'
#' \strong{Sample Inputs for conversion}:
#'
#' Old.value    to    New.value
#'
#'    #        >>>>       #
#'
#' c(#, #)     >>>>    c(#, #)
#'
#' c(#, #)     >>>>       #
#'
#'    #:#      >>>>       #
#'
#' @seealso
#'
#' \code{\link{generate}}, \code{\link{egen}}, \code{\link{duplicates}}
#'
#' @keywords recode, change, generate, produce
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
#' table(infert$case)
#' infert.new <- recode(infert, case, 0, 2)
#' table(infert.new$case)
#'
#' infert.new <- recode(infert.new, case, c(1, 2), c(3, 4))
#' table(infert.new$case)
#'
#' table(infert$parity)
#' infert.new <- recode(infert, parity, c(1,2,3,4), 0)
#' table(infert.new$parity)
#'
#' table(infert$age)
#' infert.new <- recode(infert, age, 21:30, 1)
#' infert.new <- recode(infert.new, age, 31:40, 2)
#' infert.new <- recode(infert.new, age, 41:44, 3)
#' table(infert.new$age)
#'
#' # errors
#' recode(infert, age, 21:30, c(1, 2))
#' }

#' @export
recode <- function(data = NULL, var, values_old, values_new)
{
  arguments <- as.list(match.call())
  data.name <- deparse(substitute(data))
  var.name <- deparse(substitute(var))

  if (!is.null(data)) {
    var <- eval(substitute(var), data)
  }

  if (is.factor(var)) {
    var.factor <- TRUE
    var <- as.character(var)
  } else {var.factor <- FALSE}

  old.len <- length(values_old)
  new.len <- length(values_new)

  # if old value == 1, new value should be 1
  # if old value > 1, then new value can be 1 or same as old value
  if (old.len > 1) {
    if (new.len == 1) {
      values_new <- rep(values_new, old.len)
    } else {
      if (new.len != old.len)
        stop(paste0(" >>> New values must have the same lenth",
                    "as old values or contain one value. <<< "))
    }
  } else {
    if (new.len != 1) {
      stop("... At least one new value must be specified ...")
    }
  }

  value.change.n <- NULL
  for (i in 1:old.len) {
    if (is.na(values_old[i])) {
      value.change.n <- c(value.change.n,
                          length(var[is.na(var)]))
      var[is.na(var)] <- values_new[i]
    } else {
      value.change.n <- c(value.change.n,
                          length(var[var == values_old[i]]))
      var[var == values_old[i]] <- values_new[i]
    }
    printMsg(paste0(value.change.n[i], " values recoded from '",
                    values_old[i], "' into '",
                    values_new[i], "'"))
  }

  # change back to factor
  if (var.factor) var <- factor(var)

  # return dataframe if any
  if (is.null(data)) {
    data <- var
  } else {
    data[, var.name] <- var
  }

  return(data)
}
