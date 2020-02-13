#' @title Recode contents of a variable
#' @description
#' \code{recode} easily manipulates contents of a new variable within dataframe
#' @param var name of a new variable
#' @param data data frame
#' @param old.value vector
#' @param new.value vector (length of 1 or same with old.value)
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
#' Common rules:
#'
#' Old.value  New.value  Example             Meaning
#'
#' #          #          0 >>> 1             0 recoded to 1
#'
#' c(#, #)    c(#, #)    c(1,2) >>> c(3,4)   1 to 3; 2 to 4
#'
#' c(#, #)    #          c(1,2) >>> 1        1 to 1; 2 to 1
#'
#' #:#        #          1:5 >>> 1           1 to 1; 2 to 1; ... ; 5 to 1
#'
#' @seealso \code{\link{generate}}
#' @keywords recode, change, generate, produce
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
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
recode <- function(data = NULL, var, old.value, new.value)
{
  arguments <- as.list(match.call())
  var.name <- deparse(substitute(var))
  data.name <- deparse(substitute(data))

  var <- eval(substitute(var), data)
  if (is.factor(var)) {
    var.factor <- TRUE
    var <- as.character(var)
  } else {var.factor <- FALSE}

  old.len <- length(old.value)
  new.len <- length(new.value)

  # if old value == 1, new value should be 1
  # if old value > 1, then new value can be 1 or same as old value
  if (old.len > 1) {
    if (new.len == 1) {
      new.value <- rep(new.value, old.len)
    } else {
      if (new.len != old.len)
        stop(paste0("... new values must have the same lenth as old values ",
                    "or one value only ..."))
    }
  } else {
    if (new.len != 1) {
      stop("... at least one new value must be specified ...")
    }
  }

  value.change.n <- NULL
  for (i in 1:old.len) {
    if (is.na(old.value[i])) {
      value.change.n <- c(value.change.n, length(var[is.na(var)]))
      var[is.na(var)] <- new.value[i]
    } else {
      value.change.n <- c(value.change.n, length(var[var == old.value[i]]))
      var[var == old.value[i]] <- new.value[i]
    }
    printMsg(paste0(value.change.n[i], " values recoded | '", old.value[i], "' => '",
                    new.value[i], "'"))
  }

  # change back to factor
  if (var.factor) var <- factor(var)

  # return dataframe if any
  if (!is.null(data)) {
    data[, var.name] <- var
    var <- data
  }

  return(var)
}
