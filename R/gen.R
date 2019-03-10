#' @title Create a new variable within the data frame
#' @description
#' \code{gen} function allows R user to easily create a new variable within the data frame
#' @param data data frame
#' @param var name of a new variable
#' @param expr value or Expression: See examples below.
#' @details
#' \code{gen} (generate) creates a new variable within the data frame. The value of the
#' variable are specified by \code{expr} argument. If \code{expr} is not specified,
#' \code{gen} generates \code{NA} by default.
#' @seealso \code{\link{label}}
#' @keywords value label, label levels
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' data(infert)
#' df <- infert
#' str(df)
#' df <- gen(df, age.grp, ifelse(age < 25, 1, ifelse(age >= 25 & age < 35, 2, 3)))
#' table(df$age.grp)
#' df$age.grp <- label(df$age.grp, c("young", "middle", "old"))
#' table(df$age.grp)
#'
#' ## create new variable with mean value
#' gen(infert, age.mean, mean(age))

#' @export
gen <- function(data, var, expr = NULL)
{
  if (!is.data.frame(data)) stop("Input must be a data frame.")
  var <- deparse(substitute(var))
  expr <- deparse(substitute(expr))
  txt <- paste0("data$", var)
  if (length(eval(parse(text = txt))) == 0) {
    if (expr == "NULL") {
      eval(parse(text = paste0(txt, " <- NA")))
    } else {
      eval(parse(text = paste0(txt, " <- with(data, ", expr, ")")))
    }
  } else stop(paste0(var, " already exists."))
  return(data)
}
