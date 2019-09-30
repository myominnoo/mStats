#' @title Create a new variable
#' @description
#' \code{generate} easily create a new variable within the data frame
#' @param var name of a new variable
#' @param data data frame
#' @param expr value or Expression: See examples below.
#' @details
#'
#' If data is specified, it returns the whole dataframe with recoded variables.
#'
#' The value of the variable are specified by \code{expr} argument.
#' If \code{expr} is NULL, \code{generate} produce a vector
#' \code{NA}.
#'
#' @seealso \code{\link{recode}}
#' @keywords new variable, generate, produce
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#' infert.new <- generate(infert, age)
#' head(infert.new)
#'
#' infert.new <- generate(infert, age.cat)
#' head(infert.new)
#'
#' infert.new <- generate(infert, age.cat,
#'          ifelse(age < 20, 1,
#'                 ifelse(age >= 20 & age < 30, 2,
#'                        ifelse(age >= 30 & age < 40, 3, 4))))
#' head(infert.new)
#'
#' # based on vector
#' age <- infert$age
#' age.cat <- generate(NULL, age.cat, ifelse(age < 20, 1,
#'                  ifelse(age >= 20 & age < 30, 2,
#'                     ifelse(age >= 30 & age < 40, 3, 4))))
#' age.cat
#' }


#' @export
generate <- function(data = NULL, var, expr = NULL)
{
  arguments <- as.list(match.call())
  data.null <- is.null(data)

  var.name <- deparse(substitute(var))
  data.name <- deparse(substitute(data))
  expr <- paste(deparse(substitute(expr)), collapse = "")

  if (data.null) {
    expr.txt <- expr
    expr.lbl <- expr
  } else {
    if (var.name %in% names(data))
      stop(paste0("... '", var.name, "' already exists ... "))
    expr.txt <- paste0("with(data, ", expr, ")")
    expr.lbl <- paste0("with(", data.name, ", ", expr, ")")
  }

  f <- tryCatch(eval(parse(text = expr.txt)),
                error=function(cond) {
                  message(paste0(" ... cannot evaluate the expression '",
                                 expr.txt, "'  ... "))
                })

  f.len <- length(f)

  if (!data.null) {
    if (length(f) != nrow(data))
      f <- c(f, rep(NA, (nrow(data) - length(f)) ))
    f <- cbind(data, f)
    names(f)[length(f)] <- var.name
    f.len <- length(f[, var.name])
  }

  printMsg(paste0(f.len, " values generated"))
  printMsg(paste0("Expression used: '", expr.lbl, "'"))

  return(f)
}
