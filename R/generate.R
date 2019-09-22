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
#' str(infert)
#' infert.new <- generate(age.cat, infert)
#' str(infert.new)
#'
#' # variable already exists
#' infert.new <- generate(age.cat, infert.new)
#'
#' infert.new <- generate(age.cat, infert, ifelse(age < 20, 1,
#'                  ifelse(age >= 20 & age < 30, 2,
#'                     ifelse(age >= 30 & age < 40, 3, 4))))
#' str(infert.new)
#'
#' # based on vector
#' age <- infert$age
#' age.cat <- generate(age.cat, NULL, ifelse(age < 20, 1,
#'                  ifelse(age >= 20 & age < 30, 2,
#'                     ifelse(age >= 30 & age < 40, 3, 4))))
#' age.cat
#' }

#' @export
generate <- function(var, data = NULL, expr = NULL)
{
  arguments <- as.list(match.call())
  var.name <- deparse(substitute(var))
  data.name <- deparse(substitute(data))
  expr.txt <- paste(deparse(substitute(expr)), collapse = "")
  data.null <- is.null(data)

  if (data.null) {
    txt <- expr.txt
    heading <- paste0(var.name, " <- ", txt)
  } else {
    if (var.name %in% names(data))
      stop(paste0("... '", var.name, "' already exists ... "))
    txt <- paste0("with(", data.name, ", ", expr.txt, ")")
    heading <- paste0(data.name, "$", var.name, " <- ", txt)
  }
  if (is.null(expr)) {
    txt <- "NA"
    heading <- paste0(var.name, " <- ", txt)
  }

  f <- tryCatch(eval(parse(text = txt)),
                error=function(cond) {
                  message(paste0(" ... cannot evaluate the expression '",
                                 txt, "'  ... "))
                })

  cat(paste0("... ", length(f), " values are generated from '", var.name,
             " ... \n... Expresssion used: \n\t", wrap.output(heading, 80, "\n\t")))

  if (!data.null) {
    f <- cbind(data, f)
    names(f)[length(f)] <- var.name
  }

  invisible(f)
}
