#' @title Keep or lose variables and pick rows
#'
#' @description
#'
#' \code{keep} or \code{lose} variables within dataframe
#'
#' \code{pick} observations that satisfy specified condition
#'
#' @param data Dataset
#' @param ... variables
#'
#' @details
#'
#' \code{keep} remove all unspecified variables in the dataset.
#' Variables are also rearranged based on the order they are mentioned.
#'
#' \preformatted{keep(data, var1, var2, var3, etc)}
#'
#' \code{lose} works the same as \code{keep}, except that
#' the variables mentioned are removed from the dataset.
#'
#' \preformatted{lose(data, va5, var6, var7, etc)}
#'
#' \code{pick} selects the rows that meets the specified conditions.
#' This is the same as \strong{filter} function in spreadsheet or
#' subset in R.
#'
#' \code{data.frame} or \code{filter} can be used with the same
#' epxressions.
#'
#' \preformatted{pick(data, var1 == var2 | var3 > var4)}
#'
#' \code{arrange} orders the observations of current dataset
#' into ascending order based on the names of the variables mentioned.
#' There is no limit to the number of variables that can be specified
#' in the arguments.
#'
#' It will be in decreasing order if specified negative sign or "-"
#' in front of the desired variable(s) without spacing.
#'
#' \preformatted{arrange(data, var1, var2, -var3)}
#'
#' @note
#'
#' \code{drop} and \code{keep} are not reversible.  Once you have
#' eliminated variables and assigned to the same name,
#' deleted variables are gone. You need to go back and read the original
#' dataset again.
#'
#' Hence, the best strategy is to assign to a different name rather than
#' putting it back to the original dataset.
#'
#' @seealso
#'
#' \code{\link{generate}}, \code{\link{egen}}, \code{\link{recode}}
#'
#' @keywords
#'
#' keep, drop, delete, subset, filter, pick, arrange, select
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
#' ## keeping variables
#' keep(infert, education, age, parity)
#' keep(infert, parity, age:spontaneous, stratum) # variables repositioned
#'
#' ## losing or removing variables
#' lose(infert, age, parity)
#' lose(infert, age:case, education)
#'
#' ## pick observations
#' pick(infert, age > 30)
#' pick(infert, parity == 6)
#' pick(infert, age > 30 & age < 35)
#' pick(infert, case == 0, age > 30 & age < 35, parity == 6)
#'
#' # use vector instead of dataframe
#' age <- infert$age
#' pick(age, age > 40)
#' pick(infert, age > 40)
#'
#' ## arrange observations
#' arrange(infert, age)
#' arrange(infert, -age)
#' arrange(infert, age, induced)
#' arrange(infert, age, -induced)
#' arrange(infert, age, -induced, case)
#' arrange(infert, age, -induced, -case)
#'
#' arrange(infert, education)
#' arrange(infert, education, -age)
#' }


#' @export
keep <- function(data, ... ) {
  if (is.null(data) | !is.data.frame(data))
    stop(" ... a dataframe must be specified ... ")

  arguments <- as.list(match.call())
  vars.names <- names(data)
  vars <- as.character(arguments[c(-1,-2)])

  vars.found <- vars.names[vars.names %in% vars]
  if (any(grep(":", vars))) {
    vars.colon <- grep(":", vars, value = TRUE)
    vars <- vars[!(vars %in% vars.colon)]
    vars.colon <- unlist(strsplit(vars.colon, ":"))
    vars.colon <- which(vars.names %in% vars.colon)
    vars.colon <- seq(vars.colon[1], vars.colon[2])
    vars.colon <- vars.names[vars.colon]
    vars.found <- unique(c(vars.found, vars.colon))
    vars <- unique(c(vars, vars.colon))
  }

  vars.missed <- vars[!(vars %in% vars.found)]

  if (length(vars.missed) > 0)
    stop(paste0("\n ... variable not found: ", vars.missed))

  data <- data[, vars.found]

  printMsg(paste0(length(vars.found), " variables kept: ",
                  paste(vars.found, collapse = ", ")))
  return(data)
}


#' @rdname keep
#' @export
lose <- function(data, ... ) {
  if (is.null(data) | !is.data.frame(data))
    stop(" ... a dataframe must be specified ... ")

  arguments <- as.list(match.call())
  vars.names <- names(data)
  vars <- as.character(arguments[c(-1,-2)])

  vars.found <- vars.names[vars.names %in% vars]
  if (any(grep(":", vars))) {
    vars.colon <- grep(":", vars, value = TRUE)
    vars <- vars[!(vars %in% vars.colon)]
    vars.colon <- unlist(strsplit(vars.colon, ":"))
    vars.colon <- which(vars.names %in% vars.colon)
    vars.colon <- seq(vars.colon[1], vars.colon[2])
    vars.colon <- vars.names[vars.colon]
    vars.found <- unique(c(vars.found, vars.colon))
    vars <- unique(c(vars, vars.colon))
  }

  vars.missed <- vars[!(vars %in% vars.found)]

  if (length(vars.missed) > 0)
    stop(paste0("\n ... variable not found: ", vars.missed))

  vars.keep <- vars.names[!vars.names %in% vars]
  data <- data[, vars.keep]
  printMsg(paste0(length(vars), " variables removed: ",
                  paste(vars, collapse = ", ")))
  return(data)
}


#' @rdname keep
#' @export
pick <- function(data, ... )
{
  arguments <- as.list(match.call())
  data.name <- deparse(substitute(data))
  expr <- as.character(arguments[c(-1,-2)])

  expr.len <- length(expr)
  if (expr.len > 1) {
    expr.c <- paste0("(", expr[1], ")")
    for (n in 1:(expr.len - 1)) {
      expr.c  <- paste0(expr.c, " & (", expr[n + 1], ")")
    }
    expr <- expr.c
  }

  if (is.data.frame(data)) {
    expr.txt <- paste0("data[ with(data, ", expr, "), ]")
    expr.lbl <- paste0(data.name, "[ with(", data.name, ", ", expr, "), ]")
  } else {
    expr.txt <- paste0("data[ ", expr, " ]")
    expr.lbl <- paste0(data.name, "[ ", expr, " ]")
  }

  f <- tryCatch(eval(parse(text = expr.txt)),
                error=function(cond) {
                  message(paste0(" ... cannot evaluate the expression '",
                                 expr.txt, "'  ... "))
                })
  #
  printMsg(paste0(ifelse(is.data.frame(data), nrow(f), length(f)),
                  " observations picked"))
  printMsg(paste0("Expression used: '", expr.lbl, "'"))
  return(f)
}


#' @rdname keep
#' @export
arrange <- function(data, ... )
{
  if (is.null(data) | !is.data.frame(data))
    stop(" ... a dataframe must be specified ... ")

  arguments <- as.list(match.call())
  data.names <- deparse(substitute(data))
  expr <- as.character(arguments[c(-1,-2)])
  expr.vars <- expr

  expr.len <- length(expr)
  if (expr.len > 1) {
    expr.c <- expr[1]
    for (n in 1:(expr.len - 1)) {
      expr.c  <- paste0(expr.c, ", ", expr[n + 1])
    }
    expr <- expr.c
  }

  expr.txt <- paste0("data[ with(data, order(", expr, ") ), ]")
  expr.lbl <- paste0(data.names, "[ with(", data.names, ", order(", expr, ") ), ]")

  f <- tryCatch(eval(parse(text = expr.txt)),
                error=function(cond) {
                  message(paste0(" ... cannot evaluate the expression '",
                                 expr.txt, "'  ... "))
                })
  printMsg(paste0("dataframe arranged by: '",
                  paste(expr.vars, collapse = "' => '"), "'"))
  printMsg(paste0("Expression used: '", expr.lbl, "'"))
  return(f)
}
