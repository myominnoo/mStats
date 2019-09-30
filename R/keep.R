#' @title Keep or lose variables and pick rows
#' @description
#' \code{keep} or \code{lose} variables within dataframe
#'
#' \code{pick} observations that satisfy specified condition
#'
#' @param data dataframe (or Vector in pick)
#' @param ... variables
#' @details
#'
#' \code{keep}
#'
#' It keeps variables in the data, meaning that variables not specified are
#' deleted.
#' Variables can also be repositioned based on position.
#'
#' \code{lose}
#'
#' It works the same as \code{keep}, except that you specify the
#' variables or observations to be lost from the dataframe.
#'
#' Warning: drop and keep are not reversible.  Once you have
#' eliminated variables and assigned back, you cannot read them back in again.
#' You would need to go back to the original dataframe and read it in
#' again.
#'
#' Instead of assigning back to the original dataframe, consider using multiple
#' copies of dataframe subsets temporarily. This is usually the best strategy.
#'
#' \code{pick}
#'
#' It selects the rows that meets the specified conditions.
#' This is the same as filter function in spreadsheet or subset in R.
#'
#' Dataframe or vector can be used and epxressions to specify are almost similar.
#'
#' \code{arrange}
#'
#' It arranges the observations of the current data into ascending
#' order based on the names of the variables.
#' There is no limit to the number of variables that can be specified in the
#' arguments.
#' It can be in decreasing order if specified negative sign or "-" in front of
#' the desired variable(s) without spacing.
#'
#' @seealso \code{\link{generate}}, \code{\link{recode}}, \code{\link{rename}}
#' @keywords keep, drop, delete, subset, filter, pick
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#' keep(infert, education, age, parity)
#' keep(infert, parity, age:spontaneous, stratum) # variables repositioned
#'
#' lose(infert, age, parity)
#' lose(infert, age:case, education)
#'
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
                  paste(vars.found, collapse = " | ")))
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
  printMsg(paste0(length(vars), " variables deleted: ",
                  paste(vars, collapse = " | ")))
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
