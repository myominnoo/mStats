#' @title Create a new variable
#'
#' @description
#' \code{generate} creates a new variable within the data frame
#'
#' @param data data frame
#' @param var name of a new variable
#' @param expr value or Expression for simple arithmetic or logical operations:
#' See examples.
#'
#' @details
#'
#' If the data is specified, it returns the whole dataframe with
#' recoded variables. If \code{expr} is NULL, \code{generate} produce a vector
#' \code{NA}. The value of the variable are specified by \code{expr} argument.
#'
#' @references
#'
#' STATA DATA MANAGEMENT. UCLA: Statistical Consulting Group.
#' from https://stats.idre.ucla.edu/stata/seminars/stata-data-management/
#' (accessed Febrary 25, 2020).
#'
#' @seealso
#'
#' \code{\link{recode}}
#'
#' @keywords new variable, generate, produce, create, transform
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
#' # create a NA variable age.grp in infert
#' infert.new <- generate(infert, age.grp)
#' listView(infert.new, age, age.grp)
#'
#' # create variable age.cat using expression
#' infert.new <- generate(infert, age.cat,
#'          ifelse(age < 20, 1,
#'                 ifelse(age >= 20 & age < 30, 2,
#'                        ifelse(age >= 30 & age < 40, 3, 4))))
#' listView(infert.new, age, age.cat)
#'
#' # create variable age.cat using expression with "labels"
#' infert.new <- generate(infert, age.cat,
#'          ifelse(age <= 20, "<=20",
#'                 ifelse(age > 20 & age <= 30, "21-30",
#'                        ifelse(age > 30 & age <= 40, "31-40", "41+"))))
#' listView(infert.new, age, age.cat)
#'
#' # based on vector
#' age <- infert$age
#' age.cat <- generate(NULL, age.cat, ifelse(age < 20, 1,
#'                  ifelse(age >= 20 & age < 30, 2,
#'                     ifelse(age >= 30 & age < 40, 3, 4))))
#' age.cat
#'
#' # Example from IDRE website in STATA
#' path <- "https://stats.idre.ucla.edu/stat/data/patient_pt1_stata_dm.dta"
#' hosp <- haven::read_dta(path)
#' hosp <- generate(hosp, average, (test1 + test2) / 2)
#' str(hosp)
#' listView(hosp, test1:average)
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
