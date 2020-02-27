#' @title Convert \code{2x2 tables} to \code{data.frame}
#'
#' @description
#'
#' \code{expandTables} generates a data frame and supports two levels.
#'
#' @param ... vectors of 2x2 tables
#' @param exp_name Name of \code{Exposure} Variable
#' @param exp_lvl Names of two categories in the order of
#' Exposed and non-exposed
#' @param case_name Name of \code{Case} variable
#' @param case_lvl names of two categories in the order of
#' @param strata_name Name of stratified variable
#' @param stringsAsFactors \code{TRUE} or \code{FALSE}
#' If \code{TRUE}, character vector is converted to a factor when
#' \code{data.frame} is constructed.
#'
#' @details
#'
#' \code{expandTables} uses the vectors of \code{2x2} tables and
#' generates a data frame of at least two columns:
#' Exposure and Outcome.
#'
#' \preformatted{expandTables(c(100, 200, 100, 200))}
#'
#' \strong{Strata}
#'
#' Strata can be included using multiple named vectors. The
#' generated tables can be used for further calculation such as
#' Mantel Haenszel methods.
#'
#' \preformatted{
#' expandTables(
#'              strata1 = c(100, 200, 100, 200),
#'              strata2 = c(100, 200, 100, 200),
#'              strata3 = c(100, 200, 100, 200),
#'              exp_name = "Exposure",
#'              exp_lvl = c("exposed", "unexposed"),
#'              case_name = "Outcome",
#'              case_lvl = c("case", "control"),
#'              strata_name = "Strata"
#' )
#' }
#'
#' \strong{Labels}
#'
#' If variables' names or levels are not specified, the followings are
#' applied.
#'
#' \enumerate{
#'     \item Exposure Name: \code{exposure}
#'     \item Exposure levels: \code{exposed} and \code{unexposed}
#'     \item Outcome Name: \code{outcome}
#'     \item Outcome levels: \code{disease} and \code{healthy}
#'     \item Strata Name: \code{strata}
#'     \item Note: Strata levels are not considered as vectors must
#'     be named.
#' }
#'
#' @return
#'
#' \code{data.frame}
#'
#' @seealso
#'
#' \code{\link{mhodds}}, \code{\link{stmh}}
#'
#' \code{\link{tab}}
#'
#' @keywords expand tables, two-by-two tables, contigency tables
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
#' ## creating a table
#' df <- expandTables(c(60, 140, 60, 40))
#' tab(df)
#' tab(df, exposure)
#' tab(df, exposure, by = outcome)
#'
#'
#'
#' ## one table with full labels
#' df <- expandTables(
#'     c(60, 140, 60, 40),
#'     exp_name = "areaType",
#'     exp_lvl = c("Rural", "urban"),
#'     case_name = "Antibodies",
#'     case_lvl = c("Yes", "No"))
#' tab(df)
#' tab(df, areaType, by = Antibodies)
#'
#'
#'
#' ## TWo tables
#' df <- expandTables(
#'     male = c(36, 14, 50, 50),
#'     female = c(24, 126, 10, 90),
#'     exp_name = "areaType", exp_lvl = c("Rural", "Urban"),
#'     case_name = "Antibodies", case_lvl = c("Yes", "No"),
#'     strata_name = "gender")
#' tab(df)
#' tab(df, areaType, by = Antibodies)
#'
#'
#'
#' ### checking numbers
#' df %>%
#'     pick(gender == "male") %>%
#'     tab(areaType, Antibodies)
#'
#' df %>%
#'     pick(gender == "female") %>%
#'     tab(areaType, Antibodies)
#' }


#' @export
expandTables <- function( ... ,
                          exp_name = "exposure",
                          exp_lvl = c("exposed", "unexposed"),
                          case_name = "outcome",
                          case_lvl = c("disease", "healthy"),
                          strata_name = "strata",
                          stringsAsFactors = FALSE)
{
  arguments <- as.list(match.call())
  arguments <- arguments[-1]
  arg_name <- names(arguments)
  extraArg <- c("exp_name", "exp_lvl", "case_name", "case_lvl",
                "strata_name")
  if (any(arg_name %in% extraArg)) {
    arguments <- arguments[!(arg_name %in% extraArg)]
  }
  strata_lvl <- names(arguments)
  if (is.null(strata_lvl) | length(strata_lvl) < 2) {
    times <- as.numeric(as.character(arguments[[1]])[-1])
    exp <- c(exp_lvl[1], exp_lvl[1], exp_lvl[2], exp_lvl[2])
    case <- c(case_lvl[1], case_lvl[2], case_lvl[1],
              case_lvl[2])
    t <- as.data.frame(
      cbind("exposure" = rep(exp, times),
            "case" = rep(case, times)),
      stringsAsFactors = stringsAsFactors)
    names(t) <- c(exp_name, case_name)
  } else {
    t <- do.call(
      rbind,
      lapply(strata_lvl, function(z) {
        times <- as.numeric(as.character(arguments[[z]])[-1])
        exp <- c(exp_lvl[1], exp_lvl[1], exp_lvl[2], exp_lvl[2])
        case <- c(case_lvl[1], case_lvl[2], case_lvl[1],
                  case_lvl[2])
        as.data.frame(
          cbind("exposure" = rep(exp, times),
                "case" = rep(case, times),
                "strata" = z),
          stringsAsFactors = stringsAsFactors)
      })
    )
    names(t) <- c(exp_name, case_name, strata_name)
  }

  return(t)
}
