#' @title List and view the observations of variables from a dataset
#' @description
#' \code{listView} display a list of observations of variables from
#' a dataset. If \code{vars} is not specified, the observations of
#' all the variables will be displayed.
#' @param data a data frame object
#' @param vars either one variable or a vector of variables or two
#' variables separated by colon (x:y)
#' @param head A logical value to specify heading or tailing values
#' @param nrow specify the number of observations
#' @param ... optional arguments
#' @import utils
#' @seealso \code{\link{codebook}}
#' @keywords list, display, view, head, tail
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#' listView(infert, education)
#' listView(infert, education:stratum)
#' listView(infert, case:stratum)
#' listView(infert, case:stratum, head = FALSE)
#' listView(infert, c(education, case, parity))
#' listView(infert)
#' }


#' @export
listView <- function(data, vars = NULL, head = TRUE, nrow = 6)
{
    arguments <- as.list(match.call())
    vars <- (deparse(substitute(vars)))

    if (!is.data.frame(data))
        printMsg(" >>> use a data.frame <<< ")

    if (grepl(":", vars)) {
        x <- list()
    } else if (grepl("c\\(", vars)) {
        x <- list()
    } else if (vars == "NULL") {
        x <- data.frame()
    } else {
        x <- character()
    }

    UseMethod("listView", x)
}


#' @rdname listView
#' @export
listView.default <- function(...) {
    printMsg(" >>> Try listView(infert) <<< ")
}


#' @rdname listView
#' @export
listView.character <- function(data, vars = NULL, head = TRUE, nrow = 6)
{
    arguments <- as.list(match.call())
    vars <- (deparse(substitute(vars)))
    if (head) {
        head(data[, vars], n = nrow)
    } else {
        tail(data[, vars], n = nrow)
    }
}

#' @rdname listView
#' @export
listView.list <- function(data, vars = NULL, head = TRUE, nrow = 6)
    {
    arguments <- as.list(match.call())
    vars <- (deparse(substitute(vars)))

    if (grepl(":", vars)) {
        varsNames <- unlist(strsplit(vars, split = ":"))
        varsNames <- paste0("^", varsNames, "$")
        varsNames <- names(data)[grep(varsNames[1], names(data)):
                                 grep(varsNames[2], names(data))]
    } else {
        varsNames <- unlist(strsplit(gsub("^c\\(|\\)$| ", "", vars), ","))
    }
    if (head) {
        head(data[, varsNames], n = nrow)
    } else {
        tail(data[, varsNames], n = nrow)
    }
}

#' @rdname listView
#' @export
listView.data.frame <- function(data, vars = NULL, head = TRUE, nrow = 6)
{
    if (head) {
        head(data, n = nrow)
    } else {
        tail(data, n = nrow)
    }
}

