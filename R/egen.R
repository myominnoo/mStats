#' @title An extension to generate function
#'
#' @description
#' \code{egen} transforms a numeric vector to a factor vector.
#'
#' @param data dataframe
#' @param old.var variable to perform cut
#' @param cut either a single number or a numeric vector.
#' @param lbl specify to name the factor levels.
#' @param new.var name of new variable generated
#' @param na.rm A logical value to specify missing values
#' @details
#' \code{egen} allows easy conversion of a numeric vector to factor.
#'
#' \strong{Cut-off Intervals}
#' If the interval is not specified, it is cut at an interval of 10. Otherwise,
#' it is divided into equal cut-off points by specified number.
#'
#' \strong{Labelling}
#' If not specified, the labels are constructed in the format:
#' \strong{variable name} + "." + \strong{"cut-off intervals"}.
#' @seealso \code{\link{generate}}
#' @keywords distribution, number summary, correlation
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#' ## variable with dataframe
#' # automatically categorized into interval of 10
#' infert.new <- egen(infert, age)
#' str(infert.new)
#' tab(age.cat, infert.new)
#'
#' infert.new <- egen(infert, age, c(30, 35, 40))
#' str(infert.new)
#' tab(age.cat, infert.new)
#'
#' infert.new <- egen(infert, age, c(30, 35, 40),
#'                 c("young", "mid", "old", "oldest"))
#' str(infert.new)
#' tab(age.cat, infert.new)
#'
#' # give variable name as age.grp
#' infert.new <- egen(infert, age, c(30, 35, 40),
#'                 c("young", "mid", "old", "oldest"), age.grp)
#' str(infert.new)
#' tab(age.grp, infert.new)
#'
#' # age.grp duplicates
#' infert.new1 <- egen(infert.new, age, c(30, 35, 40),
#'                 c("young", "mid", "old", "oldest"), age.grp)
#' }


#' @export
egen <- function(data = NULL, old.var, cut = NULL, lbl = NULL,
                 new.var = NULL, na.rm = FALSE)
{
    arguments <- as.list(match.call())
    old.var.name <- unlist(arguments$old.var)

    if (is.null(data)) {
        if (!is.null(arguments$new.var))
            stop("... no need to specify 'new.var' ...")
        data <- as.numeric()
    } else {
        if (!is.data.frame(data)) {
            stop("... specify a dataframe or NULL in the fist argument ...")
        }
        data <- data.frame()
    }
    UseMethod("egen", data)
}


#' @rdname egen
#' @export
egen.default <- function(data = NULL, old.var, cut = NULL, lbl = NULL,
                         new.var = NULL, na.rm = FALSE)
{
    stop("... Wrong data type ...")
}

egen.numeric <- function(data = NULL, old.var, cut = NULL, lbl = NULL,
                         new.var = NULL, na.rm = FALSE)
{
    arguments <- as.list(match.call())
    old.var.name <- arguments$old.var
    if (na.rm) old.var <- old.var[!is.na(old.var)]

    if (is.null(cut)) {
        cut <- 10
        old.var.brk <- seq(min(old.var, na.rm = TRUE),
                           max(old.var, na.rm = TRUE),
                           cut)
        old.var.brk <- c(old.var.brk[1],
                         old.var.brk[2:(length(old.var.brk)-1)] - 1,
                         old.var.brk[length(old.var.brk)] - 1)
    }  else {
        if (length(cut) == 1) {
            old.var.brk <- seq(min(old.var, na.rm = TRUE),
                               max(old.var, na.rm = TRUE),
                               cut)
            old.var.brk <- c(old.var.brk[1],
                             old.var.brk[2:(length(old.var.brk)-1)] - 1,
                             old.var.brk[length(old.var.brk)] - 1)
        } else {
            old.var.brk <- cut
        }
    }

    old.var.min <- min(old.var, na.rm = TRUE)
    old.var.brk.min <- old.var.brk[1]
    if (old.var.min < old.var.brk.min) {
        old.var.brk <- c(old.var.min, old.var.brk)
    }
    if (old.var.brk.min < old.var.min) {
        old.var.brk <- c(old.var.min, old.var.brk)
        old.var.brk <- old.var.brk[old.var.brk >= old.var.min]
    }
    old.var.max <- max(old.var, na.rm = TRUE)
    old.var.brk.max <- old.var.brk[length(old.var.brk)]

    if (old.var.brk.max < old.var.max) {
        old.var.brk <- c(old.var.brk, old.var.max)
    } else {
        old.var.brk <- old.var.brk[old.var.brk < old.var.max]
        if (old.var.max != max(old.var.brk, na.rm = TRUE))
            old.var.brk <- c(old.var.brk, old.var.max)
    }

    old.var.brk.len <- length(old.var.brk)
    if (is.null(lbl)) {
        old.var.lbl.lwr <- c(old.var.brk[1],
                             old.var.brk[-c(1, old.var.brk.len)])
        old.var.lbl.upr <- c(old.var.brk[2:(old.var.brk.len - 1)] - 1 ,
                             old.var.brk[old.var.brk.len])
        old.var.lbl <- paste0("l.",
                              paste(old.var.lbl.lwr, old.var.lbl.upr, sep = "-"))
    } else {old.var.lbl <- lbl}

    old.var <- cut(old.var, breaks = old.var.brk, labels = old.var.lbl, right = FALSE,
                   include.lowest = TRUE)

    printMsg(paste0(length(old.var), " values generated with labels: ",
                    paste0(old.var.lbl, collapse = ", ")))
    return(old.var)
}


#' @rdname egen
#' @export
egen.data.frame <- function(data = NULL, old.var, cut = NULL, lbl = NULL,
                            new.var = NULL, na.rm = FALSE)
{
    arguments <- as.list(match.call())
    data.name <- arguments$data
    old.var.name <- gsub(" ", "", arguments$old.var)
    old.var <- data[, old.var.name]
    new.var.name <- as.character(arguments$new.var)
    if (length(new.var.name) == 0)
        new.var.name <- paste0(old.var.name, ".cat")
    if (any(names(data) %in% new.var.name))
        stop(paste0("... ", new.var.name, " already exists. Specify a new name ..."))
    new.var <- egen.numeric(NULL, old.var, cut = cut, lbl = lbl, na.rm = na.rm)
    data <- cbind(data, new.var)
    names(data)[length(data)] <- new.var.name

    printMsg(paste0("'", new.var.name,
                    "' created & appended to dataframe '", data.name, "'"))

    return(data)
}



