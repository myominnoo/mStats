#' @title An extension to \code{generate}
#'
#' @description
#' \code{egen()} transforms a numeric vector to a factor vector.
#'
#' @param data dataset
#' @param var_old variable to perform cut
#' @param cut either a single number or a numeric vector.
#' @param var_new name of new variable generated
#' @param lbl specify to name the factor levels.
#' @param na.rm A logical value to specify missing values
#'
#' @details
#' \code{egen} allows easy conversion of a numerical variable to a categorical
#' variable.
#'
#' \strong{Cut-off Intervals}
#'
#'
#' If the interval is not specified, it is cut at an interval of 10,
#' starting from the minimum value. Otherwise,
#' it is divided into corresponding intervals by specified cut-off points.
#'
#' \strong{Automatic Labelling}
#'
#'
#' If \code{lbl} is not specified, labels are constructed in
#' this format: \code{lbl[##-##]}
#'
#' @seealso
#'
#' \code{\link{generate}}, \code{\link{duplicates}}, \code{\link{listView}}
#'
#' @keywords
#'
#' new variable, generate, produce, create, transform, conversion, transform
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
#' ## variable with dataframe
#' # automatically categorized into interval of 10
#' infert.new <- egen(infert, age)
#' str(infert.new)
#'
#' infert.new <- egen(infert, age, c(31, 36, 41))
#' str(infert.new)
#'
#' # give variable name as age.grp
#' infert.new <- egen(infert, age, c(31, 36, 41), age.grp,
#'                 c("young", "mid", "old", "oldest"))
#' str(infert.new)
#'
#' # age.grp duplicates
#' infert.new <- egen(infert.new, age, c(31, 36, 41), age.grp,
#'                 c("young", "mid", "old", "oldest"))
#' str(infert.new)
#'
#' # Example dataset from IDRE website
#' path <- "https://stats.idre.ucla.edu/stat/data/patient_pt1_stata_dm.dta"
#' hosp <- haven::read_dta(path)
#' hosp <- egen(hosp, bmi, c(16, 18.5, 25, 30, 35, 40))
#' str(hosp)
#' }


#' @export
egen <- function(data = NULL, var_old, cut = NULL,
                 var_new = NULL, lbl = NULL, na.rm = FALSE,
                 print.notes = TRUE)
{
    arguments <- as.list(match.call())

    if (is.null(data)) {
        x <- numeric()
    } else {
        if (!is.data.frame(data)) {
            stop(" >>> Data must be data.frame <<< ")
        }
        x <- data.frame()
    }
    UseMethod("egen", x)
    typeof(x)
}


#' @param print.notes logical value to indicate whether notes will be
#' printed
#' @rdname egen
#' @export
egen.numeric <- function(data = NULL, var_old, cut = NULL,
                         lbl = NULL, na.rm = FALSE,
                         print.notes = TRUE)
{
    arguments <- as.list(match.call())
    var_old.name <- deparse(substitute(var_old))
    if (!is.null(data)) {
        var_old <- eval(substitute(var_old), data)
    }

    if (na.rm) var_old <- var_old[!is.na(var_old)]

    if (is.null(cut)) {
        cut <- 10
        var_new.brk <- seq(min(var_old, na.rm = TRUE),
                           max(var_old, na.rm = TRUE),
                           cut)
        var_new.brk <- c(var_new.brk[1],
                         var_new.brk[2:(length(var_new.brk)-1)] - 1,
                         var_new.brk[length(var_new.brk)] - 1)
    } else {
        if (length(cut) == 1) {
            var_new.brk <- seq(min(var_old, na.rm = TRUE),
                               max(var_old, na.rm = TRUE),
                               cut)
            var_new.brk <- c(var_new.brk[1],
                             var_new.brk[2:(length(var_new.brk)-1)] - 1,
                             var_new.brk[length(var_new.brk)] - 1)
        } else {
            var_new.brk <- cut
        }
    }
    var_old.min <- min(var_old, na.rm = TRUE)
    var_new.brk <- var_new.brk[var_old.min < var_new.brk]
    if (var_old.min != var_new.brk[1]) {
        var_new.brk <- c(var_old.min, var_new.brk)
    }

    var_old.max <- max(var_old, na.rm = TRUE)
    var_new.brk <- var_new.brk[var_old.max > var_new.brk]
    if (var_old.max != var_new.brk[length(var_new.brk)]) {
        var_new.brk <- c(var_new.brk, var_old.max)
    }

    decimal <- grepl("\\.", cut)
    if (any(decimal)) {
        decimal <- strsplit(as.character(cut[decimal][1]), "\\.")[[1]][2]
        decimal <- nchar(decimal)
    } else {
        decimal <- 0
    }
    var_new.brk <- round(var_new.brk, decimal)

    if (is.null(lbl)) {
        lastSec <- length(var_new.brk) - 1
        var_new.lbl <- paste(
            "lbl[", var_new.brk[1:lastSec], "-",
            c(var_new.brk[2:lastSec] - (1 / (10 ^ decimal)),
              var_new.brk[length(var_new.brk)]), "]",
            sep = ""
        )
    } else {var_new.lbl <- lbl}

    var_new <- cut(var_old, breaks = as.numeric(var_new.brk),
                   labels = var_new.lbl,
                   right = FALSE,
                   include.lowest = TRUE)
    if (print.notes) {
        printMsg(paste0(length(var_new), " values generated from '",
                        var_old.name, "'"))
        printMsg(paste0("Labels created: ",
                        paste0(var_new.lbl, collapse = ", ")))
    }
    return(var_new)
}

#' @rdname egen
#' @export
egen.data.frame <- function(data = NULL, var_old, cut = NULL,
                            var_new = NULL, lbl = NULL, na.rm = FALSE)
{
    arguments <- as.list(match.call())
    data.name <- arguments$data
    var_new.name <- arguments$var_new
    vars.names <- names(data)

    var_old.name <- gsub(" ", "", arguments$var_old)
    data <- data.frame(data)
    var_old <- data[, var_old.name]

    var_new <- egen.numeric(NULL, var_old, cut, lbl, na.rm, FALSE)
    data <- cbind(data, var_new)

    if (is.null(var_new.name)) {
        var_new.name <- paste0(var_old.name, ".cat")
    } else {
        var_new.name <- as.character(var_new.name)
    }

    if (any(names(data) %in% var_new.name)) {
        stop(paste0(" >>> variable '", var_new.name, "' already existed.",
                    " <<< "))
    }

    names(data)[length(data)] <- var_new.name
    printMsg(paste0("Generated ", length(var_new), " values into '",
                    var_new.name, "' from '", var_old.name, "'"))
    return(data)
}
