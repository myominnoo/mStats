#' @title `Keep`, `drop` and `filter` variables; `arrange` observations
#'
#' @description
#'
#' \code{keep()} or \code{drop()} variables within dataframe
#'
#' \code{arrange()} sorts variables or columns
#'
#' \code{filter()} keeps observations that satisfy specified conditions
#'
#' @param data Dataset
#' @param ... Variables or conditions in \code{filter()}
#'
#' @details
#'
#' # Select Variables
#' \code{keep()} removes unspecified variables from the dataset.
#' Variables are also rearranged based on the order they are mentioned.
#'
#' \preformatted{keep(data, var1, var2, var3, etc)}
#'
#' \code{drop()} works the opposite of \code{keep()} removed specified variables
#' from the dataset.
#'
#' \preformatted{drop(data, var5, var6, var7, etc)}
#'
#' # Sort
#'
#' \code{arrange()} sorts the dataset by specifying variables. The minus symbol `-`
#' indicates descending order.
#'
#' \preformatted{arrange(data, var1, var2, -var3, var4)}
#'
#'
#' # Filter
#'
#' \code{filter} keeps observations that meet the specified conditions. If conditions
#' are specified by comma, they are joined by `AND` operators
#'
#' \preformatted{filter(data, var1 == var2 | var3 > var4)}
#'
#' \preformatted{filter(data, var1 == var2, var3 > var4)}
#'
#'
#' @return
#' Modified Dataset
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
#'
#' ## use infert data
#' data(infert)
#' codebook(infert)
#'
#' ## DEMONSTRATION: KEEP
#' ## suppose we want to keep education, age, induced and case
#' infert.new <- keep(infert, education, age, induced, case)
#' codebook(infert.new)
#'
#' ## use colon separator :
#' infert.new <- keep(infert, education:case)
#' codebook(infert.new)
#'
#' ## change the order of variables
#' infert.new <- keep(infert, stratum, pooled.stratum, education:spontaneous)
#' codebook(infert.new)
#'
#'
#' \dontrun{
#' ## DEMONSTRATION: drop
#'
#' ## suppose we want to remove education, age, induced and case
#' infert.new <- drop(infert, education, age, induced, case)
#' codebook(infert.new)
#'
#' ## use colon separator :
#' infert.new <- drop(infert, education:case)
#' codebook(infert.new)
#'
#'
#'
#' ## DEMONSTRATION: filter
#'
#' ## suppose we want to remove rows with parity less than 5 or less than or equal to 4
#' tab(infert, parity)
#' infert.new <- filter(infert, parity < 5)
#' tab(infert.new, parity)
#'
#' ## we want parity value between 2 and 5
#' infert.new <- filter(infert, parity > 1, parity < 6)
#' tab(infert.new, parity)
#'
#' ## or we can write like this.
#' infert.new <- filter(infert, parity >= 2 & parity <= 5)
#' tab(infert.new, parity)
#'
#' ## DEMONSTRATION: arrange
#'
#' ## arrange by education
#' arrange(infert, education)
#' arrange(infert, -education)
#' arrange(infert, education, age, case)
#'
#' }
#'
#' @export
keep <- function(data, ... )
{
    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    ## get variables' names within three dots
    .vars.names <- as.character(enquos(.args, "data"))

    ## Check if colon is there.
    ## if present, retrieve variables between the two variables
    if (any(grepl(":", .vars.names))) {
        .vars.names <- do.call(
            c,
            lapply(.vars.names, function(z) {
                .colon <- grepl(":", z)
                if (.colon) {
                    splitByColon(data, z, .colon)
                } else {
                    z
                }
            })
        )
    }

    .data <- data[, .vars.names]

    ## Display message to nofity changes
    printMsg(paste0(length(.vars.names), " variables kept: ",
                    paste(.vars.names, collapse = ", ")))

    return(.data)
}





#' @rdname keep
#' @export
drop <- function(data, ... )
{
    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    ## get variables' names within three dots
    .vars.names <- as.character(enquos(.args, "data"))

    ## Check if colon is there.
    ## if present, retrieve variables between the two variables
    if (any(grepl(":", .vars.names))) {
        .vars.names <- do.call(
            c,
            lapply(.vars.names, function(z) {
                .colon <- grepl(":", z)
                if (.colon) {
                    splitByColon(data, z, .colon)
                } else {
                    z
                }
            })
        )
    }

    .data <- data[, !(names(data) %in% .vars.names)]

    ## Display message to nofity changes
    printMsg(paste0(length(.vars.names), " variables dropped: ",
                    paste(.vars.names, collapse = ", ")))

    return(.data)
}





#' @rdname keep
#' @export
arrange <- function(data, ... )
{
    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    ## assign data into .data for further evaluation
    .data <- data
    .vars.names <- names(data)


    ## get variables' names within three dots
    .vars <- as.character(enquos(.args, "data"))
    # If length is 0, then this is equal to all variables
    .vars.len <- length(.vars)
    if (.vars.len == 0) {
        .vars <- .vars.names
    }


    ## check minus symbol and remove it
    .minus.contain <- grepl("-", .vars)
    .vars <- gsub("-", "", .vars)


    ## subset .data with .vars
    .data <- .data[, .vars]
    .vars.type <- sapply(.data, function(z) class(unlist(z))[1])


    ## assing minus symbol back to elements and collapse to put them into order()
    .vars.type <- ifelse(.vars.type %in% c("character", "factor"),
                         paste0("as.numeric(as.factor(", .vars, "))"),
                         .vars)
    .vars.type <- ifelse(.minus.contain, paste0("-", .vars.type), .vars.type)


    ## create expression
    .expr <- paste0(.vars.type, collapse = ", ")

    ## .data reassign for evaluation
    .data <- data

    ## formulate text syntax, evaluate and assign to .data
    .expr.txt <- paste0(".data[with(.data, order(", .expr, ")), ]")

    tryCatch({
        .data <- eval(parse(text = .expr.txt))
    }, error = function(cnd) {
        stop(" ... Expression cannot be evaluated! ... ")
    })

    for (i in 1:length(.vars.names)) {
        .lbl <- attr(data[[.vars.names[i]]], "label")
        if (length(.lbl) > 1) {
            .lbl <- paste0(.lbl, collapse = ", ")
        }
        attr(.data[[.vars.names[i]]], "label") <- .lbl
    }

    ## Display message to nofity changes
    printMsg(paste0("Expression used to arrange: '", .expr.txt, "'"))
    printMsg(paste0(nrow(.data), " observations arranged"))

    return(.data)
}


#' @rdname keep
#' @export

filter <- function(data, ... )
{
    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    ## assign data into .data for further evaluation
    .data <- data

    ## get variables' names within three dots
    .expr <- as.character(enquos(.args, "data"))


    ## if more than one expression, combine with & operator
    if (length(.expr) > 1) {
        .expr <- paste0("(", .expr, ")", collapse = " & ")
    }

    ## formulate text syntax, evaluate and assign to .data
    .expr.txt <- paste0(".data[with(.data, (", .expr, ")), ]")
    tryCatch({
        .data <- eval(parse(text = .expr.txt))
    }, error = function(cnd) {
        stop(" ... Expression cannot be evaluated! ... ")
    })



    ## add labels back
    .data.names <- names(data)
    for (i in 1:length(.data.names)) {
        attr(.data[[.data.names[i]]], "label") <- attr(data[[.data.names[i]]], "label")
    }



    ## Display message to nofity changes
    printMsg(paste0("Expression used to filter: '", .expr.txt, "'"))
    printMsg(paste0(nrow(.data), " observations filtered."))

    return(.data)
}
