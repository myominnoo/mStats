#' @title Change contents of an existing variable
#'
#' @description
#'
#' \code{replace()} alters the values of a variable when specified
#' conditions are met.
#'
#'
#' \preformatted{replace(data, var, value,
#'      var < somevalue | var > somevalue, is.na(var))}
#'
#'
#' If conditions are not specified, \code{replac()} changes the whole
#' variable with specified value.
#'
#' \preformatted{replace(data, var, value)}
#'
#' @param data Dataset
#' @param var Variable
#' @param value Replacement value
#' @param ... `if` conditions.
#'
#' @details
#'
#' It is used when multiple conditions have to be met to change a value.
#' The function first checks whether specified value is a variable of the dataset.
#' If yes, then the values are replaced with those of that variables
#' with the conditions.
#'
#'
#' @return
#' Modified Dataset
#'
#' @concept
#'
#' replace recode change values
#'
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
#'
#'
#' # Example from IDRE UCLA
#' path <- "https://stats.idre.ucla.edu/stat/data/patient_pt1_stata_dm.dta"
#' hosp <- haven::read_dta(path)
#' codebook(hosp)
#'
#' ## to use piping function
#' library(magrittr)
#'
#' hosp %>% replace(pain, 0, pain < 4)
#' hosp %>% replace(pain, NA, pain < 4)
#'
#' ## conditions separated by comma as `AND` statement
#' hosp %>% replace(pain, 0, pain < 4, mobility < 2)
#'
#' ## Conditions not specified
#' hosp %>% replace(pain, 0)
#' hosp %>% replace(pain, NA)
#'
#'
#' ## Mixed multiple conditions met
#' hosp %>% replace(co2, NA, mobility == 2, pain < 4)
#' hosp %>% replace(co2, NA, mobility == 2, pain < 4, tumorsize < 70)
#'
#'
#' ## replace with values from another variable
#' hosp %>% replace(pain, mobility, pain < 4)
#' hosp %>% replace(co2, tumorsize, mobility == 2, pain < 4)
#' hosp %>% replace(co2, tumorsize, mobility == 2, pain < 4, tumorsize < 70)
#'
#'
#' ## convert data type without expression
#' replace(hosp, wbc, as.numeric(wbc)) %>%
#'     codebook
#'
#' replace(hosp, wbc, sex) %>%
#'     keep(sex, wbc)
#' }
#'
#' @export
replace <- function(data, var, value, ... )
{
    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", deparse(substitute(data)), "' is not data.frame ... "))

    .args <- as.list(match.call())

    ## assign data into .data for further evaluation
    .data <- data

    ## variable name
    .var.name <- as.character(.args$var)



    ## get variables' names within three dots
    .expr <- as.character(enquos(.args, c("data", "var", "value")))

    ## if more than one expression, combine with & operator
    if (length(.expr) > 1) {
        .expr <- paste0("(", .expr, ")", collapse = " & ")
    } else if (length(.expr) == 0) {
        .expr <- NULL
    }



    ## check if value is a variable
    .value.var <- FALSE
    if (any(names(data) %in% as.character(.args$value))) {
        if (length(as.character(.args$value)) > 1) {
            value <- deparse(substitute(value))
        } else {
            .value.var <- TRUE
            .value <- as.character(.args$value)
            assign(.value, eval(parse(text = paste0("data$", .value))))
            value <- .value
        }
    } else {
        ## check if value is a character
        value <- ifelse(is.character(value), paste0("'", value, "'"), value)
    }



    ## formulate text syntax, evaluate and assign to .data
    if (.value.var) {
        .expr.txt <- paste0("within(.data, ", .var.name,
                            "[", .expr, "] <- ", value,
                            "[", .expr, "])")
    } else {
        .expr.txt <- paste0("within(.data, ", .var.name,
                            "[", .expr, "] <- ", value, ")")
    }

    if (is.null(.expr)) {
        .expr.txt <- paste0("within(.data, ", .var.name, " <- ", value, ")")
    }

    ## if var is labelled num, removed labels
    attr(.data[[.var.name]], "names") <- NULL
    attr(.data[[.var.name]], "labels") <- NULL



    ## getting the data
    tryCatch({
        if (length(.expr) == 0) {
            .changed.values.count <- .data[[.var.name]]
        } else {
            .changed.values.count <- eval(parse(text = (
                paste0("with(.data, ", .var.name, "[with(.data, ",
                       .expr, ")])")
            )))
        }
        .data <- eval(parse(text = .expr.txt))
    }, error = function(cnd) {
        stop(" ... Expression cannot be evaluated! ... ")
    })

    ## Display message to nofity changes
    printMsg(paste0("Expression used to replace: '", .expr.txt, "'"))
    printMsg(paste0(length(.changed.values.count),
                    " observations replaced with",
                    ifelse(.value.var,
                           paste0(" the values from '", value),
                           paste0(" '", value))
                    , "'"))

    return(.data)
}
