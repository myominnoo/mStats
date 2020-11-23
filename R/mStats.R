
# DATA MANAGEMENT ---------------------------------------------------------


#' @title Describe the data
#'
#' @description
#' \code{codebook()} examines the variable names, labels, and data
#' to produce a codebook for describing the dataset
#'
#' @param data data.frame
#'
#' @details
#'
#' It reports a description of the data with the following information.
#'
#' \strong{ANNOTATIONS}:
#'
#' `No` = serial number
#'
#' `Variable` = variable name
#'
#' `Label` = variable label
#'
#' `Type` = type of variable
#'
#' `Obs` = number of valid observations
#'
#' `NA` = number of observations with missing value `NA`
#'
#' @note
#'
#' For `haven_labelled` data.frame, data types are generated
#' using `typeof()`.
#'
#' @return
#'
#' a data.frame containing the codebook
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' codebook(infert)
#' codebook(iris)
#' codebook(mtcars)
#'
#' @export
codebook <- function(data)
{
    ## if data is not a data.frame, stop
    .data_name <- deparse(substitute(data))
    if (!is.data.frame(data)) {
        stop(paste0("`", .data_name, "` must be a data.frame"),
             call. = FALSE)
    }

    ## retrieve information
    .data_lbl <- attr(data, "label")
    if (length(.data_lbl) != 1) .data_lbl <- "-"
    .obs_no <- nrow(data)
    .vars_no <- ncol(data)
    .vars_name <- names(data)
    .vars_label <- sapply(data, function(z) {
        .lbl <- attr(z, "label")
        if (length(.lbl) > 1 | is.null(.lbl)) {
            NA
        } else {
            ifelse(nchar(.lbl) > 30,
                   paste0(strtrim(.lbl, 30), "..."), .lbl)
        }
    })
    # .vars_type <- sapply(data.frame(data), typeof)
    .vars_type <- sapply(data, function(z) {
        .class <- class(unlist(z))[1]
        if (.class == "haven_labelled") {
            .class <- typeof(unlist(z))[1]
        }
        .class
    })
    .obs <- sapply(data, function(z) length(z[!is.na(z)]))
    .na <- sapply(data, function(z) length(z[is.na(z)]))

    ## combine them into a data.frame
    .df <- data.frame(1:.vars_no,
                      .vars_name,
                      .vars_label,
                      .vars_type,
                      .obs,
                      .na)
    ## change column names and reset row names
    names(.df) <- c("No", "Variable", "Label", "Type",
                    "Obs", "<NA>")
    row.names(.df) <- NULL
    ## add horizontal and vertical lines
    .df <- formatdf(.df, 2, 3)


    ## Display information
    printDFlenLines(.df)
    cat(paste0("       Codebook : ", .data_name, "\n"))
    printDFlenLines(.df)
    cat(paste0("  Dataset Label : ", .data_lbl, "\n"))
    cat(paste0("            Obs : ", .obs_no, "\n"))
    cat(paste0("           Vars : ", .vars_no, "\n"))
    print.data.frame(.df, row.names = FALSE, max = 1e9)


    ## return
    invisible(.df)
}




#' @title Attach labels to data and variables
#'
#' @description
#' \code{label()} manipulates labels
#'
#' @param data data.frame
#' @param ... For variable label, `Var = "Var Label"`:
#' For data label, `"Example data lable"`.
#'
#' @details
#'
#' \strong{Attach labels}
#'
#' It has two inputs. If only one label is specified, that
#' label is attached to the data. Otherwise, the pattern
#' `Var = "Var Label"` are used to attach labels to variables.
#'
#' \strong{Remove labels}
#'
#' `NA` or `NULL` is used to remove labels.
#'
#' @return
#' data.frame
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' ## Variable label
#' x <- label(infert,
#'            education = "Education levels",
#'            age = "Age in years of case",
#'            parity = "count",
#'            stratum = "1-83",
#'            pooled.stratum = "1-63")
#'
#' ## Data label
#' x <- label(x, "Infertility and Abortion Dataset")
#' codebook(x)
#'
#' @export
label <- function(data, ... )
{
    ## if data is not a data.frame, stop
    .data_name <- deparse(substitute(data))
    if (!is.data.frame(data)) {
        stop(paste0("`", .data_name, "` must be a data.frame"),
             call. = FALSE)
    }

    ## match call arguments
    .args <- as.list(match.call())

    ## get the names within three dots
    .vars <- .args[-c(1:2)]
    ## if .vars list does not have a name, it means the label for
    # the dataset. Otherwise, it's for the variables
    if (length(.vars) == 1 & names(.vars)[1] == "") {
        .vars <- unlist(.vars)
        attr(data, "label") <- .vars
        cat(paste0("  (Dataset '", .data_name, "' labeled as '",
                   .vars, "')\n"))
    } else {
        sapply(names(.vars), function(z) {
            ## check if all vars specified are in the dataset
            if (!(z %in% names(data))) {
                stop(paste0("Variable '", z, "' not found in the dataset"),
                     call. = FALSE)
            }
            attr(data[[z]], "label") <<- .vars[[z]]
            cat(paste0("  (Variable '", z, "' labeled as '",
                       .vars[[z]], "')\n"))
        })
    }

    return(data)
}



#' @title Recode a variable
#'
#' @description
#' \code{recode()} changes the values of a variable.
#'
#' @param data data.frame
#' @param var variable name
#' @param ... specify in pattern: `old value` / `new value`.
#'
#' @details
#'
#' It changes the values of a variable according to the old values
#' specified. Values that does not meet any of the conditions,
#' they are left unchanged.
#'
#' \strong{Using colon `:` to indicate a range of numeric numbers}
#'
#' A numeric vector can be indicated by using `:` in `old value`.
#' The function automatically filters the values that meet the
#' range and assigns a specified new value to these.
#'
#' @return
#' a data.frame
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' x <- recode(infert, case, 0/"No", 1/"Yes")
#' tab(x, case)
#'
#' \dontrun{
#' ## recode a factor
#' x <- recode(infert, education, "0-5yrs"/1, "6-11yrs"/2, "12+ yrs"/3)
#' tab(x, education)
#'
#' ## recode numeric vectors
#' x <- recode(infert, age, 21:28.9/1, 29:34.9/2, 35:44/3)
#' tab(x, age)
#'
#' ## recode NA
#' infert[4:20, "case"] <- NA
#' x <- recode(infert, case, NA/"Missing value")
#' tab(infert, case)
#' }
#'
#' @export
recode <- function(data, var, ... )
{
    ## match call arguments
    .args <- as.list(match.call())

    var <- .args$var
    .var <- data[[var]]
    .lbl <- attr(.var, "label")
    ## change double to numeric
    if (is.double(.var)) {
        .var <- as.numeric(.var)
    } else if (is.factor(.var)) {
        .var <- as.character(.var)
    }
    vals <- .args[-c(1:3)]
    lapply(vals, function(z) {
        .val <- as.character(z)
        .old <- ifelse(.val[2] == "NA", NA, .val[2])
        .new <- ifelse(.val[3] == "NA", NA,
                       ifelse(.val[3] == "NULL", NULL, .val[3]))

        .chk <- .var == .old
        if (grepl(":", .old)) {
            .old <- eval(parse(text = .old))
            .chk <- .var >= .old[1] & .var <= .old[length(.old)]
        } else if (is.na(.old)) {
            .chk <- is.na(.var)
        }
        if (any(.chk)) {
            if (any(is.na(.old))) {
                .var[is.na(.var)] <<- .new
            } else {
                .var[.chk] <<- .new
            }
        } else {
            stop(paste0("`", .old, "` not found in '", var, "'"),
                 call. = FALSE)
        }

        ## Print notification message
        cat(paste0("  ( ", length(which(.chk)), " values recoded as '",
                   .new, "')\n"))
    })

    if (is.factor(data[[var]])) .var <- factor(.var)
    attr(.var, "label") <- .lbl
    data[[var]] <- .var

    return(data)
}


#' @title Create a new variable
#'
#' @description
#'
#' \code{generate()} creates a new variable either by
#' deriving from existing variables or with a constant value.
#'
#' @param data data.frame
#' @param var name for the new variable
#' @param expr a constant value, name of an existing variable or
#' an expression for simple arithmetic or logical operations:
#'
#' @details
#'
#' The values of the variable are specified by \code{expr}.
#'
#' \strong{Label}
#'
#' The newly created variable is automatically labeled with
#' the expression specified.
#'
#' @return
#'
#' data.frame with the new variable
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' ## generate variable with a constant value
#' generate(mtcars, new_var, NA)
#' generate(mtcars, new_var, 99)
#'
#' ## generate variable from an existing variable
#' generate(mtcars, new_var, mpg)
#'
#' ## generate variable with arithmetic operations
#' generate(iris, Length, Sepal.Length + Petal.Length)
#'
#' @export
generate <- function(data, var, expr = NULL )
{
    ## match call arguments
    .args <- as.list(match.call())

    ## get names of dataset and headings
    .data_name <- deparse(substitute(data))
    .vars_names <- names(data)

    ## if input is not a data.frame, stop
    if (!is.data.frame(data)) {
        stop(paste0("`", .data_name, "` must be a data.frame"),
             call. = FALSE)
    }

    ## get variable name
    var <- as.character(.args$var)

    ## if variable already exisits, then stop
    if (any(.vars_names %in% var)) {
        stop(paste0("'", var, "' already exisits in the dataset"),
             call. = FALSE)
    }

    ## create expression text
    ## if value is more than one vector, then collapse it
    expr <- paste0(deparse(substitute(expr)), collapse = "")
    .expr <- paste0("with(data, ", expr, ")")


    ## add new var to the dataset
    ## add label
    tryCatch({
        data$var <- eval(parse(text = .expr))
    }, error = function(cnd) {
        stop(cnd)
    })
    attr(data$var, "label") <- expr

    ## get missing value and total number
    ## and print
    .var_miss <- sum(is.na(data$var))
    .var_nrow <- length(data$var)
    names(data)[ncol(data)] <- var
    if (any(.var_miss)) {
        cat(paste0("  (", .var_nrow - .var_miss, " valid & ",
                   .var_miss, " missing values generated)\n"))
    } else {
        cat(paste0("  (", .var_nrow - .var_miss,
                   " valid values generated)\n"))
    }
    return(data)
}


#' @title Change contents of an existing variable
#'
#' @description
#'
#' \code{replace()} alters the contents of a variable when specified
#' conditions are met.
#'
#' @details
#'
#' If only `value` is specified, the whole variable is assigned
#' with the `value`. Multiple conditions can be specified within
#' the three dots.
#'
#' @param data data.frame
#' @param var variable
#' @param value value for replacement
#' @param ... `if` conditions or expressions
#'
#' @return
#'
#' data.frame
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' x <- replace(infert, case, 2, case == 0)
#' tab(x, case)
#'
#' x <- replace(infert, parity, 4, parity > 4)
#' tab(x, parity)
#'
#' \dontrun{
#' ## More examples
#' ## replacing mpg with standardized values of mpg
#' replace(mtcars, mpg, mpg / mean(mpg))
#'
#' ## replacing mpg with NA if < 10 or > 20
#' replace(mtcars, mpg, NA, mpg < 10 | mpg > 20)
#'
#' ## replacing education levels with one value
#' replace(infert, education, "6+yrs",
#'         education == "6-11yrs" | education == "12+ yrs")
#'
#' ## replacing mpg with NA if mpg is from 10 and 20.
#' replace(mtcars, mpg, NA, mpg >= 10, mpg <= 20)
#' }
#'
#' @export
replace <- function(data, var, value, ... )
{
    ## match call arguments
    .args <- as.list(match.call())

    ## get names of dataset and headings
    .data_name <- deparse(substitute(data))
    var <- deparse(substitute(var))
    value <- deparse(substitute(value))

    ## if input is not a data.frame, stop
    if (!is.data.frame(data)) {
        stop(paste0("`", .data_name, "` must be a data.frame"),
             call. = FALSE)
    }

    ## get var label
    .var.lbl <- attr(data[[var]], "label")

    ## get expression from three dots
    .expr <- enquotes(.args, c("data", "var", "value"))
    ## if more than one expression, combine with & operator
    if (length(.expr) >= 1) {
        .expr <- paste0("(", .expr, ")", collapse = " & ")
        .expr_txt <- paste0("[", .expr, "]")
    } else if (length(.expr) == 0) {
        .expr_txt <- NULL
    }

    # if var is a factor, remove corresponding levels
    if (is.factor(data[[var]])) {
        data[[var]] <- as.character(data[[var]])
    }

    ## if value is more than one vector, then collapse it
    value <- paste0(value, collapse = "")

    ## if expression is empty, then replace the whole variable
    ## if not, repress using expression
    ## evaluate the whole expression
    tryCatch({
        .df <- eval(parse(
            text = paste0("within(data, ", var, .expr_txt, " <- ", value, ")")
        ))
    }, error = function(cnd) {
        stop(cnd, call. = FALSE)
    })

    # if var is a factor, convert to factor again corresponding levels
    if (is.factor(data[[var]])) {
        .df[[var]] <- as.factor(.df[[var]])
    }

    ## assign label back to the var
    .num <- data[[var]] == .df[[var]]
    data[[var]] <- .df[[var]]
    attr(data[[var]], "label") <- .var.lbl

    ## Display message to nofity changes
    cat(
        paste0("  (",
               ifelse(is.null(.expr_txt), nrow(data),
                      sum(!.num, na.rm = TRUE) + sum(is.na(.num))),
               " values replaced)\n")
    )

    return(data)
}



#' @title Categorize a numerical variable
#'
#' @description
#' \code{egen()} transforms a numeric vector to a factor vector.
#'
#' @param data data.frame
#' @param var existing variable
#' @param cut either a number or a numeric vector
#' @param lbl labels to specify
#' @param new_var name of new variable to be created
#'
#' @details
#' \code{egen} allows easy conversion of a numerical variable to a categorical
#' variable.
#'
#' If only a number is specified in `cut`, it categorizes
#' into equal intervals based on that number. If no value is set
#' for `cut`, the default interval is `10`.
#'
#' \strong{Automatic naming new variable}
#'
#'
#' If \code{new_var} is not specified, new names will be automatically
#' created by appending `_cat` as suffix.
#' \code{VARNAME`_cat`}
#'
#' \strong{Automatic Labelling}
#'
#' If \code{lbl} is not specified, labels are constructed in
#' \code{`##-##`}.
#'
#' @return
#'
#' data.frame
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' x <- egen(infert, age)
#' tab(x, age_cat)
#'
#' \dontrun{
#' ## Set cut-off points
#' x <- egen(infert, age, c(26, 31, 36, 41))
#' tab(x, age_cat)
#'
#' ## Add labels and give a new name
#' x <- egen(infert, age, c(26, 31, 36, 41),
#'          lbl = c("<= 25", "26 - 30", "31 - 35",
#'            "36 - 40", "41+"),
#'          new_var = age_grp)
#' tab(x, age_grp)
#' }
#'
#' @export
egen <- function(data, var, cut = NULL, lbl = NULL, new_var = NULL)
{
    ## match call arguments
    .args <- as.list(match.call())

    ## get names of dataset and headings
    .data_name <- deparse(substitute(data))
    .vars_names <- names(data)

    ## if input is not a data.frame, stop
    if (!is.data.frame(data)) {
        stop(paste0("`", .data_name, "` must be a data.frame"),
             call. = FALSE)
    }


    ## create old and new var's name
    .vars_names <- names(data)
    .var_name <- .args$var
    new_var <- .args$new_var
    new_var <- ifelse(is.null(new_var),
                      paste0(.var_name, "_cat"),
                      paste(new_var))

    ## If variable is already in the dataset, stop
    if (any(.vars_names %in% new_var)) {
        stop(paste0("'", new_var, "' already exisits."),
             call. = TRUE)
    }
    ## assign var to data
    var <- data[[.var_name]]

    ## create break / cut-off labels
    if (length(cut) > 1) {
        .brk <- cut
    } else {
        ## if cut is not specified, cut <- 10
        if (is.null(cut)) {
            cut <- 10L
        }
        .brk <- seq(min(var, na.rm = TRUE), max(var, na.rm = TRUE), cut)
        if (length(.brk) == 1) {
            stop(paste0(cut, "' is too large to cut the data."),
                 call. = FALSE)
        } else {
            .brk <- c(.brk[1], .brk[2:(length(.brk)-1)] - 1,
                      .brk[length(.brk)] - 1)
        }
    }

    ## get minimum value, check and add to the cut sequence
    .brk.min <- min(var, na.rm = TRUE)
    .brk <- .brk[.brk.min < .brk]
    if (.brk.min != .brk[1]) {
        .brk <- c(.brk.min, .brk)
    }

    ## get minimum value, check and add to the cut sequence
    .brk.max <- max(var, na.rm = TRUE)
    .brk <- .brk[.brk.max > .brk]
    if (.brk.max != .brk[length(.brk)]) {
        .brk <- c(.brk, ceiling(.brk.max))
    }

    ## remove duplicated category
    .brk <- .brk[!duplicated(.brk)]

    ## check decimals
    checkDecimals <- function(x)
    {
        ## check if there are any decimal values
        .decimal <- grepl("\\.", x)
        if (any(.decimal)) {
            .decimal <- strsplit(as.character(x[.decimal][1]), "\\.")[[1]][2]
            .decimal <- nchar(.decimal)
        } else {
            .decimal <- 0
        }
        .decimal
    }
    .decimal <- checkDecimals(cut)

    ## change numbers to decimal values
    .brk <- c(floor(.brk[1]),
              round(.brk[-c(1, length(.brk))], .decimal),
              ceiling(.brk[length(.brk)]))

    ## construct labels
    if (is.null(lbl)) {
        .last.sec.pos <- length(.brk) - 1
        .lbl <- paste(
            .brk[1:.last.sec.pos], "-",
            c(.brk[2:.last.sec.pos] - (1 / (10 ^ .decimal)),
              .brk[length(.brk)]),
            sep = ""
        )
    } else {
        .lbl <- lbl
    }

    ## create new var
    .var <- cut(var, breaks = as.numeric(.brk),
                labels = .lbl, right = FALSE,
                include.lowest = TRUE)
    ## assign .var back to data, add label and change the names
    data$new_var <- .var
    attr(data$new_var, "label") <- paste0(.var_name, " categories")
    names(data)[ncol(data)] <- new_var

    ## Print notification
    .na <- is.na(.var)
    cat(paste0("  (", sum(!.na), " valid ",
               ifelse(any(.na), paste0("& ", sum(.na), " missing "), ""),
               "values generated)\n"))

    return(data)
}



#' @title Count from `n_` to `N_`
#'
#' @description
#' \code{n_()} generates the current observation number
#' per specified group. It is
#' regarded as grouped serial numbers.
#'
#'
#' \code{N_()} generates total number of observation per group.
#' It is regarded as grouped total number.
#'
#' @param data data.farme
#' @param ... variables for grouping
#'
#' @details
#' If no variable is set in `...`, all variables in the datset
#' is used for grouping.
#'
#' @return
#'
#' data.frame
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' x <- n_(iris, Species)
#' \dontrun{
#' x
#' codebook(x)
#'
#' x <- N_(iris, Species)
#' x
#' codebook(x)
#' }
#'
#' @export
n_ <- function(data, ... )
{
    ## match call arguments
    .args <- as.list(match.call())

    ## get names of dataset and headings
    .data_name <- deparse(substitute(data))
    .vars_name <- names(data)

    ## if input is not a data.frame, stop
    if (!is.data.frame(data)) {
        stop(paste0("`", .data_name, "` must be a data.frame"),
             call. = FALSE)
    }

    .vars <- enquotes(.args, c("data"))
    if (length(.vars) == 0) {
        .vars <- names(data)
    }
    ## create expression to order data
    .data <- eval(parse(
        text = paste0("data[with(data, order(",
                      paste0(.vars, collapse = ", "),
                      ")), ]")
    ))

    ## create identifiers to check duplications
    .id <- apply(.data[.vars], 1, paste, collapse = " ")
    ## create seiral id with ave function
    .id_num <- ave(.id, .id, FUN = seq_along)

    ## assign the id back to the original dataset
    .data$n_ <- as.numeric(.id_num)
    attr(.data$n_, "label") <- "<Sys.Gen: Current obs number>"

    return(.data)
}


#' @rdname n_
#' @export
N_ <- function(data, ... )
{
    ## match call arguments
    .args <- as.list(match.call())

    ## get names of dataset and headings
    .data_name <- deparse(substitute(data))
    .vars_name <- names(data)

    ## if input is not a data.frame, stop
    if (!is.data.frame(data)) {
        stop(paste0("`", .data_name, "` must be a data.frame"),
             call. = FALSE)
    }

    .vars <- enquotes(.args, c("data"))
    if (length(.vars) == 0) {
        .vars <- names(data)
    }
    ## create expression to order data
    .data <- eval(parse(
        text = paste0("data[with(data, order(",
                      paste0(.vars, collapse = ", "),
                      ")), ]")
    ))

    ## create identifiers to check duplications
    .id <- apply(.data[.vars], 1, paste, collapse = " ")
    ## create seiral id with ave function
    .id_num <- ave(.id, .id, FUN = seq_along)

    ## get the last number of serial number
    .last_obs <- sapply(.id, function(z) {
        .dup_id <- .id_num[.id == z]
        .dup_id[length(.dup_id)]
    })

    ## assign the id back to the original dataset
    .data$N_ <- as.numeric(.last_obs)
    attr(.data$N_, "label") <- "<Sys.Gen: total number of obs>"

    return(.data)
}




#' @title Report, tag or drop the duplicate observations
#'
#' @description
#' \code{duplicates()} generates a table showing the
#' duplicate `Observations` as one or more copies as well as
#' their `Surplus` indicating the second, third, `...` copy of
#' the first of each group of duplicates.
#'
#' @param data data.frame
#' @param ... variables to find the duplicate observations
#' @param drop `TRUE` deletes all the duplicate observations.
#'
#' @details
#'
#' If no variable is specified in `...`, all variables are used
#' to find the duplicate observations.
#'
#' If `drop` is set to `TRUE`, all occurrences of each group
#' of observations except the first are deleted from the
#' dataset.
#'
#' @return
#'
#' data.frame with a column `dup_num`, indicating the number of duplicate
#' observations of each group of observations
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' x <- duplicates(iris, Species)
#' x <- duplicates(iris)
#'
#' @export
duplicates <- function(data, ... , drop = FALSE)
{
    ## match call arguments
    .args <- as.list(match.call())

    ## get names of dataset and headings
    ## get number of observations
    .data_name <- .args$data
    .vars_name <- names(data)

    ## if input is not a data.frame, stop
    if (!is.data.frame(data)) {
        stop(paste0("`", .data_name, "` must be a data.frame"),
             call. = FALSE)
    }

    .vars <- enquotes(.args, c("data", "drop"))
    if (length(.vars) == 0) {
        .vars <- names(data)
        .txt <- "all variables"
    } else {
        .txt <- paste(.vars, collapse = " + ")
    }
    ## create expression to order data
    .data <- eval(parse(
        text = paste0("data[with(data, order(",
                      paste0(.vars, collapse = ", "),
                      ")), ]")
    ))

    ## create identifiers to check duplications
    .id <- apply(.data[.vars], 1, paste, collapse = " ")
    ## create seiral id with ave function
    .id_num <- ave(.id, .id, FUN = seq_along)

    ## get the last number of serial number
    .last_obs <- sapply(.id, function(z) {
        .dup_id <- .id_num[.id == z]
        .dup_id[length(.dup_id)]
    })

    ## Make changes to the dataset
    ## create a dup variable for indication
    .data$dup_num <- as.numeric(.last_obs) - 1
    attr(.data$dup_num, "label") <- "<Sys.Gen: # of duplicate obs>"

    ## create table and use the categories to calculate surplus number
    .dup_obs_tbl <- table(.last_obs)
    .dup_obs_tbl_names <- as.numeric(names(.dup_obs_tbl))
    .non_dup <- sapply(.dup_obs_tbl_names, function(z) {
        .dup_id <- .id[.last_obs == z]
        length(.dup_id[!duplicated(.dup_id)])
    })

    ## create final table for duplication report
    .tbl <- data.frame(cbind(.dup_obs_tbl_names, .dup_obs_tbl,
                             .dup_obs_tbl - .non_dup))
    names(.tbl) <- c("Copies", "Observations", "Surplus")
    row.names(.tbl) <- NULL
    .tbl <- formatdf(.tbl, 2, 2)

    ## Display information
    cat(paste0("  Duplicates in terms of ", .txt, "\n"))
    print.data.frame(.tbl, row.names = FALSE, max = 1e9)
    cat(paste0("  (Total obs: ", nrow(.data), ")\n"))

    ## remove the duplicate observations
    if (drop) {
        .dup <- .id_num == 1
        .data <- .data[.dup, ]
        cat(paste0("  (", sum(!.dup),
                   " observations deleted)\n"))
    }

    return(.data)
}



#' @title Duplicate observations within a dataframe
#'
#' @description
#'
#' \code{expand2} generates duplicated observations within a dataframe.
#'
#' @param data a data frame object
#' @param n_n index or indexes specifying row numbers
#' @param copies desired number of copies
#' @param original a logical indicating whether to keep the original dataframe
#'
#' @details
#'
#' \code{expand2} appends observations from the dataframe
#' with n copies of the observations with
#' specified indexes of observations or all data.
#'
#' @return
#'
#' data.frame
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' ## create duplicates
#' x <- expand2(infert, 1:5, copies = 2)
#'
#' ## check duplicates report and rmeove dup
#' duplicates(x, drop = TRUE)
#'
#' @export
expand2 <- function(data, n_n = NULL, copies = 2, original = TRUE)
{
    ## match call arguments
    .args <- as.list(match.call())

    ## copy data to .data
    .data <- data

    ## get names of dataset and headings
    .data_name <- deparse(substitute(data))
    .vars_names <- names(.data)

    ## if input is not a data.frame, stop
    if (!is.data.frame(.data)) {
        stop(paste0("`", .data_name, "` must be a data.frame"),
             call. = FALSE)
    }

    data.lbl <- attr(data, "label")
    data <- data.frame(data)
    vars.lbl <- sapply(data, function(z) {
        lbl <- attr(z, "label")
        if (is.null(lbl)) {
            lbl <- "<NA>"
        } else {
            lbl <- paste(attr(z, "label"), collapse = " ")
        }
        lbl
    })
    #### if n_n is empty, put number of all rows to n_n
    if (is.null(n_n)) {
        n_n <- nrow(data)
    }
    #### if there are more than one values in n_n, take the last value
    if (length(n_n) == 1) {
        n_n <- 1:n_n
    }
    t <- data[n_n, ]

    if (original) {
        f <- data
    } else {
        f <- NULL
    }
    for (i in 1:(copies)) {
        f <- rbind(f, t)
    }
    attr(f, "label") <- data.lbl
    for (i in 1:ncol(f)) {
        attr(f[, i], "label") <- vars.lbl[i]
    }

    return(f)
}





#' @title Append datasets
#'
#' @description
#'
#' \code{append()} row-combines multiple datasets of the same column names.
#'
#' @param data data.frame
#' @param ... one or multiple data.frame
#'
#' @details
#'
#' A single or multiple datasets can be appended.
#'
#' The appending datasets must have at least one variable name
#' which is there in the master dataset.
#'
#' The order of variables of the appending datasets is automatically
#' set based on the variable arrangement of the master dataset.
#'
#'
#' @return
#'
#' data.frame
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' x <- append(infert[, -c(3,4)], infert[, -5], infert[, -6])
#' ## codebook(x)
#'
#' \dontrun{
#' ## if no variables are matched, ERROR
#' append(infert, iris)
#' }
#'
#' @export
append <- function(data, ... )
{
    ## match call arguments
    .args <- as.list(match.call())

    ## get names of dataset and headings
    .data_name <- deparse(substitute(data))
    .vars_name <- names(data)
    ## if input is not a data.frame, stop
    if (!is.data.frame(data)) {
        stop(paste0("`", .data_name, "` must be a data.frame"),
             call. = FALSE)
    }

    .ls <- list(...)
    .ls_name <- enquotes(.args, c("data"))

    .df <- do.call(
        rbind,
        lapply(1:length(.ls), function(z) {
            x <- .ls[[z]]
            .names <- names(x)
            if (!any(.vars_name %in% .names)) {
                stop(paste0("`", .ls_name[z], "` must have at least",
                " one variable name that is in the master dataset '",
                .args$data, "'"), call. = FALSE)
            }
            .vars <- intersect(.vars_name, .names)
            x <- x[.vars]
            x[, .vars_name[!(.vars_name %in% names(x))]] <- NA
            cat(paste0("  ('", .ls_name[z], "' appended)\n"))
            x[, .vars_name]
        })
    )
    data <- rbind(data, .df)

    return(data)
}




#' @title Format Dates
#'
#' @description
#' \code{formatDate} converts characters or numbers to dates.
#' \code{is.Date} indicates which elements are Dates.
#'
#' @param x a character or numeric object
#' @param format only for character vectors:
#' @param sep separator character for date components
#' @param century specify either 2000 or 1900 for two-digit years
#'
#' @details
#'
#' \code{dmy} represents \code{dd mm YYYY} format.
#' In combination with separators from \code{sep}, this can change to
#' several date formats.
#' For example, \code{dmy} + \code{-} convert to
#' \code{dd-mm-yyyy} format.
#'
#' \strong{Possible conversions}
#'
#' \enumerate{
#'     \item \code{dmy} + \code{-} >>> \code{dd-mm-yyyy}
#'     \item \code{dmy} + \code{/} >>> \code{dd/mm/yyyy}
#'     \item \code{mdy} + \code{/} >>> \code{mm/dd/yyyy}
#'     \item \code{ymd} + \code{/} >>> \code{yyyy/mm/dd}
#'     \item \code{dby} + \code{-} >>> \code{dd-JAN-yy}
#'     \item \code{dby} + \code{/} >>> \code{dd/JAN/yy}
#' }
#'
#' \strong{Numeric conversions}
#' Origin is set at \code{1899-12-30}.
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' ## convert strings to dates
#' x <- c("2019-01-15", "2019-01-20", "2019-01-21", "2019-01-22")
#'
#' # check if it is a Date format
#' is.Date(x)
#'
#' \dontrun{
#' y <- formatDate(x, "Ymd", "-")
#'
#' # check if it is a Date format
#' is.Date(y)
#' y
#'
#'
#' ## another format
#' x <- c("22-JAN-19", "24-MAR-20")
#' y <- formatDate(x, "dby", "-")
#' is.Date(y)
#' y
#'
#'
#' ## convert numbers to dates
#' x <- 42705:42710
#' y <- formatDate(x)
#' is.Date(y)
#' y
#'
#'
#' ## get day, month or year
#' day(y)
#' month(y)
#' year(y)
#' }
#'
#' @export
formatDate <- function(x, format = "dmY", sep = "/", century = NULL)
{
    if (is.character(x)) {
        f <- paste(
            paste0(
                "%",
                unlist(strsplit(format, split = NULL, useBytes = T))
            ),
            collapse = sep
        )
        x <- as.Date(x, format = f)
        if (!is.null(century)) {
            y <- do.call(
                rbind, strsplit(as.character(x), split = "-", fixed = TRUE)
            )[,1]
            m <- do.call(
                rbind, strsplit(as.character(x), split = "-", fixed = TRUE)
            )[,2]
            d <- do.call(
                rbind, strsplit(as.character(x), split = "-", fixed = TRUE)
            )[,3]
            if (century) {
                y <- (as.numeric(y) %% 100) + 2000
            } else {
                y <- (as.numeric(y) %% 100) + 1900
            }
            x <- as.Date(paste(y, m, d, sep = "-"), format = "%Y-%m-%d")
        }
    } else if (is.numeric(x)) {
        x <- as.Date(x, origin = "1899-12-30")
    } else {
        stop("x must be a character or numeric.")
    }
    return(x)
}

#' @rdname formatDate
#' @export
is.Date <- function(x)
{
    return(class(x) == 'Date')
}

#' @rdname formatDate
#' @export
year <- function(x)
{
    if (!is.Date(x)) stop("x must be Date.")
    as.numeric(format(x, "%Y"))
}

#' @rdname formatDate
#' @export
month <- function(x)
{
    if (!is.Date(x)) stop("x must be Date.")
    as.numeric(format(x, "%m"))
}

#' @rdname formatDate
#' @export
day <- function(x)
{
    if (!is.Date(x)) stop("x must be Date.")
    as.numeric(format(x, "%d"))
}



#' @title Expand \code{2x2 table} into \code{data.frame}
#'
#' @description
#'
#' \code{expandtbl()} generates a data.frame based on vectors.
#'
#' @param ... vectors
#' @param exp_name Name of \code{exp} Variable
#' @param exp_lvl Names of two categories in the order of
#' Exposed and non-exposed
#' @param case_name Name of \code{Case} variable
#' @param case_lvl names of two categories in the order of
#' @param strata_name Name of stratified variable
#'
#' @details
#'
#' \strong{expandtbl}
#'
#' uses the vectors of \code{2x2} tables and
#' generates a data frame of at least two columns:
#' exp and case.
#'
#' \preformatted{expandtbl(c(100, 200, 100, 200))}
#'
#' \code{Strata}
#'
#' Multiple tables can be used to construct a dataset by specifying
#' \code{strata_name} as follow. Strata can be included
#' using multiple named vectors.
#'
#' \preformatted{
#' expandtbl(
#'              strata1 = c(100, 200, 100, 200),
#'              strata2 = c(100, 200, 100, 200),
#'              strata3 = c(100, 200, 100, 200),
#'              exp_name = "exp",
#'              exp_lvl = c("exposed", "unexposed"),
#'              case_name = "case",
#'              case_lvl = c("case", "control"),
#'              strata_name = "Strata"
#' )
#' }
#'
#' \code{Labels for variables}
#'
#' If names or levels of variables are not specified, the followings are
#' applied.
#'
#' \enumerate{
#'     \item exp Name: \code{exp}
#'     \item exp levels: \code{exposed} and \code{unexposed}
#'     \item case Name: \code{case}
#'     \item case levels: \code{case} and \code{control}
#'     \item Strata Name: \code{strata}
#'     \item Note: Strata levels are not considered as vectors must
#'     be named.
#' }
#'
#' @return
#'
#' data.frame
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#'
#' ## Asthma Example from Essential Medical Statistics
#' ## page 160
#' asthma <- expandtbl(c(81, 995, 57, 867),
#'               exp_name = "sex",
#'               exp_lvl = c("woman", "man"),
#'               case_name = "asthma",
#'               case_lvl = c("yes", "no"))
#'
#' \dontrun{
#' ## label variable and dataset
#' asthma <- label(asthma, "Hypothetical Data of Asthma Prevalence")
#' asthma <- label(asthma, sex = "Man or Woman",
#'                            asthma = "Asthma or No Asthma")
#'
#' ## Checking codebook
#' codebook(asthma)
#'
#'
#' ## simple tabulation
#' tab(asthma)
#'
#' ## cross-tabulation
#' tab(asthma, sex, by = asthma)
#' }
#'
#' @export
expandtbl <- function( ... ,
                       exp_name = "exp",
                       exp_lvl = c("exposed", "unexposed"),
                       case_name = "case",
                       case_lvl = c("case", "control"),
                       strata_name = "strata")
{
    # get vectors within three dots
    .vec <- list(...)
    .vec_len <- length(.vec)

    ## calculate strata
    .strata_names <- sapply(1:length(.vec), function(z) names(.vec[z]))
    .strata_times <- sapply(.vec, function(z) sum(z))

    ## create data.frame

    .exp <- rep(exp_lvl, each = 2)
    .case <- rep(case_lvl, times = 2)

    ## create data frame
    tryCatch({
        data <- do.call(
            rbind,
            lapply(1:.vec_len, function(z) {
                .times <- unlist(.vec[z])
                .vec.exp <- rep(.exp, .times)
                .vec.case <- rep(.case, .times)
                .vec_strata <- rep(names(.vec[z]), sum(.times))

                if (is.null(.vec_strata)) {
                    .df <- data.frame(.vec.exp, .vec.case)
                    names(.df) <- c(exp_name, case_name)
                } else {
                    .df <- data.frame(.vec.exp, .vec.case, .vec_strata)
                    names(.df) <- c(exp_name, case_name, strata_name)
                }
                .df
            })
        )

        ## print message
        cat(paste0("  (expanded into a dataset)\n"))

    }, error = function(cnd) {
        stop(cnd, call. = FALSE)
    })


    return(data)
}


#' @describeIn expandtbl
#'
#' \code{expandfreq()} expands a frequency-weighted table
#' into a data.frame.
#'
#' @param data frequency table in data.frame
#' @param freq name of variable for the weighted frequency
#'
#' @details
#'
#' \strong{expandfreq()} uses the weighted frequencies in
#' data.frame format and construct another data.frame
#' based on the frequency weight.
#' The name of the frequency weighted variable can be
#'  specified by \code{freq} argument.
#'
#' @examples
#'
#' ## Example for expanding frequency weighted data
#'
#' ## Example from UCLA website
#' ## you can download the dataset here:
#' ## https://stats.idre.ucla.edu/stat/stata/examples/icda/afterlife.dta
#'
#' x <- data.frame(gender = c(1, 1, 0, 0),
#'                  aftlife = c(1, 0, 1, 0),
#'                  freq = c(435, 147, 375, 134))
#' y <- expandfreq(x, freq)
#'
#' ## check the numbers by tabulation
#' ## tab(y, gender, by = aftlife)
#'
#' @export
expandfreq <- function(data, freq)
{
    ## match call arguments
    .args <- as.list(match.call())

    ## if input is not a data.frame, stop
    if (!is.data.frame(data)) {
        stop(paste0("`", .data_name, "` must be a data.frame"),
             call. = FALSE)
    }

    ## get names of dataset and headings
    .data_name <- deparse(substitute(data))
    .vars_names <- names(data)
    freq <- .args$freq

    ## get where the frequency column is and get the freq
    .freqi <-.vars_names %in% as.character(freq)
    .freq <- data[[freq]]

    ## if freq is not numbers, stop
    if (!is.numeric(.freq)) {
        stop(paste0("`", .freq, "` must be a number."),
             call. = FALSE)
    }

    ## repeat other columns per freq
    .t <- apply(data[, !.freqi], 2, function(z) rep(z, .freq))

    ## put the dataset back to the original data frame
    ## this preserves the data structure of original data
    .df <- data[0, !.freqi]
    .df[1:nrow(.t), ] <- .t

    ## print message
    cat(paste0("  ('", .data_name, "' expanded into dataset)\n"))

    return(.df)
}



#' @title Lag a variable
#' @description
#'
#' creates lagged version of an existing variable.
#'
#' @param x data.frame
#' @param var variable to be lagged
#' @param by variable for grouped lagged version
#' @param new_var name of new lagged variable
#' @param last_obs `TRUE`retrieves the last observation per group.
#' @param ... further arguments to be passed to or from methods.
#'
#' @details
#'
#' This is often encountered in time-related analysis.
#' In a lagged variable, values from earlier points in time are placed in later
#' rows of dataset.
#'
#' @note
#'
#' Before using \code{lagRows}, the dataset needs to be sorted by a id variable
#' or similar variable.
#'
#' @return
#'
#' data.frame
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' set.seed(100)
#' ## create a dataset with dates
#' x <- data.frame(
#'     hospid = 1:100,
#'     docid = round(runif(100, 1, 10)),
#'     dis_date = formatDate(runif(100, 42700, 42800))
#' )
#'
#' ## lagged dis_date, not specifed "by"
#' lag(x, dis_date)
#'
#' \dontrun{
#' ## lagged dis_date by docid
#' ## first we need to sort
#' y <- x[order(x$docid), ]
#' y
#'
#' ## lag dates within groups
#' lag(y, dis_date, by = docid, new_var = lag_date)
#' lag(y, dis_date, by = docid, lag_date, TRUE)
#' }
#'
#' @rdname lag
#' @export
lag.data.frame <- function(x, var, by = NULL, new_var = NULL,
                           last_obs = FALSE, ... )
{
    ## match call arguments
    .args <- as.list(match.call())
    data <- x
    ## get names of dataset and headings
    .data_name <- deparse(substitute(x))
    .vars_name <- names(data)

    ## if input is not a data.frame, stop
    if (!is.data.frame(data)) {
        stop(paste0("`", .data_name, "` must be a data.frame"),
             call. = FALSE)
    }

    ## assign var and by
    var <- data[[.args$var]]
    by <- .args$by

    ## create lagged variables
    if (is.null(by)) {
        .lag <- c(var[1], var[-length(var)])
        .lag[1] <- NA
    } else {
        ## if by is specified, then assign by
        ## then create levels
        by <- data[[as.character(by)]]
        .lvl <- unique(by)
        .lag <- do.call(
            c,
            lapply(.lvl, function(z) {
                if (is.na(z)) {
                    .equal <- is.na(by)
                } else {
                    .equal <- which(by == z)
                }
                .lag <- var[.equal]
                .lag <- c(.lag[1], .lag[-length(.lag)])
                .lag[1] <- NA
                if (last_obs) {
                    .lag <- var[.equal]
                    .lag <- rep(.lag[length(.lag)], length(.lag))
                }
                .lag
            })
        )
    }

    ## add lagged variable to dataset
    ## add label and create name
    tryCatch({
        data$new_var <- .lag
    }, error = function(cnd) {
        stop(cnd, call. = FALSE)
    })
    attr(data$new_var, "label") <-
        paste0("<SYS.GEN: Lagged version of '", .args$var, "'>")
    new_var <- ifelse(is.null(.args$new_var),
                      paste0(.args$var, "_lag"),
                      as.character(.args$new_var))

    names(data)[ncol(data)] <- new_var

    ## print changes
    cat(paste0("  (lagged into '",
               new_var, "')\n"))

    return(data)
}






# SUMMARY STATISTICS ------------------------------------------------------

#' @title Tabulation
#'
#' @description
#'
#' \code{tab()} generates one-way or two-way tabulation of variables.
#' If no variables are specified, tabulations for all the variables
#' in the dataset are generated.
#'
#' @param data data.frame
#' @param ... variable name or names of multiple variables
#' @param by variable name for bivariate analysis
#' @param row.pct `TRUE`, `FALSE` or `NULL`.
#' @param na.rm logical: if `TRUE`, it removes observations with missing values.
#' @param digits specify rounding of numbers.
#'
#' @details
#'
#' \strong{One-way tabulation}
#'
#' If \code{by} is not specified, \code{tab} generates one-way tabulation of
#' a variable or multiple variables. The table is displayed
#' in \code{Freq.} (frequency), \code{Percent}
#' (Relative Frequency) and \code{Cum.} (Cumulative Relative frequency).
#'
#' \strong{Two-way tabulation}
#'
#' Specifying \code{by} leads to two-way tabulation. By default,
#' row percentages are displayed along with count data. If `row.pct`
#' is set to `NULL`, it shows a count table without percentages. If set to
#' `FALSE`, a table with column percentages is generated. P-values from
#' `Chi-squared` and Fisher's `Exact` tests are also shown, regardless
#' of displaying percentages.
#'
#' \strong{Tabulating the whole dataset}
#'
#' This is helpful when the dataset has been processed and finalized.
#' The final dataset can be fed into the function without
#' inputting any variables. This automatically filters and generates
#' tables on variables with possible data types for tabulation. These
#' data types include `character`, `factor`, `order factor`, and `logical`.
#'
#' \strong{Using colon `:` to tabulate multiple variables}
#'
#' A colon separator `:` can be used to generate one-way or two-way
#' tables efficiently.
#'
#' \strong{Labels}
#'
#' Labels for corresponding variables are displayed below the
#' table.
#'
#' @return
#'
#' A list with `tab` class containing three sets of data.frame type:
#' 1) tabulation result,
#' 2) tabulation result without any format,
#' 3) labels for corresponding variables.
#'
#' @import stats
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' ## One-way tabulation
#' tab(infert, education)
#' tab(infert, education, parity:spontaneous)
#' tab(infert)
#'
#' ## Two-way tabulation
#' tab(infert, education, by = case)
#' tab(infert, education, parity:spontaneous, by = case)
#' tab(infert, by = case)
#'
#' @export
tab <- function(data, ... , by = NULL, row.pct = TRUE, na.rm = FALSE, digits = 1)
{
    ## if data is not a data.frame, stop
    .data_name <- deparse(substitute(data))
    if (!is.data.frame(data)) {
        stop(paste0("`", .data_name, "` must be a data.frame"),
             call. = FALSE)
    }

    ## match call arguments
    .args <- as.list(match.call())

    ## get variable names
    .vars <- enquotes(.args, c("data", "by", "row.pct", "na.rm", "digits"))
    .vars <- checkEnquotes(data, .vars)
    if (length(.vars) == 0) {
        .vars_type <- sapply(data, function(z) {
            .class <- class(unlist(z))[1]
            if (.class == "haven_labelled") {
                .class <- typeof(unlist(z))[1]
            }
            .class
        })
        .types <- c("factor", "character", "orderedfactor", "logical")
        .vars <- names(data)[.vars_type %in% .types]
    }

    ## check if var names are valid
    sapply(.vars, function(z) {
        ## check if all vars specified are in the dataset
        if (!(z %in% names(data))) {
            stop(paste0("Variable '", z, "' not found in the dataset"),
                 call. = FALSE)
        }
    })

    ## Tabulation
    by <- .args$by
    if (length(by) == 0) {
        .df <- do.call(rbind, lapply(.vars, tab1, data, na.rm, digits))
        .txt <- paste0("  One-way Tabulation")
    } else {
        if (!(as.character(by) %in% names(data))) {
            stop(paste0("`", by, "` not found."),
                 call. = FALSE)
        }
        .df <- do.call(
            rbind, lapply(.vars, tab2, data, by, row.pct, na.rm, digits)
        )
        .txt <- paste0("  Tabulation by '", by, "'")
    }
    .df_raw <- .df

    ## Add horizontal and vertical lines for visual appealing
    hpos <- as.numeric(row.names(.df)[!(.df$Variable == "")])[-1]
    hpos <- hpos + 0:(length(hpos) - 1)
    sapply(hpos, function(z) {
        .df <<- addHLines(.df, z)
    })
    hpos <- (c(hpos, nrow(.df) + 1) - 1) + 0:(length(hpos))
    sapply(hpos, function(z) {
        .df <<- addHLines(.df, z)
        .df[z, 1] <<- ""
    })
    .df <- formatdf(.df, 2, 3)
    if (length(by) != 0) {
        .df <- cbind(.df[, 1:(ncol(.df) - 3)], .df[, 4],
                     .df[, (ncol(.df) - 2):ncol(.df)])
        names(.df) <- .df[2, ]
    }

    ## Display information
    # printDFlenLines(.df)
    cat(paste0(.txt, "\n"))
    # printDFlenLines(.df)
    print.data.frame(.df, row.names = FALSE, max = 1e9)

    ## get vars lbl
    .vars_lbl <- sapply(c(by, .vars), getLabel, data)
    .vars_lbl <- data.frame(vars = c(as.character(by), .vars),
                            lbl = .vars_lbl)
    .vars_lbl$lbl[is.na(.vars_lbl$lbl)] <- .vars_lbl$vars[is.na(.vars_lbl$lbl)]

    ## create list with class for tabulation
    .list <- list(tab = .df,
                  tab_raw = .df_raw,
                  lbl = .vars_lbl,
                  type = ifelse(is.null(row.pct), "tab2",
                         ifelse(length(by) == 0, "tab1",
                                "tab2p")))
    class(.list) <- "tab"
    invisible(.list)
}

#### Helper functions for tab()
tab1 <- function(x, data, na.rm = FALSE, digits = 1)
{
    .x <- data[[x]]

    ## check NA
    .useNA <- ifelse(na.rm, "no", "ifany")

    ## create tabulation table
    .freq <- table(.x, useNA = .useNA)
    .pct <- sprintf(prop.table(.freq) * 100,
                    fmt = paste0("%#.", digits, "f"))
    .cumpct <- sprintf(cumsum(.pct),
                       fmt = paste0("%#.", digits, "f"))
    .freq <- c(.freq, Total = sum(.freq, na.rm = TRUE))

    ## combine all statistics
    .df <- data.frame(cbind(Variable = c(x, rep("", length(.freq) - 1)),
                            Category = names(.freq),
                            Freq. =  .freq,
                            Percent = c(.pct, 100),
                            Cum. = c(.cumpct, 100)))
    row.names(.df) <- NULL

    return(.df)
}

tab2 <- function(x, data, by, row.pct = TRUE, na.rm = FALSE, digits = 1)
{
    .x <- data[[x]]
    .by <- data[[by]]

    ## check NA
    .useNA <- ifelse(na.rm, "no", "ifany")

    ## Create confusion matrices of raw, row and col percentages
    .tbl <- table(.x, .by, useNA = .useNA)
    .tbl_count <- addmargins(.tbl)

    .tbl_pct_row <- addmargins(prop.table(.tbl, 1) * 100)
    .tbl_pct_row[nrow(.tbl_pct_row), ] <-
        .tbl_count[nrow(.tbl_count), ] / sum(.tbl) * 100
    .tbl_pct_row <- round(.tbl_pct_row, digits)

    .tbl_pct_col <- addmargins(prop.table(.tbl, 2) * 100)
    .tbl_pct_col[, ncol(.tbl_pct_col)] <-
        .tbl_count[, ncol(.tbl_count)] / sum(.tbl) * 100
    .tbl_pct_col <- round(.tbl_pct_col, digits)


    ## row and col names of the tables
    .row_name <- c(row.names(.tbl), "Total")
    .col_name <- c(colnames(.tbl), "Total")
    .row_pct_name <- c(rbind(.col_name, rep("r(%)", length(.col_name))))
    .col_pct_name <- c(rbind(.col_name, rep("c(%)", length(.col_name))))

    row.names(.tbl_count) <- .row_name
    colnames(.tbl_count) <- .col_name
    row.names(.tbl_pct_row) <- .row_name
    row.names(.tbl_pct_col) <- .row_name

    .row_order <- order(c(2 * (1:ncol(.tbl_count) - 1) + 1,
                          2 * 1:ncol(.tbl_pct_row)))
    .tbl_pct_row <- cbind(.tbl_count, .tbl_pct_row)[, .row_order]
    colnames(.tbl_pct_row) <- .row_pct_name
    .tbl_pct_col <- cbind(.tbl_count, .tbl_pct_col)[, .row_order]
    colnames(.tbl_pct_col) <- .col_pct_name

    ## Choose which table to print and return
    if (is.null(row.pct)) {
        .df <- .tbl_count
    } else if (row.pct) {
        .df <- .tbl_pct_row
    } else {
        .df <- .tbl_pct_col
    }
    .df <- as.data.frame(cbind(.df))
    .df <- cbind(.df[, 0], Variable = c(x, rep("", nrow(.df) - 1)),
                 Category = .row_name, .df)
    row.names(.df) <- NULL


    ## calculate p-values
    ## if need to remove NA, create a data.frame and then omit NA
    if (na.rm) {
        .data <- data.frame(x = .x, by = .by)
        .data <- na.omit(.data)
        .x <- .data$x
        .by <- .data$by
    }


    ## get pvalue from chi square and fisher tests
    pvalue <- tryCatch({
        suppressWarnings(chisq.test(.x, .by, correct = FALSE)$p.value)
    }, error = function(cnd) {
        return(NA)
    })
    pvalue <- c(
        pvalue,
        tryCatch({
            suppressWarnings(
                fisher.test(.x, .by, simulate.p.value = TRUE)$p.value)
        }, error = function(cnd) {
            return(NA)
        })
    )
    pvalue <- sprintf(pvalue, fmt = '%#.3f')
    ## add pvalue back to .df
    .df$p1 <- c(pvalue[1], rep("", nrow(.df) - 1))
    .df$p2 <- c(pvalue[2], rep("", nrow(.df) - 1))
    names(.df)[(ncol(.df)-1):ncol(.df)] <- c("Chi-squared", "Exact.")

    return(.df)
}




#' @title Summary statistics
#'
#' @description
#'
#' \code{summ()} calculates and displays a variety of summary statistics.
#' If no variables are specified, summary statistics are calculated
#' for all the variables in the dataset.
#'
#' @param data data.frame
#' @param ... variable name or names of multiple variables
#' @param by variable name for bivariate analysis
#' @param na.rm logical: if `TRUE`, it removes observations with missing values.
#' @param digits specify rounding of numbers.
#' @param detail logical: if `TRUE`, it displays a full spectrum of
#' summary statistics such as inter-quartile range, and p-value from normality
#' test.
#'
#' @details
#'
#' It calculates seven number summary statistics, and p-values from relevant
#' statistical tests of association.
#'
#'
#' \strong{ANNOTATIONS}
#'
#' \code{Obs} = Number of observations
#'
#' \code{NA} = Number of observations with missing value
#'
#' \code{Mean} = Mean
#'
#' \code{Std.Dev} = Standard deviation
#'
#' \code{Median} = Median value
#'
#' \code{25%} = First quartile or percentile
#'
#' \code{75%} = Third quartile or percentile
#'
#' \code{Min} = Minimum value
#'
#' \code{Max} = Maximum value
#'
#' \code{Normal} = p-value from Shapiro-Wilk Normality Test
#'
#' \strong{Grouped summary statistics}
#'
#' If a strata variable `by` is specified, grouped summary statistics
#' are calculated. In addition, based on the levels of `by`,
#' relevant statistical tests of association such as Student's t-test
#' and Wilcoxon, ANOVA and Kruskal-Wallis tests are calculated and their
#' associated p-values are displayed.
#'
#' \strong{Tabulating the whole dataset}
#'
#' This is helpful when the dataset has been processed and finalized.
#' The final dataset can be fed into the function without
#' inputting any variables. This automatically filters and generates
#' tables on variables with possible data types for summary statistics. These
#' data types include `numeric`, `double`, `integer`, and `logical`.
#'
#' \strong{Using colon `:` to summarize multiple variables}
#'
#' A colon separator `:` can be used to summarize variables more efficiently.
#'
#' \strong{Labels}
#'
#' Labels for corresponding variables are displayed below the
#' table.
#'
#' @return
#'
#' A list with `summ` class containing three sets of data.frame type:
#' 1) summary result,
#' 2) summary result without any format,
#' 3) labels for corresponding variables.
#'
#' @import stats
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' ## Univariate summary statistics
#' summ(iris, Sepal.Length)
#' summ(iris, Sepal.Length:Petal.Width)
#'
#' ## Bivariate summary statistics
#' summ(iris, Sepal.Length:Petal.Width, by = Species)
#'
#' \dontrun{
#' ## Using the whole dataset
#' summ(iris)
#' summ(iris, by = Species)
#'
#' ## Detailed summary statistics
#' summ(iris, detail = TRUE)
#' summ(iris, by = Species, detail = TRUE)
#' }
#'
#' @export
summ <- function(data, ... , by = NULL, na.rm = FALSE, digits = 1, detail = FALSE)
{
    ## if data is not a data.frame, stop
    .data_name <- deparse(substitute(data))
    if (!is.data.frame(data)) {
        stop(paste0("`", .data_name, "` must be a data.frame"),
             call. = FALSE)
    }

    ## match call arguments
    .args <- as.list(match.call())

    ## get variable names
    .vars <- enquotes(.args, c("data", "by", "na.rm", "digits", "detail"))
    .vars <- checkEnquotes(data, .vars)
    if (length(.vars) == 0) {
        .vars_type <- sapply(data, function(z) {
            .class <- class(unlist(z))[1]
            if (.class == "haven_labelled") {
                .class <- typeof(unlist(z))[1]
            }
            .class
        })
        .types <- c("numeric", "double", "integer", "logical")
        .vars <- names(data)[.vars_type %in% .types]
    }

    ## check if var names are valid
    sapply(.vars, function(z) {
        ## check if all vars specified are in the dataset
        if (!(z %in% names(data))) {
            stop(paste0("Variable '", z, "' not found in the dataset"),
                 call. = FALSE)
        }
    })

    ## Summary measures
    by <- .args$by
    if (length(by) == 0) {
        .df <- do.call(rbind, lapply(.vars, summ1, data, na.rm, digits))
        .df_raw <- .df
        if (!detail) .df <- .df[, -c(6:8)]
        .df <- formatdf(.df, 2, 2)
        if (length(.vars) > 5) {
            .seq <- seq(3, nrow(.df), 5)
            .seq <- .seq[-c(1, length(.seq))]
            .seq <- .seq + 0:(length(.seq) - 1)
            sapply(.seq, function(z) {
                .df <<- addHLines(.df, z)
            })
            .df[.seq, ] <- .df[2, ]
        }
        .txt <- paste0("  Summary")
    } else {
        if (!(as.character(by) %in% names(data))) {
            stop(paste0("`", by, "` not found."),
                 call. = FALSE)
        }
        .by <- data[[by]]
        if (na.rm) {
            .chk <- is.na(.by)
            .by <- .by[!.chk]
            data <- data[!.chk, ]
        } else {
            if (is.factor(.by)) .by <- as.character(.by)
            .by[is.na(.by)] <- "NA"
        }

        .df <- summ2(data, .vars, .by, na.rm, digits)
        .df <- do.call(rbind, lapply(.df, function(z) z))
        .df_raw <- .df
        if (!detail) .df <- .df[, -c(7:11)]

        ## Add horizontal and vertical lines for visaul appealing
        hpos <- as.numeric(row.names(.df)[!(.df$Variable == "")])[-1]
        hpos <- hpos + 0:(length(hpos) - 1)
        sapply(hpos, function(z) {
            .df <<- addHLines(.df, z)
        })
        hpos <- (c(hpos, nrow(.df) + 1) - 1) + 0:(length(hpos))
        sapply(hpos, function(z) {
            .df <<- addHLines(.df, z)
            .df[z, 1] <<- ""
        })
        .df <- formatdf(.df, 2, 3)
        .txt <- paste0("  Summary by '", by, "'")
    }

    ## Display information
    cat(paste0(.txt, "\n"))
    print.data.frame(.df, row.names = FALSE, max = 1e9)

    ## get vars lbl
    .vars_lbl <- sapply(c(by, .vars), getLabel, data)
    .vars_lbl <- data.frame(vars = c(as.character(by), .vars),
                            lbl = .vars_lbl)

    ## create list with class for summary
    .list <- list(summ = .df,
                  summ_raw = .df_raw,
                  lbl = .vars_lbl,
                  type = ifelse(detail, "summ2d",
                                ifelse(length(by) == 0, "summ1",
                                       "summ2")))
    class(.list) <- "summ"
    invisible(.list)
}

summ1 <- function(x, data, na.rm = FALSE, digits = 1)
{
    .x <- data[[x]]
    .obs <- ifelse(na.rm, length(.x[!is.na(.x)]), length(.x))
    .na <- length(.x[is.na(.x)])

    ## Set na.rm to TRUE for further operations
    na.rm <- TRUE
    ## construct 7 number summary statistics
    .mu <- mean(.x, na.rm = na.rm)
    .std <- sd(.x, na.rm = na.rm)
    .q <- round(quantile(.x, probs = c(0, .25, .5, .75, 1),
                         na.rm = na.rm), digits)
    .v <- round(c(.mu, .std, .q), digits)
    .v <- sprintf(.v, fmt = paste0("%#.", digits, "f"))
    ## get p value from normality test
    pvalue <- tryCatch({
        suppressWarnings(shapiro.test(.x)$p.value)
    }, error = function(err) {
        return(NA)
    })
    pvalue <- sprintf(pvalue, fmt = '%#.4f')
    ## final .df for return
    .df <- data.frame(x, .obs, .na, .v[1], .v[2], .v[5], .v[4],
                      .v[6], .v[3], .v[7], pvalue)
    names(.df) <- c("Variable", "Obs", "<NA>", "Mean", "Std.Dev",
                    "Median", "25%", "75%", "Min", "Max", "Normal.")
    row.names(.df) <- NULL
    return(.df)
}


summ2 <- function(data, .vars, .by, na.rm = FALSE, digits = 1)
{
    lapply(.vars, function(z) {
        .split <- split(data, .by)
        .sub <- do.call(
            rbind, lapply(.split, function(i) summ1(z, i, TRUE, digits))
        )
        .sub <- rbind(.sub, summ1(z, data, na.rm, digits))
        .x <- data[[z]]
        ## get pvalue  from ANOVA and Kruskal Wallis or t.test / Wilcox
        ## calculate p-values from ANOVA and Kruskal Wallis or t.test / Wilcox
        if (length(.split) > 2) {
            pvalue <- tryCatch(
                {suppressWarnings(summary(aov(.x ~ .by))[[1]][1,5])},
                error = function(cnd) {return(NA)}
            )
            pvalue <- c(pvalue, tryCatch(
                {suppressWarnings(kruskal.test(.x ~ .by)$p.value)},
                error = function(cnd) {return(NA)}))
            .pvalue.name <- c("ANOVA", "K-Wallis")
        } else {
            pvalue <- tryCatch(
                {suppressWarnings(t.test(.x ~ .by)$p.value)},
                error = function(cnd) {return(NA)}
            )
            pvalue <- c(pvalue, tryCatch(
                {suppressWarnings(suppressWarnings(wilcox.test(.x ~ .by)$p.value))},
                error = function(cnd) {return(NA)})
            )
            .pvalue.name <- c("t-test", "Wilcoxon")
        }
        pvalue <- sprintf(pvalue, fmt = '%#.4f')
        ## add pvalue back to .sub
        .sub$p1 <- c(pvalue[1], rep("", nrow(.sub) - 1))
        .sub$p2 <- c(pvalue[2], rep("", nrow(.sub) - 1))
        names(.sub)[(ncol(.sub)-1):ncol(.sub)] <- .pvalue.name
        .sub$Variable[-1] <- ""
        .sub$Level <- c(row.names(.sub)[-nrow(.sub)], "Total")
        row.names(.sub) <- NULL
        .sub <- cbind(Variable = .sub[, 1], Level = .sub[, ncol(.sub)],
                      .sub[, 2:(ncol(.sub) - 1)])
        .sub
    })
}




# INFERENTIAL STATISTICS --------------------------------------------------




# LINEAR REGRESSION -------------------------------------------------------

#' @title Linear Regression Model
#'
#' @description
#' \code{regress()} produces summary of the model
#' with coefficients and 95% Confident Intervals.
#'
#' @param model glm or lm model
#' @param vce if `TRUE`, robust standard errors are calculated.
#' @param digits specify rounding of numbers. See \code{\link{round}}.
#'
#' @details
#'
#' \code{regress} is based on \code{\link{lm}}. All statistics presented
#' in the function's output are derivatives of \code{\link{lm}},
#' except AIC value which is obtained from \code{\link{AIC}}.
#' It uses `lm()` function to run the model.
#'
#' \strong{Outputs}
#'
#' Outputs can be divided into three parts.
#'
#' 1) `Info of the model`:
#' Here provides number of observations (Obs.), F value, p-value
#' from F test,
#' R Squared value, Adjusted R Squared value, square root of mean square
#' error
#' (Root MSE) and AIC value.
#'
#' 2) `Errors`:
#' Outputs from `anova(model)` is tabulated here. SS, DF and MS indicate
#' sum of square of errors, degree of freedom and mean of square of errors.
#'
#' 3) `Regression Output`:
#' Coefficients from summary of model are tabulated here along with 95\%
#' confidence interval.
#'
#' \strong{using Robust Standard Errors}
#'
#' if heteroskedasticity is present in our data sample,
#' the ordinary least square (OLS) estimator will remain unbiased
#' and consistent,
#' but not efficient. The estimated OLS standard errors
#' will be biased and cannot be solved with a larger sample size.
#' To remedy this, robust standard errors can be used to adjusted
#' standard errors.
#'
#' The `regress` uses sandwich estimator to estimate Huber-White's standard
#' errors. The calculation is based on the tutorial by Kevin Goulding.
#'
#' \deqn{Variance of Robust = (N / N - K) (X'X)^(-1)
#'  \sum{Xi X'i ei^2} (X'X)^(-1)}
#'
#' where N =  number of observations, and K =  the number of regressors
#' (including the intercept). This returns a Variance-covariance (VCV)
#' matrix
#' where the diagonal elements are the estimated heteroskedasticity-robust
#' coefficient
#' variances — the ones of interest. Estimated coefficient standard errors
#' are the square root of these diagonal elements.
#'
#'
#' @note
#'
#' Credits to Kevin Goulding, The Tarzan Blog.
#'
#' @return
#'
#' a list containing
#'
#' 1. `info` - info and error tables
#' 2. `reg` - regression table
#' 3. `model` - raw model output from `lm()`
#' 4. `fit` - formula for fitting the model
#' 5. `lbl` - variable labels for further processing in `summary`.
#'
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' fit <- lm(Ozone ~ Wind, data = airquality)
#' regress(fit)
#'
#' \dontrun{
#' ## labelling variables
#' airquality2 <- label(airquality, Ozone = "Ozone level", Wind = "Wind Speed")
#' fit2 <- lm(Ozone ~ Wind, data = airquality2)
#' reg <- regress(fit2)
#' str(reg)
#' }
#'
#' @export
regress <- function(model, vce = FALSE, digits = 5)
{
    ## match call arguments
    .args <- as.list(match.call())

    ## if input is not a lm or glm, stop
    if (!any(class(model) %in% c("glm", "lm"))) {
        stop(paste0("`", .args$model, "` must be linear model"),
             call. = FALSE)
    }

    ## refitting model for glm and lm
    .getcall <- getCall(model)
    .data_name <- .getcall$data
    data <- eval(.data_name)
    .formula <- .getcall$formula
    .txt <- paste0("lm(", Reduce(paste, deparse(.formula)),
                   ", data = ", .data_name, ")")
    .model <- eval(parse(text = .txt))

    ## calculate errors and statistics
    .err <- calcRegress(.model, digits)

    ## Calculate model coefficients and SE
    if (vce) {
        .coef <- vceRobust(.model)
        .txt <- paste0("            Linear Regression Output",
                       " with Robust Standard Error")
    } else {
        .coef <- data.frame(coef(summary(.model)))
        .txt <- paste0("                Linear Regression Output")
    }
    ## put intercept to last and names
    .coef <- rbind(.coef[-1, ], .coef[1, ])
    names(.coef) <- c("e", "se", "t", "p")

    ## gather all statistics
    .t <- data.frame(
        cbind(
            row.names(.coef),
            sprintf(.coef$e, fmt = paste0('%#.', digits, 'f')),
            sprintf(.coef$se, fmt = paste0('%#.', digits, 'f')),
            sprintf(.coef$t, fmt = paste0('%#.', 2, 'f')),
            sprintf(.coef$p, fmt = paste0('%#.', digits, 'f')),
            sprintf(.coef$e - (1.96 * .coef$se),
                    fmt = paste0('%#.', digits, 'f')),
            sprintf(.coef$e + (1.96 * .coef$se),
                    fmt = paste0('%#.', digits, 'f'))
        )
    )
    ## get y name
    .y_name <- as.character(.formula)[2]
    names(.t) <- c(.y_name, "Coef.", "Std.Err", "t",
                   "P>|t|", "[95% Conf.", "Interval]")
    .t <- formatdf(.t, 2, 2)


    ## Print tabulation and labels
    # printDFlenLines(.t)
    cat(paste0("       ", .txt, "\n"))
    # printDFlenLines(.t)
    print.data.frame(.err, row.names = FALSE, max = 1e9)
    print.data.frame(.t, row.names = FALSE, max = 1e9)
    cat(paste0("  Model fit: ",
               Reduce(paste, deparse(getCall(model))), "\n"))

    ## get raw data for labelling
    .vars <- all.vars(formula(.model)[-2])
    ## get vars lbl
    .vars_lbl <- sapply(c(.y_name, .vars), getLabel, data)
    .vars_lbl <- data.frame(vars = c(as.character(.y_name), .vars),
                            lbl = .vars_lbl)
    .vars_lbl$lbl[is.na(.vars_lbl$lbl)] <- .vars_lbl$vars[is.na(.vars_lbl$lbl)]


    ## create list with class
    .list <- list(info = .err,
                  reg = .t,
                  model = .model,
                  fit = .formula,
                  data = data,
                  lbl = .vars_lbl)

    ## add label for further processing
    attr(.t, "label") <- ifelse(
        vce, "Linear Regression with Robust Standard Errors",
        "Linear Regression"
    )

    ## create class for S3 method to use in summary()
    class(.list) <- "regress"

    invisible(.list)
}

calcRegress <- function(model, rnd)
{
    ## model summary and F test
    .s <- summary(model)
    .aov <- anova(model)

    ## calculate errors
    ## errors without residuals
    .t <- .aov[-nrow(.aov), ]

    ## errors with residuals
    .r <- .aov["Residuals", 1:3]
    .r <- rbind(c(colSums(.t[, 1:2]),
                  "Mean Sq" = mean(.t[["Mean Sq"]])), .r)

    ## DSS Princeton linear101 training
    ## add total row
    .df <- sum(.r$Df)
    .ss <- sum(.r$`Sum Sq`)
    .msq <- .ss / .df
    .r[3, ] <- c(.df, .ss, .msq)
    .r <- cbind(c("Model", "Residual", "Total"), .r)
    names(.r) <- c("Source", "DF", "SS", "MS")

    ## calculate MSE
    .mse <- sqrt(.r["Residuals", "MS"])
    .mse <-  sprintf(.mse, fmt = paste0("%#.", 2, "f"))
    ## fix decimal places
    .r[, "SS"] <- sprintf(.r[, "SS"], fmt = paste0("%#.", 1, "f"))
    .r[, "MS"] <- sprintf(.r[, "MS"], fmt = paste0("%#.", 1, "f"))
    row.names(.r) <- NULL


    ## number of observations, f statistics, pvalue
    .obs <- c("Number of Obs", length(.s$residuals))
    ## calculate F statistics
    .f <- .s$fstatistic
    .fv <- .f["value"]
    .fndf <- .f["numdf"]
    .fddf <- .f["dendf"]
    if (is.null(.f)) {
        .fv <- .fndf <- 0
        .fddf <- length(.s$residuals) - 1
        .pf <- 0
    } else {
        .pf <- pf(.fv, .fndf, .fddf, lower.tail = FALSE)
    }

    .f <- c(paste0("F(", .fndf, ", ", .fddf, ")"),
            sprintf(.fv, fmt = paste0("%#.", 2, "f")))
    .pf <- c("Prob > F",
             sprintf(.pf, fmt = paste0("%#.", rnd, "f")))

    ## calculate r squared, adj r squared
    .r2 <- c("R-Squared",
             sprintf(.s$r.squared, fmt = paste0("%#.", rnd, "f")))
    .r2_adj <- c(
        "Adj R-Squared",
        sprintf(.s$adj.r.squared, fmt = paste0("%#.", rnd, "f"))
    )

    ## calculate AIC
    .aic <- AIC(model)
    .aic <- sprintf(.aic, fmt = paste0("%#.", 2, "f"))

    ## result table
    data <- data.frame(rbind(.obs, .f, .pf, .r2, .r2_adj))
    names(data) <- c("Stats", "Value")
    data <- formatdf(data, 2, 2)
    data

    # ## process ERROR Table to return
    .r <- rbind(.r, c("AIC", .aic, "Root MSE", .mse))
    .r <- formatdf(.r, 2, 2)
    .r <- rbind(.r[1:4, ], .r[7, ], .r[5, ], .r[7, ], .r[6, ])
    .r[8, c(1, 3, 7)] <- ""
    .r <- .r[, -c(1, ncol(.r))]
    row.names(.r) <- NULL

    data <- cbind(data, "   ", .r)
    names(data) <- data[2, ]
    return(data)
}


## robust standard errors for heteroskedasticity
## Heteroskedasticity-robust standard error calculation.
## https://thetarzan.wordpress.com/2011/05/28/heteroskedasticity
## -robust-and-clustered-standard-errors-in-r/
vceRobust <- function(.model) {

    .s <- summary(.model)
    .matrix <- model.matrix(.model)
    .square <- residuals(.model)^2
    .xdx <- 0


    ## Here one needs to calculate X'DX. But due to the fact that
    ## D is huge (NxN), it is better to do it with a cycle.
    sapply(1:nrow(.matrix), function(z) {
        .xdx <<- .xdx + .square[z] * .matrix[z, ] %*% t(.matrix[z, ])
    })


    # inverse(X'X)
    .inverse <- solve(t(.matrix) %*% .matrix)

    # Variance calculation (Bread x meat x Bread)
    .var.covar <- .inverse %*% .xdx %*% .inverse

    # degrees of freedom adjustment
    .df.adj <- sqrt(nrow(.matrix))/sqrt(nrow(.matrix)-ncol(.matrix))

    # Standard errors of the coefficient estimates are the
    # square roots of the diagonal elements
    .se <- .df.adj * sqrt(diag(.var.covar))

    .t <- .model$coefficients / .se
    .p <- 2 * pnorm(-abs(.t))


    ## gather all statistics
    data <- cbind(.model$coefficients, .se, .t, .p)
    data <- data.frame(data)
    names(data) <- colnames(.s$coefficients)

    return(data)
}



##' @rdname regress
##'
##' @description
##'
##' \code{`predict.regress`} a S3 method for \code{predict} to generate
##' statistics related to the prediction of the linear model using the output
##' from the \code{regress} function of the \code{mStats}.
##'
##' @details
##'
##'
##' \code{`predict.regress`} generates an original data with statistics for model
##' diagnostics:
##'
##' 1. `fitted` (Fitted values)
##'
##' 2. `resid` (Residuals)
##'
##' 3. `std.resid` (Studentized Residuals)
##'
##' 4. `hat` (leverage)
##'
##' 5. `sigma`
##'
##' 6. `cooksd` (Cook's Distance)
##'
##' @inheritParams stats::predict
##'
##' @examples
##'
##' \dontrun{
##' predict(reg)
##' }
##'
##' @export
predict.regress <- function(object, ... )
{
    ## get the model from list object
    .model <- object$model
    ## get the original data
    data <- object$data
    ## get vars name in the model
    .vars <- all.vars(formula(.model))

    ## calculate model diagnostic statistics
    .hat <- data.frame(lm.influence(.model))
    .dx <- cbind(.model$model,
                 fitted = fitted(.model),
                 resid = resid(.model),
                 std.resid = rstandard(.model),
                 hat = .hat$hat,
                 sigma = .hat$sigma,
                 cooksd = cooks.distance(.model))

    ## merge the two datasets by left-join
    .df <- merge.data.frame(data, .dx,
                            by = .vars, all.x = TRUE)

    attr(.df$fitted, "label") <- "Fitted values"
    attr(.df$resid, "label") <- "Residuals"
    attr(.df$std.resid, "label") <- "Studentized Residuals"
    attr(.df$hat, "label") <- "Leverage or hat values"
    attr(.df$sigma, "label") <- "hat sigma"
    attr(.df$cooksd, "label") <- "Cook's Distance"

    return(.df)
}


##' @rdname regress
##'
##' @description
##'
##' \code{`plot.regress`} is a S3 method for \code{plot()} to create
##' graphs for checking diagnostics of linear model using the output from
##' the \code{regress} function of the \code{mStats}.
##'
##' @inheritParams base::plot
##'
##' @examples
##'
##' \dontrun{
##' plot(reg)
##' }
##'
##' @export
plot.regress <- function(x, ... )
{
    .model <- x$model

    ## Set graph parameters
    par(mfrow = c(2, 2))

    plot(.model)

    ## Reset graph parameters
    par(mfrow = c(1, 1))
}


##' @rdname regress
##'
##' @description
##'
##' \code{`ladder`} converts a variable into a normally
##' distributed one.
##'
##' @param data dataset
##' @param var variable name
##'
##'
##' @examples
##'
##' ladder(airquality, Ozone)
##'
##' @export
ladder <- function(data, var)
{
    ## match call arguments
    .args <- as.list(match.call())
    x <- as.character(.args$var)
    .x <- data[[.args$var]]

    ## get ladder vectors
    .t <- list(.x^3,
               .x^2,
               .x,
               sqrt(.x),
               log(.x),
               1 / sqrt(.x),
               1 / .x,
               1 / .x^2,
               1 / .x^3)

    ## get chi W and p-value
    .s <- do.call(
        rbind,
        lapply(.t, function(z) {
            .s <- shapiro.test(z)
            sprintf(c(.s$statistic, .s$p.value),
                    fmt = paste0("%#.", 5, "f"))
        })
    )

    ## combine all
    .df <- data.frame(c("cube", "squre", "raw", "square-root", "log",
                        "reciprocal root", "reciprocal", "reciprocal square",
                        "reciprocal cube"),
                      c(paste0(x, c("^3", "^2", "")),
                        paste0("sqrt(", x, ")"),
                        paste0("log(", x, ")"),
                        paste0("1 / sqrt(", x, ")"),
                        paste0("1 / ", x),
                        paste0("1 / (", x, "^2)"),
                        paste0("1 / (", x, "^3)")),
                      .s)
    names(.df) <- c("Transformation", "formula", "W", "P-Value")
    .df <- formatdf(.df, 2, 2)

    ## Display information
    printDFlenLines(.df)
    cat(paste0("       Ladder of Transformation : ", x, "\n"))
    printDFlenLines(.df)
    print.data.frame(.df, row.names = FALSE, max = 1e9)

    invisible(.df)
}


##' @rdname regress
##'
##' @description
##'
##' \code{`hettest`} performs the Breusch-Pagan test
##' for heteroskedasticity.
##' It presents evidence against the
##' null hypothesis that t=0 in Var(e)=sigma^2 exp(zt).
##' The formula are based on the \code{bptest} function
##' in \code{lmtest} package.
##'
##' @param regress output from \code{regress}
##' @param studentize logical.
##' If set to \code{TRUE} Koenker's studentized version
##' of the test statistic will be used.
##'
##' @details
##'
##' The `Breusch-Pagan test` fits a linear regression model
##' to the residuals of a linear regression model
##' (by default the same explanatory variables are taken as
##' in the main regression model) and rejects if too
##' much of the variance is explained by the additional
##' explanatory variables. Under \eqn{H_0} the test statistic
##' of the Breusch-Pagan test follows a chi-squared distribution
##' with \code{parameter} (the number of regressors without
##' the constant in the model) degrees of freedom.
##'
##'
##' @references
##'
##' T.S. Breusch & A.R. Pagan (1979),
##'      A Simple Test for Heteroscedasticity and Random
##'      Coefficient Variation.
##'      \emph{Econometrica} \bold{47}, 1287--1294
##'
##' R. Koenker (1981), A Note on Studentizing a Test for
##'       Heteroscedasticity. \emph{Journal of Econometrics}
##'       \bold{17}, 107--112.
##'
##' W. Krämer & H. Sonnberger (1986),
##'       \emph{The Linear Regression Model under Test}.
##'       Heidelberg: Physics
##'
##'
##' @examples
##'
##' \dontrun{
##' hettest(reg)
##' }
##'
##' @export
hettest <- function(regress, studentize = FALSE)
{
    .model <- regress$model

    ## get residual square
    .n <- nobs(.model)
    .r <- resid(.model)
    .s2 <- sum(.r^2) / .n

    ## get model matrix for lm.fit
    .Z <- model.matrix(.model)

    if (studentize) {
        .w <- .r^2 - .s2
        .aux <- lm.fit(.Z, .w)
        .bp <- .n * sum(.aux$fitted.values^2) / sum(.w^2)
        .txt <- "Studentized Breusch-Pagan test for heteroskedasticity"
        .txt <- paste0("    ", .txt)
    } else {
        .w <- .r^2 / .s2 - 1
        .aux <- lm.fit(.Z, .w)
        .bp <- 0.5 * sum(.aux$fitted.values^2)
        .txt <- "Breusch-Pagan test for heteroskedasticity"
        .txt <- paste0("          ", .txt)
    }

    .df <- .aux$rank - 1
    .p <- pchisq(.bp, .df, lower.tail = FALSE)

    ## gather all statistics
    .df <- cbind(
        "Constant variance",
        paste0("Fitted values of ", getCall(.model)$formula[2]),
        sprintf(.bp, fmt = paste0('%#.', 2, 'f')),
        sprintf(.p, fmt = paste0('%#.', 5, 'f'))
    )
    .df <- data.frame(.df)
    names(.df) <- c("Null Hypothesis", "Variables",
                    "Chi2(1)", "Prob > Chi2")
    .df <- formatdf(.df, 2, 2)

    ## Display information
    cat(paste0("\t    Breusch-Pagan test for heteroskedasticity\n"))
    print.data.frame(.df, row.names = FALSE, max = 1e9)

    invisible(.df)
}



##' @rdname regress
##'
##' @description
##'
##' \code{`linkTest`} determines whether a model in R is
##' 'well specified' using the `STATA`'s `linkTest`.
##'
##' @details
##'
##' The code for \code{`linkTest`} has been modified from Keith Chamberlain's linktext.
##' www.ChamberlainStatistics.com
##' https://gist.github.com/KeithChamberlain/8d9da515e73a27393effa3c9fe571c3f
##'
##'
##' @examples
##'
##' \dontrun{
##' linkTest(fit)
##' }
##'
##' @export
linkTest <- function(model, vce = FALSE, digits = 5)
{
    ## get the model from list object
    .model <- model
    ## get the original data
    data <- getCall(.model)$data
    .data <- eval(data)
    ## get vars name in the model
    .vars <- all.vars(formula(.model))

    ## predict hat values
    .fit <- predict(.model)
    .fit2 <- .fit^2
    # Check to see that the predicted and predicted^2 variable actually
    # vary.
    if(round(var(.fit), digits=2) == 0){
        stop("No parameters that vary. Cannot perform test.")
    }
    .df <- cbind(.model$model,
                 hat_ = .fit,
                 hatsq_ = .fit2)
    # ## merge the two datasets by left-join
    # .df <- merge.data.frame(.data, .dx,
    #                         by = .vars, all.x = TRUE, all.y = FALSE)
    .df_name <- paste0(data, "_linkTest")
    assign(.df_name, .df, envir = environment(formula(model)))

    ## re fit model with hat_ and hatsq_
    .txt <- paste0("lm(", .vars[1], " ~ hat_ + hatsq_, data = ",
                   .df_name, ")")
    .refit <- eval(parse(text = .txt))
    .list <- regress(.refit, vce = vce, digits = digits)

    invisible(.list)
}




# LOGITISTIC REGRESSION ---------------------------------------------------

#' @title Logistic Regression Model
#'
#' @description
#' \code{logit()} produces summary of the model with
#' coefficients or odds ratios (`OR`) and 95% Confident Intervals.
#'
#' @param model glm or lm model
#' @param or `TRUE`reports odds ratios instead of coefficients
#' @param digits specify rounding of numbers. See \code{\link{round}}.
#'
#' @details
#'
#' \code{logit()} is based on \code{\link{glm}} with `binomial` family.
#' All statistics presented in the function's output are derivatives of
#' \code{\link{glm}},
#' except AIC value which is obtained from \code{\link{AIC}}.
#'
#' \strong{Outputs}
#'
#' Outputs can be divided into three parts.
#'
#' 1) `Info of the model`:
#' Here provides number of observations (Obs.), chi value from Likelihood Ratio
#' test (LR chi2) and its degree of freedom, p-value from LR test,
#' Pseudo R Squared, log likelihood and AIC values.
#'
#' 2) `Regression Output`:
#' Coefficients from summary of model are tabulated here along with 95\%
#' confidence interval.
#'
#' @return
#'
#' a list containing
#'
#' 1. `info` - info and error tables
#' 2. `reg` - regression table
#' 3. `model` - raw model output from `lm()`
#' 4. `fit` - formula for fitting the model
#' 5. `lbl` - variable labels for further processing in `summary`.
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' mylogit <- glm(case ~ education + age + parity, family = binomial,
#'            data = infert)
#' logit(mylogit)
#'
#' \dontrun{
#' ## Example from UCLA website:
#' ## LOGIT REGRESSION | R DATA ANALYSIS EXAMPLES
#' ## https://stats.idre.ucla.edu/r/dae/logit-regression/
#'
#' mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
#' mydata <- replace(mydata, rank, factor(rank))
#' mydata <- label(mydata, gre = "GRE", gpa = "GPA score", rank = "Ranking")
#' mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
#'
#' ## Showing Odds Ratios
#' logit(mylogit)
#'
#' ## Showing coefficients
#' logit(mylogit, or = FALSE)
#' }
#'
#' @export
logit <- function(model, or = TRUE, digits = 5)
{
    ## match call arguments
    .args <- as.list(match.call())

    ## refitting model for glm and lm
    .getcall <- getCall(model)
    .data_name <- .getcall$data
    data <- eval(.data_name)
    .formula <- .getcall$formula

    ## if input is not a lm or glm, stop
    if (!any(class(model) %in% c("glm", "lm"))) {
        stop(paste0("`", .args$model, "` must be glm model"),
             call. = FALSE)
    }
    ## Calculate model info
    .err <- calcLogit(model, digits)

    ## get coefficients
    .s <- summary(model)
    .coef <- data.frame(coef(.s)[, 1:3],
                        confint.default(model),
                        coef(.s)[, 4])
    ## put intercept to last and names
    .coef <- rbind(.coef[-1, ], .coef[1, ])
    names(.coef) <- c("e", "se", "z", "ll", "ul", "p")

    ## calculate OR
    if (or) {
        .coef$se <- sqrt(exp(coef(model))^2 * diag(vcov(model)))
        .coef$e <- exp(.coef$e)
        .coef$ll <- exp(.coef$ll)
        .coef$ul <- exp(.coef$ul)
    }

    ## gather all statistics
    .t <- data.frame(
        cbind(
            row.names(.coef),
            sprintf(.coef$e, fmt = paste0('%#.', digits, 'f')),
            sprintf(.coef$se, fmt = paste0('%#.', digits, 'f')),
            sprintf(.coef$z, fmt = paste0('%#.', 2, 'f')),
            sprintf(.coef$p, fmt = paste0('%#.', digits, 'f')),
            sprintf(.coef$ll, fmt = paste0('%#.', digits, 'f')),
            sprintf(.coef$ul,
                    fmt = paste0('%#.', digits, 'f'))
        )
    )
    ## get y name
    .y_name <- as.character(.formula)[2]
    names(.t) <- c(.y_name, ifelse(or, "Odds Ratio", "Coef."),
                   "Std.Err", "z", "P>|z|", "[95% Conf.", "Interval]")
    .t <- formatdf(.t, 2, 2)

    ## Print tabulation and labels
    # printDFlenLines(.t)
    cat(paste0("\t\t\tLogistic Regression Output\n"))
    # printDFlenLines(.t)
    print.data.frame(.err, row.names = FALSE, max = 1e9)
    print.data.frame(.t, row.names = FALSE, max = 1e9)
    cat(paste0("  Model fit: ",
               Reduce(paste, deparse(getCall(model))), "\n"))

    ## get raw data for labelling
    .vars <- all.vars(formula(model)[-2])
    ## get vars lbl
    .vars_lbl <- sapply(c(.y_name, .vars), getLabel, data)
    .vars_lbl <- data.frame(vars = c(as.character(.y_name), .vars),
                            lbl = .vars_lbl)
    .vars_lbl$lbl[is.na(.vars_lbl$lbl)] <- .vars_lbl$vars[is.na(.vars_lbl$lbl)]

    ## create list with class
    .list <- list(info = .err,
                  reg = .t,
                  model = model,
                  fit = .formula,
                  data = data,
                  lbl = .vars_lbl)

    ## add label for further processing
    attr(.t, "label") <- "Logistic Regression"
    ## create class for S3 method to use in summary()
    class(.list) <- "logit"

    invisible(.list)
}


calcLogit <- function(model, digits)
{
    ## model summary and F test
    .s <- summary(model)
    ## number of observations, f statistics, pvalue
    .obs <- c("Number of Obs", length(resid(model)))

    ## LR Chi Square Test
    .chi <- with(model, null.deviance - deviance)
    .chi.df <- with(model, df.null - df.residual)
    .chi.p <- pchisq(.chi, .chi.df, lower.tail = FALSE)
    .chi <- c(paste0("LR chi2(", .chi.df, ")"),
              sprintf(.chi, fmt = paste0("%#.", 2, "f")))
    .chi.p <- c("Prob > chi2",
                sprintf(.chi.p, fmt = paste0("%#.", digits, "f")))

    ## calculate pseudo R-squared
    ## https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-what-are-pseudo-r-squareds/
    ## https://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/
    ## https://stats.stackexchange.com/questions/8511/how-to-calculate-pseudo-r2-from-rs-logistic-regression
    # McFadden's R squared
    .pr2 <- 1 - model$deviance / model$null.deviance
    .pr2 <- c("Pseudo R-Squared",
              sprintf(.pr2, fmt = paste0("%#.", digits, "f")))

    .ll <- c("Log likelihood",
             sprintf(logLik(model), fmt = paste0("%#.", 2, "f")))

    ## get AIC value
    .aic <- c("AIC",
              sprintf(.s$aic, fmt = paste0("%#.", 2, "f")))

    ## Combine tables
    .df1 <- data.frame(rbind(.obs, .chi, .chi.p))
    names(.df1) <- c("Stats", "Value")
    .df1 <- formatdf(.df1, 2, 2)
    .df2 <- data.frame(rbind(.aic, .pr2, .ll))
    .df <- cbind(.df1, "        ", rbind("", "", .df2, ""))
    names(.df) <- .df[2, ]
    row.names(.df) <- NULL

    return(.df)
}




# MHOR --------------------------------------------------------------------

#' @title Calculating Odds Ratios
#'
#' @description
#' \code{mhor()} calculates odds ratios, Mantel Haenszel pooled estimates and
#' 95% CI.
#'
#' @param data data.frame
#' @param exp exposure or independent variables
#' @param case case or dependent variables (outcomes)
#' @param strata if specified, MH OR is calculated.
#' @param exp_value value for exposure as reference
#' @param case_value value for outcome as reference
#' @param digits specify rounding of numbers. See \code{\link{round}}.
#'
#' @details
#'
#' Rows and Columns can be rearranged by specifying
#' `exp_value` and `case_value`. This is used
#' when the exposed and case values are not at the right place in 2x2 tables.
#'
#' Reference row value can be specified in `exp_value`.
#'
#'
#' Attributable fractions, \code{Attr. Frac. Exp} and  \code{Attr. Frac. Pop}
#' among exposed and population are calculated when OR is greater than or
#' equal to 1.
#' If OR is less than 1, preventable fractions,  \code{Prev. Frac. Exp}
#' and  \code{Attr. Frac. Pop} are calculated.
#'
#' It produces a table with Odds Ratio, 95% CI as well as
#' p-value. If \code{strata} is specified, `Mantel-Haenzsel` Pooled
#' estimates of `Odds Ratio` is generated along with Chi-squared test for
#' homogeneity.
#'
#'
#' \strong{Odds Ratio, OR}
#'
#' \deqn{OR = (D1 x H0) / (D0 x H1)}
#'
#' \strong{Error Factor, EF using Woolf's formula}
#'
#' \deqn{95\% CI = OR / EF or OR x EF}
#'
#' \deqn{EF = exp(1.96 x SE(log(OR)))}
#'
#' \deqn{SE(log(OR)) = \sqrt{1/D1 + 1/H1 + 1/D0 + 1/H0}}
#'
#' \strong{Calculating p-value from Wald's z test}
#'
#' \deqn{z = log OR / SE (log OR)}
#'
#'
#'
#' \strong{Mantel-Haenszel's OR}
#'
#' \deqn{ORMH = Q / R}
#'
#' \deqn{Q = \sum{(D1i x H0i) / ni}}
#'
#' \deqn{R = \sum{(D0i x H1i) / ni}}
#'
#' \strong{Calculating CI for MH-OR}
#'
#' \deqn{95\% CI = OR / EF or OR x EF}
#'
#' \deqn{SE(ORMH) = \sqrt{V / (Q x R)}}
#'
#' \deqn{V = \sum{(Di x Hi x n0i x n1i) / ( (ni)^2 x (ni - 1))}}
#'
#' \strong{Chi-square test for MHOR, df = 1}
#'
#' \deqn{X^2 (MH), Chi-square value = U^2 / V}
#'
#' \deqn{U = O - E}
#'
#' \deqn{O = \sum{D1i}}
#'
#' \deqn{E = \sum{Di x n1i / ni}}
#'
#'
#' \strong{Chi-square test for Heterogeneity}
#'
#' \deqn{X^2 = \sum{(D1i x H0i - ORMH x D0i x H1i)^2 / ORMH x Vi x ni^2}}
#'
#' @return
#'
#' data.frame
#'
#' @references
#'
#' \enumerate{
#'     \item Betty R. Kirkwood, Jonathan A.C. Sterne (2006, ISBN:978–0–86542–871–3)
#'     \item B. Burt Gerstman (2013, ISBN:978-1-4443-3608-5)
#'     \item Douglas G Altman (2005, ISBN:0 7279 1375 1)
#' }
#'
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#'
#' ### Example from Essential Medical Statistics
#' # Page 178, Chapter 18: Controlling for confounding: Stratification
#' lepto <- expandtbl(
#'     male = c(36, 14, 50, 50), female = c(24, 126, 10, 90),
#'     exp_name = "area", exp_lvl = c("Rural", "Urban"),
#'     case_name = "ab", case_lvl = c("Yes", "No"),
#'     strata_name = "gender"
#' )
#'
#' ## label variables and data
#' lepto <- label(lepto, "Prevalence survey of leptospirosis in West Indies")
#' lepto <- label(lepto, area="Type of area", ab = "Leptospirosis Antibodies",
#'                   gender="Gener: Male or Female")
#'
#' ## Calculate OR
#' mhor(lepto, area, ab)
#'
#' ## Calculate MHOR
#' mhor(lepto, area, ab, gender)
#'
#'
#' @export
mhor <- function(data, exp , case, strata = NULL, exp_value = NULL,
                 case_value = NULL, digits = 4)
{
    ## match call arguments
    .args <- as.list(match.call())
    ## get names of dataset and headings
    .data_name <- deparse(substitute(data))
    .vars_names <- names(data)

    ## if input is not a data.frame, stop
    if (!is.data.frame(data)) {
        stop(paste0("`", .data_name, "` must be a data.frame"),
             call. = FALSE)
    }

    ## get exp and case name
    exp <- as.character(.args$exp)
    case <- as.character(.args$case)
    strata <- as.character(.args$strata)
    ## check if these variables are in the dataset
    lapply(c(exp, case, strata) , function(z) {
        if (!(z %in% .vars_names)) {
            stop(paste0("'", z, "' not found."), call. = FALSE)
        }
    })

    ## create single vectors for exp and case
    .exp <- data[[exp]]
    .case <- data[[case]]
    ## create table
    .tbl <- table(.exp, .case, useNA = "no")
    ## case var must be binary.
    if (ncol(.tbl) > 2) {
        stop(paste0("'", case, "' must be binary."), call. = FALSE)
    }
    ## if strata is not specified, calculate OR. Otherwise, calculate MHOR
    if (length(strata) == 0) {
        ## change row and col orders
        .tbl <- rowColOrder(.tbl, exp_value, case_value)
        ## split tables if nrow > 2
        .tbl <- splitTables(.tbl, .exp.value = exp_value)
        .df <- lapply(.tbl, calcOR, exp, digits)
        .df <- do.call(rbind,
                       lapply(1:length(.df), function(z) {
                           if (z > 1) {
                               .df[[z]][-1, ]
                           } else {
                               .df[[z]]
                           }
                       })
        )

        ## create label
        .txt <- paste0(" Measure of Association : Odds Ratio of '", case, "'")
    } else {
        ## exp var must be binary for MHOR.
        if (nrow(.tbl) > 2) {
            stop(paste0("'", exp, "' must be binary."), call. = FALSE)
        }
        ## create single vector for strata
        .strata <- data[[strata]]
        .df <- calcMHOR(.exp, .case, .strata, exp_value, case_value, digits)
        .t <- do.call(rbind,
                      lapply(split(data, .strata), function(z) {
                          x <- z[[exp]]
                          y <- z[[case]]
                          calcOR(table(x, y, useNA = "no"), exp, digits)[9, -c(1, 3, 8)]
                      }))
        .t[, 1] <- row.names(.t)
        .oa <- calcOR(table(.exp, .case, useNA = "no"), exp, digits)[9, -c(1, 3, 8)]
        .oa[, 1] <- "Crude"
        names(.t) <- names(.oa) <- names(.df)

        .df <- rbind(.t, .oa, .df)
        .df <- formatdf(.df, 2, 2)
        .nrow <- nrow(.df)
        .df <- rbind(.df[1:(.nrow - 4),], .df[2, ], .df[(.nrow - 3):(.nrow - 2), ],
                     .df[2, ], .df[.nrow - 1, ], .df[2, ])

        ## create label
        .txt <- paste0("\t\t\t Mantel-Haenszel (M-H) Odds Ratios")
    }

    ## Print tabulation and labels
    # printDFlenLines(.t)
    cat(paste0("       ", .txt, "\n"))
    # printDFlenLines(.t)
    print.data.frame(.df, row.names = FALSE, max = 1e9)

    ## get vars lbl
    sapply(c(exp, case, strata), getLabel, data)

    invisible(.df)
}

calcMHOR <- function(.exp, .case, .strata, exp_value, case_value, digits)
{

    ## make three tables
    .tbl <- table(.exp, .case, .strata, useNA = "no")
    .lvl_len <- dim(.tbl)[3]

    ## calculate MHOR
    .t <- tryCatch({
        mantelhaen.test(.tbl, correct = FALSE)
    }, warning = function(w) {
        mantelhaen.test(.tbl, correct = FALSE, exact = TRUE)
    }, error = function(cnd) {
        return(NA)
    })
    .mhor <- c("M-H Combined",
               sprintf(c(.t$estimate, .t$conf.int, .t$p.value),
                       fmt = paste0("%#.", digits, "f")))

    # calculate chi-square and p-value for homogeneity test
    # Woolf test of homogeneity of odds ratios (Jewell 2004, page 154).
    .t <- do.call(
        rbind,
        lapply(1:.lvl_len, function(z) {
            ## change row and col orders
            .t <- rowColOrder(.tbl[,,z], exp_value, case_value)
            .strata_name <- dimnames(.tbl)$.strata[z]
            a <- as.numeric(.t[1, 1])
            b <- as.numeric(.t[1, 2])
            c <- as.numeric(.t[2, 1])
            d <- as.numeric(.t[2, 2])

            or <- log(((a + 0.5) * (d + 0.5)) /
                          ((b + 0.5) * (c + 0.5)))
            var <- (1 / (a + 0.5)) +
                (1 / (b + 0.5)) + (1 / (c + 0.5)) + (1 / (d + 0.5))
            w <- 1 / as.numeric(var)
            wor <- w * or

            ## calculate OR
            res <- calcOR(.t, "tocombine", digits)[7, -2]
            res[1, 1] <- .strata_name

            ## return result
            .df <- data.frame(res, or, w, wor)
            names(.df)[1:5] <- c("Level", "Estimate",
                                 "[95% Conf.", "Interval]", "Pr>chi2")
            .df
        })
    )
    lnOR <- sum(.t$wor) / sum(.t$w)
    # Equation 10.3 from Jewell (2004):
    .chi <- sum(.t$w * (.t$or - lnOR)^2)
    .pvalue <- tryCatch({
        suppressWarnings(pchisq(.chi, df = .lvl_len - 1, lower.tail = FALSE))
    }, error = function(err) {
        return(NA)
    })


    ## combine estimates
    .t <- .t[, 1:5]
    .df <- data.frame(
        rbind(
            .mhor,
            c("M-H Homogeneity Test", paste0("chi(", .lvl_len - 1, ")"),
              sprintf(.chi, fmt = paste0('%#.', 2, 'f')), "Pr>chi2",
              sprintf(.pvalue, fmt = paste0('%#.', digits, 'f')))
        )
    )
    names(.df) <- names(.t)
    return(.df)
}

calcOR <- function(.tbl, exp, rnd)
{
    ## create row and column totals and odds.
    .tblr <- cbind(.tbl, Total = rowSums(.tbl))
    .tblc <- rbind(.tblr, Total = colSums(.tblr))
    .tblf <- cbind(
        .tblc,
        Odds = sprintf(.tblc[, 1] / .tblc[, 2], fmt = paste0("%#.", rnd, "f"))
    )

    ## calculate odds ratio, 95% CI, and p-value
    .a <- as.numeric(.tbl[1, 1])
    .b <- as.numeric(.tbl[1, 2])
    .c <- as.numeric(.tbl[2, 1])
    .d <- as.numeric(.tbl[2, 2])
    .or <- (.a * .d) / (.b * .c)
    .fisher <- tryCatch({
        suppressWarnings(fisher.test(.tbl))
    }, error = function(cnd) {
        return(NA)
    })
    .confint <- .fisher$conf.int
    ## get p-value chi-square
    .chi <- tryCatch({
        suppressWarnings(chisq.test(.tbl, correct = FALSE))
    }, error = function(cnd) {
        return(NA)
    })

    ## calculate attributable or prevent
    .m1 <- .a + .b
    if (.or >= 1) {
        .afe <- (.or - 1) / .or
        .afe_confint <- (.confint - 1) / .confint
        .afp <- .afe * .a / .m1
        .lbl <- c("Odds Ratio", "Attr. Frac. Exp", "Attr. Frac. Pop")
    } else {
        .afe <- 1 - .or
        .afe_confint <- c(1 - .confint[2], 1- .confint[1])
        .afp <- (.a / .m1 * .afe) / ((.a / .m1 * .afe) + .or)
        .lbl <- c("Odds Ratio", "Prev. Frac. Exp", "Prev. Frac. Pop")
    }

    ## combine all in one dataframe
    .df <- data.frame(row.names(.tblf), .tblf)

    ## get other estimates
    .est <- data.frame(
        .lbl,
        sprintf(c(.or, .afe, .afp),
                fmt = paste0("%#.", rnd, "f")),
        c(sprintf(c(.confint[1], .afe_confint[1]),
                  fmt = paste0("%#.", rnd, "f")), ""),
        c(sprintf(c(.confint[2], .afe_confint[2]),
                  fmt = paste0("%#.", rnd, "f")), ""),
        c(sprintf(.chi$p.value,
                  fmt = paste0("%#.", rnd, "f")), "", "")
    )
    names(.est) <- names(.df) <- c(exp, colnames(.tblf))

    ## combine into final dataframe
    .df <- rbind(.df,
                 c("Measure", "Estimate", "[95% Conf.", "Interval]", "Pr>chi2"),
                 .est)
    .df <- formatdf(.df, 2, 2)
    .df <- rbind(.df[1:5, ], .df[10, ], .df[6, ], .df[10, ], .df[7:9, ], .df[10, ])
    row.names(.df) <- NULL

    return(.df)
}




# MHRR --------------------------------------------------------------------

#' @title Calculating Risk Ratios
#'
#' @description
#' \code{mhrr()} calculates different measures of risk including risk
#' ratios (RR) as well as
#' Mantel-Haenszel pooled estimates.
#'
#' @param data data.frame
#' @param exp exposure or independent variables
#' @param case case or dependent variables (outcomes)
#' @param strata if specified, MH OR is calculated.
#' @param exp_value value for exposure as reference
#' @param case_value value for outcome as reference
#' @param digits specify rounding of numbers. See \code{\link{round}}.
#'
#'
#' @details
#'
#' Rows and Columns can be rearranged by specifying
#' `exp_value` and `case_value`. This is used
#' when the exposed and case values are not at the right place in 2x2 tables.
#'
#' Reference row value can be specified in `exp_value`.
#'
#'
#' Attributable fractions, \code{Attr. Frac. Exp} and  \code{Attr. Frac. Pop}
#' among exposed and population are calculated when RR is greated than or
#' equal to 1.
#' If RR is less than 1, preventable fractions,  \code{Prev. Frac. Exp}
#' and  \code{Attr. Frac. Pop} are calculated.
#'
#' It produces a table with Risk Ratio, 95% CI as well as
#' p-value. If \code{strata} is specified, `Mantel-Haenzsel` Pooled
#' estimates of `Risk Ratio` is generated along with Chi-squared test for
#' homogeneity.
#'
#' @references
#'
#'
#' \enumerate{
#'     \item  Betty R. Kirkwood, Jonathan A.C. Sterne (2006, ISBN:978–0–86542–871–3)
#'     \item B. Burt Gerstman (2013, ISBN:978-1-4443-3608-5)
#'     \item Douglas G Altman (2005, ISBN:0 7279 1375 1)
#' }
#'
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#'
#' ### Example from Essential Medical Statistics
#' # Page 178, Chapter 18: Controlling for confounding: Stratification
#' lepto <- expandtbl(
#'     male = c(36, 14, 50, 50), female = c(24, 126, 10, 90),
#'     exp_name = "area", exp_lvl = c("Rural", "Urban"),
#'     case_name = "ab", case_lvl = c("Yes", "No"),
#'     strata_name = "gender"
#' )
#'
#' ## label variables and data
#' lepto <- label(lepto, "Prevalence survey of leptospirosis in West Indies")
#' lepto <- label(lepto, area="Type of area", ab = "Leptospirosis Antibodies",
#'                   gender="Gener: Male or Female")
#'
#' ## Calculate RR
#' mhrr(lepto, area, ab)
#'
#' ## Calculate MHRR
#' mhrr(lepto, area, ab, gender)
#'
#'
#' \dontrun{
#' ### Demonstration: Calculating Risk Ratios
#'
#' ## Essential Medical Statistics, Betty R. Kirkwood, Second Edition
#' ## Chapter 16, Table 16.4, Page 154
#' ## For Risk Ratio
#' lung <- expandtbl(
#'     c(39, 29961, 6, 59994),
#'     exp_name = "smoking",
#'     exp_lvl = c("Smokers", "Non-smokers"),
#'     case_name = "cancer",
#'     case_lvl = c("Yes", "No")
#' )
#'
#' ## label variable and dataset
#' lung <- labelVar(lung, smoking="Yes or No", cancer="Yes or no")
#' lung <- labelData(lung, "Follow up lung cancer study")
#'
#' ## check dataset
#' codebook(lung)
#'
#' ## calculate RR
#' mhrr(lung, smoking, cancer, exp_value = "Smokers", case_value = "Yes")
#'
#'
#'
#' ## Simpson's paradox
#' ## Burt Gerstman's Epidemiology, page 326, table 14.1
#' simpson <- expandtbl("1" = c(1000, 9000, 50, 950),
#'                         "2" = c(95, 5, 5000, 5000),
#'                         exp_name = "trt",
#'                         exp_lvl = c("new", "standard"),
#'                         case_name = "case",
#'                         case_lvl = c("alive", "dead"),
#'                         strata_name = "clinic")
#'
#' ## calculate RR
#' mhrr(simpson, trt, case, exp_value = "new", case_value = "alive")
#'
#' ## calculate MH RR
#' mhrr(simpson, trt, case, clinic)
#'
#' }
#'
#' @export
mhrr <- function(data, exp , case, strata = NULL, exp_value = NULL,
                 case_value = NULL, digits = 4)
{
    ## match call arguments
    .args <- as.list(match.call())
    ## get names of dataset and headings
    .data_name <- deparse(substitute(data))
    .vars_names <- names(data)

    ## if input is not a data.frame, stop
    if (!is.data.frame(data)) {
        stop(paste0("`", .data_name, "` must be a data.frame"),
             call. = FALSE)
    }

    ## get exp and case name
    exp <- as.character(.args$exp)
    case <- as.character(.args$case)
    strata <- as.character(.args$strata)
    ## check if these variables are in the dataset
    lapply(c(exp, case, strata) , function(z) {
        if (!(z %in% .vars_names)) {
            stop(paste0("'", z, "' not found."), call. = FALSE)
        }
    })

    ## create single vectors for exp and case
    .exp <- data[[exp]]
    .case <- data[[case]]
    ## create table
    .tbl <- table(.exp, .case, useNA = "no")
    ## case var must be binary.
    if (ncol(.tbl) > 2) {
        stop(paste0("'", case, "' must be binary."), call. = FALSE)
    }
    ## if strata is not specified, calculate OR. Otherwise, calculate MHOR
    if (length(strata) == 0) {
        ## change row and col orders
        .tbl <- rowColOrder(.tbl, exp_value, case_value)
        ## split tables if nrow > 2
        .tbl <- splitTables(.tbl, .exp.value = exp_value)
        .df <- lapply(.tbl, calcRR, exp, digits)
        .df <- do.call(rbind,
                       lapply(1:length(.df), function(z) {
                           if (z > 1) {
                               .df[[z]][-1, ]
                           } else {
                               .df[[z]]
                           }
                       })
        )

        ## create label
        .txt <- paste0(" Measure of Association : Risk Ratio of '", case, "'")
    } else {
        ## exp var must be binary for MHOR.
        if (nrow(.tbl) > 2) {
            stop(paste0("'", exp, "' must be binary."), call. = FALSE)
        }
        ## create single vector for strata
        .strata <- data[[strata]]
        .df <- calcMHRR(.exp, .case, .strata, exp_value, case_value, digits)
        .t <- do.call(rbind,
                      lapply(split(data, .strata), function(z) {
                          x <- z[[exp]]
                          y <- z[[case]]
                          calcRR(table(x, y, useNA = "no"), exp, digits)[9, -c(1, 3, 8)]
                      }))
        .t[, 1] <- row.names(.t)
        .oa <- calcRR(table(.exp, .case, useNA = "no"), exp, digits)[9, -c(1, 3, 8)]
        .oa[, 1] <- "Crude"
        names(.t) <- names(.oa) <- names(.df)

        .df <- rbind(.t, .oa, .df)
        .df <- formatdf(.df, 2, 2)
        .nrow <- nrow(.df)
        .df <- rbind(.df[1:(.nrow - 4),], .df[2, ], .df[(.nrow - 3):(.nrow - 2), ],
                     .df[2, ], .df[.nrow - 1, ], .df[2, ])

        ## create label
        .txt <- paste0("\t\t\t Mantel-Haenszel (M-H) Risk Ratios")
    }

    ## Print tabulation and labels
    # printDFlenLines(.t)
    cat(paste0("       ", .txt, "\n"))
    # printDFlenLines(.t)
    print.data.frame(.df, row.names = FALSE, max = 1e9)

    ## get vars lbl
    sapply(c(exp, case, strata), getLabel, data)

    invisible(.df)
}

calcMHRR <- function(.exp, .case, .strata, exp_value, case_value, rnd)
{

    ## make three tables
    .tbl <- table(.exp, .case, .strata, useNA = "no")
    .lvl_len <- dim(.tbl)[3]

    ## calculate MHRR and homogeneity test
    .t <- do.call(
        rbind,
        lapply(1:.lvl_len, function(z) {
            ## change row and col orders
            .t <- rowColOrder(.tbl[,,z], exp_value, case_value)
            .strata_name <- dimnames(.tbl)$.strata[z]
            a <- as.numeric(.t[1, 1])
            b <- as.numeric(.t[1, 2])
            c <- as.numeric(.t[2, 1])
            d <- as.numeric(.t[2, 2])

            n <- sum(.t)
            n1 <- a + b
            n0 <- c + d
            m1 <- a + c
            m0 <- b + d

            ## MHRR ==> (Rothman 2002 p 148 and 152, equation 8-2):
            q <- a * n0 / n
            r <- c * n1 / n

            ## MHRR confidence interval
            u <- ((m1 * n1 * n0) / n^2) - ((a * c) / n)
            v <- (a * n0) / n
            x <- (c * n1) / n

            ##
            ## Woolf test of homogeneity of risk ratios (Jewell 2004, page 154).
            # First work out the Woolf estimate of the adjusted risk ratio
            ## based on Jewell (2004, page 134):
            rr <- log((a / (a + b)) / (c / (c + d)))
            var <- (b / (a * (a + b))) + (d / (c * (c + d)))
            w <- 1 / var
            wrr <- w * rr

            ## calculate stratum-specific RR
            ## cacluate OR
            res <- calcRR(.t, "tocombine", rnd)[7, -2]
            res[1, 1] <- .strata_name

            ## return result
            .df <- data.frame(res, q, r, u, v, x, rr, w, wrr)
            names(.df)[1:5] <- c("Level", "Estimate",
                                 "[95% Conf.", "Interval]", "Pr>chi2")
            .df
        })
    )

    .mhrr <- sum(.t$q) / sum(.t$r)
    .mhrr.se <- sqrt(sum(.t$u) / (sum(.t$v) * sum(.t$x)))
    .mhrr.confint <- exp(c(log(.mhrr) - (1.96 * .mhrr.se),
                           log(.mhrr) + (1.96 * .mhrr.se)))

    ## calculate chisquare test for MHRR
    .mhrr.p <- tryCatch({
        suppressWarnings(mantelhaen.test(.tbl, correct = FALSE)$p.value)
    }, error = function(err) {
        return(NA)
    })

    .mhrr <- c("M-H Combined",
               sprintf(c(.mhrr, .mhrr.confint, .mhrr.p),
                       fmt = paste0("%#.", rnd, "f")))


    ## test of homogeneity
    lnRR <- sum(.t$wrr) / sum(.t$w)
    # Equation 10.3 from Jewell (2004):
    .chi <- sum(.t$w * (.t$rr - lnRR)^2)
    .pvalue <- tryCatch({
        suppressWarnings(pchisq(.chi, df = .lvl_len - 1, lower.tail = FALSE))
    }, error = function(err) {
        return(NA)
    })

    ## combine estimates
    .t <- .t[, 1:5]
    .df <- data.frame(
        rbind(
            .mhrr,
            c("M-H Homogeneity Test", paste0("chi(", .lvl_len - 1, ")"),
              sprintf(.chi, fmt = paste0('%#.', 2, 'f')), "Pr>chi2",
              sprintf(.pvalue, fmt = paste0('%#.', rnd, 'f')))
        )
    )
    names(.df) <- names(.t)
    return(.df)
}


calcRR <- function(.tbl, exp, rnd)
{
    ## create row and column totals and odds.
    .tblr <- cbind(.tbl, Total = rowSums(.tbl))
    .tblc <- rbind(.tblr, Total = colSums(.tblr))
    .tblf <- cbind(
        .tblc,
        Risks = sprintf(.tblc[, 1] / .tblc[, 3], fmt = paste0("%#.", rnd, "f"))
    )

    ## calculate odds ratio, 95% CI, and p-value
    .a <- as.numeric(.tbl[1, 1])
    .b <- as.numeric(.tbl[1, 2])
    .c <- as.numeric(.tbl[2, 1])
    .d <- as.numeric(.tbl[2, 2])

    .n <- sum(.tbl)


    ## calculate OR
    .or <- (.a * .d) / (.b * .c)
    .fisher <- tryCatch({
        suppressWarnings(fisher.test(.tbl))
    }, error = function(cnd) {
        return(NA)
    })
    .confint <- .fisher$conf.int
    ## get p-value chi-square
    .chi <- tryCatch({
        suppressWarnings(chisq.test(.tbl, correct = FALSE))
    }, error = function(cnd) {
        return(NA)
    })
    .or <- c("Odds Ratio", sprintf(c(.or, .confint, .chi$p.value),
                                   fmt = paste0("%#.", rnd, "f")))

    ## calculate Risk Ratio
    .n1 <- .a + .b
    .n0 <- .c + .d
    .p1 <- .a / .n1
    .p0 <- .c / .n0
    .rr <- .p1 / .p0
    .rr.se <- sqrt((1 / .a) - (1 / .n1) + (1 / .c) - (1 / .n0))
    .rr.confint <- exp(c(log(.rr) - (1.96 * .rr.se),
                         log(.rr) + (1.96 * .rr.se)))
    .rr_ <- c("Risk Ratio", sprintf(c(.rr, .rr.confint, .chi$p.value),
                                    fmt = paste0("%#.", rnd, "f")))


    ## calculate Risk difference
    .rd <- .p1 - .p0
    .rd.se <- sqrt((.p1 * (1 - .p1) / .n1) +  (.p0 * (1 - .p0) / .n0))
    .rd.confint <- c(.rd - (1.96 * .rd.se), .rd + (1.96 * .rd.se))
    .p <- (.a + .c) / .n
    .rd.z <- .rd / sqrt(.p * (1 - .p) * (1/.n1 + 1/.n0))
    .rd.p <- pnorm(.rd.z, lower.tail = FALSE)
    .rd <- c("Risk Difference", sprintf(c(.rd, .rd.confint, .rd.p),
                                        fmt = paste0("%#.", rnd, "f")))



    ## calculate attributable or prevented fraction
    ## STATA 14 MANUAL, page 554
    ## Unstratified cumulative incidence data (cs and csi)
    if (.rr >= 1) {
        .afe <- (.rr - 1) / .rr
        .afe_confint <- (.rr.confint - 1) / .rr.confint
        .afp <- .afe * .a / .n1
        # .afp <- ((.a + .c) / .n) - (.c / .n0)

        .afe <- c("Attr. Frac. Exp",
                  sprintf(c(.afe, .afe_confint), fmt = paste0('%#.', rnd, 'f')),
                  "")
        .afp <- c("Attr. Frac. Pop",
                  sprintf(.afp, fmt = paste0('%#.', rnd, 'f')),
                  "", "", "")
    } else {
        .afe <- 1 - .rr
        .afe_confint <- 1 - .rr.confint
        .afp <- .afe * ( (.a + .c) / .n)
        .afe <- c("Prev. Frac. Exp",
                  sprintf(c(.afe, .afe_confint[2], .afe_confint[1]),
                          fmt = paste0('%#.', rnd, 'f')),
                  "")
        .afp <- c("Prev. Frac. Pop",
                  sprintf(.afp, fmt = paste0('%#.', rnd, 'f')),
                  "", "", "")
    }

    ## combine all in one dataframe
    .df <- data.frame(row.names(.tblf), .tblf)
    ## get other estimates
    .est <- data.frame(rbind(.rr_, .or, .rd, .afe, .afp))
    names(.est) <- names(.df) <- c(exp, colnames(.tblf))

    ## combine into final dataframe
    .df <- rbind(.df,
                 c("Measure", "Estimate", "[95% Conf.", "Interval]", "Pr>chi2"),
                 .est)
    .df <- formatdf(.df, 2, 2)
    .df <- rbind(.df[1:5, ], .df[2, ], .df[6, ], .df[2, ],
                 .df[7:11, ], .df[2, ])
    row.names(.df) <- NULL

    return(.df)
}




# INCIDENCE RATES ---------------------------------------------------------

#' @title Calculate Incidence Rates from time-to-event data
#'
#' @description
#' \code{strate()} calculates incidence rates and Corresponding 95\% CI.
#'
#' @param data Dataset
#' @param time person-time variable
#' @param var outcome variable: preferably 1 for event, 0 for censored
#' @param ... variables for stratified analysis
#' @param fail a value or values to specify failure event
#' @param per units to be used in reported rates
#' @param digits Rounding of numbers
#'
#' @details
#' Rates of event occurrences, known as incidence rates are outcome measures in
#' longitudinal studies. In most longitudinal studies, follow-up times vary due
#' to logistic reasons, different periods of recruitment, delay enrollment into
#' the study, lost-to-follow-up, immigration or emigration and death.
#'
#' \strong{Follow-up time in longitudinal studies}
#'
#' Period of observation (called as follow-up time) starts when individuals join
#' the study and ends when they either have an outcome of interest, are lost-to-
#' follow-up or the follow-up period ends, whichever happens first. This period is
#' called \strong{person-year-at-risk}. This is denoted by \emph{PY} in \code{strate}
#' function's output and number of event by \emph{D}.
#'
#' \strong{Rate}
#'
#' is calculated using the following formula:
#' \deqn{\lambda = D / PY}
#'
#' \strong{Confidence interval of rate}
#'
#' is derived using the following formula:
#'
#' \deqn{95\% CI (rate) = rate x Error Factor}
#' \deqn{Error Factor (rate) = exp(1.96 / \sqrt{D})}
#'
#'
#' \code{plot}, if \code{TRUE}, produces a graph of the rates against
#' the numerical code used for categories of \code{by}.
#'
#'
#' @references
#'
#' Betty R. Kirkwood, Jonathan A.C. Sterne (2006, ISBN:978–0–86542–871–3)
#'
#' @import stats
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' \dontrun{
#'
#' ## Using the diet data (Clayton and Hills 1993) described in STATA manual
#' import diet data: require haven package to read dta format.
#' magrittr package for piping operation
#' diet <- haven::read_dta("https://www.stata-press.com/data/r16/diet.dta")
#'
#' diet <- generate(diet, time, (dox - doe) / 365.25)
#' diet <- replace(diet, time, as.numeric(time))
#' diet <- generate(diet, age, as.numeric(doe - dob) / 365.25)
#' diet <- egen(diet, age, c(41, 51, 61, 71), new_var = ageband)
#' diet <- egen(diet, month, c(3, 6, 8), new_var = monthgrp)
#'
#' ## calculate overall rates and 95% Confidence intervals
#' strate(diet, time, fail, fail = c(1, 3, 13))
#'
#' ## per 100 unit
#' strate(diet, time, fail, fail = c(1, 3, 13), per = 100)
#'
#' ## calculate Stratified rates and 95% Confidence Intervals
#' strate(diet, time, fail, job, fail = c(1, 3, 13))
#' strate(diet, time, fail, job, ageband, monthgrp, fail = c(1, 3, 13))
#'
#' ## per 100 unit
#' strate(diet, time, fail, job, ageband, monthgrp, fail = c(1, 3, 13), per = 100)
#' }
#'
#' @export
strate <- function(data, time, var, ... , fail = NULL, per = 1, digits = 5)
{
    ## match call arguments
    .args <- as.list(match.call())
    ## get names of dataset and headings
    .data_name <- deparse(substitute(data))
    .vars_name <- names(data)
    time <- .args$time
    var <- .args$var

    ## if input is not a data.frame, stop
    if (!is.data.frame(data)) {
        stop(paste0("`", .data_name, "` must be a data.frame"),
             call. = FALSE)
    }

    ## get variable names within three dots to search for duplicates
    .vars <- enquotes(.args, c("data", "time", "var", "fail",
                               "per", "digits"))
    ## check colon, and check data types if the whole dataset
    .vars <- checkEnquotes(data, .vars)
    ## check variables in the dataset
    sapply(c(time, var, .vars), function(z) {
        if (!any(as.character(z) %in% .vars_name)) {
            stop(paste0("`", z, "` not found."),
                 call. = FALSE)
        }
    })

    ## get individual data
    .time <- data[[time]]
    .var <- data[[var]]
    ## check failure value
    .fail_lvl <- unique(.var)
    ## if failure value is not specified, the first value is used
    if (is.null(fail)) {
        fail <- .fail_lvl[1]
    }
    ## check failure values are correct
    sapply(fail, function(z) {
        if (!any(z %in% .fail_lvl)) {
            stop(paste0("Failure value `", z, "` not found"),
                 call. = FALSE)
        }
    })

    ## if no variable, then overall strate is calculated
    if (length(.vars) == 0) {
        .t <- calcRate(.time, .var, fail, per, digits)
        .df <- cbind(var = as.character(var), .t)
        ## change headings and add dash lines
        names(.df) <- c("Variable", "Failure", "Person-Years", "Inc. Rate",
                        "[95% Conf.", "Interval]")
        .df <- formatdf(.df, 2, 2)
    } else {
        ## calculate stratified rates
        .df <- do.call(rbind,
                       lapply(.vars, function(z) {
                           .by <- data[[z]]
                           .t <- calcSRate(.time, .var, .by, fail, per, digits)
                           cbind(var = c(z, rep("", nrow(.t) - 1)), .t)
                       }))

        ## change headings and add dash lines
        names(.df) <- c("Variable", "Category", "Failure",
                        "Person-Years", "Inc. Rate",
                        "[95% Conf.", "Interval]")
        .df_raw <- .df
        ## Add horizontal and vertical lines for visual appealing
        hpos <- as.numeric(row.names(.df)[!(.df$Variable == "")])[-1]
        hpos <- hpos + 0:(length(hpos) - 1)
        sapply(hpos, function(z) {
            .df <<- addHLines(.df, z)
        })
        .df <- formatdf(.df, 2, 3)
    }


    ## Display information
    cat(paste0("  Estimated Incidence Rates",
               " and 95% Confidence Intervals\n"))
    cat(paste0("  (", length(.var), " records included in the analysis)\n"))
    print.data.frame(.df, row.names = FALSE, max = 1e9)
    sapply(c(var, time, .vars), getLabel, data)

    invisible(.df)
}

calcSRate <- function(time, var, by, fail, per, rnd)
{
    ## get levels of var, if NA, remove
    .lvl <- unique(by)
    .lvl <- .lvl[!is.na(.lvl)]

    ## calculate rates and statistics
    .df <- do.call(
        rbind,
        lapply(.lvl, function(z) {
            time <- time[by == z]
            var <- var[by == z]
            calcRate(time, var, fail, per, rnd)
        })
    )

    ## add levels to df
    .df <- cbind(lvl = .lvl, .df)
    .df <- data.frame(.df)

    return(.df)
}
calcRate <- function(time, var, fail, per, rnd)
{
    ## calculate statistics
    .d <- sum(var %in% fail, na.rm = TRUE)
    .py <- sum(time, na.rm = TRUE)
    .ir <- .d / .py
    .ef <- exp(1.96 * (1/ sqrt(.d)))
    .ir.confint <- c(.ir / .ef, .ir * .ef)
    .df <- sprintf(c(.py / per, c(.ir, .ir.confint) * per),
                   fmt = paste0("%#.", rnd, "f" ))

    .df <- data.frame(rbind(c(.d, .df)))
    names(.df) <- c("D", "Y", "R", "LL", "UL")

    return(.df)
}





# GRAPHS ------------------------------------------------------------------

#' @title Histograms with overlay normal curve
#'
#' @description
#'
#' \code{histogram()} draws a histogram with formatted texts and
#' adds a normal curve over the histogram.
#'
#' @param data Dataset
#' @param var variable
#' @param breaks \link[graphics]{hist}
#' @param xlab \link[graphics]{hist}
#' @param main \link[graphics]{hist}
#' @param sub \link[graphics]{hist}
#' @param labels \link[graphics]{hist}
#' @param freq \link[graphics]{hist}
#' @param curve logical. If `TRUE` (default), a normal curve is
#' overlaid over the histogram.
#' @param ... \link[graphics]{hist}
#'
#' @details
#'
#' If `freq` is set to `FALSE`, probability densities,
#' component density, are plotted (so that the histogram has
#' a total area of one). In this case, normal curve will not be
#' generated.
#'
#' @importFrom graphics hist lines
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' # histogram(infert, age)
#' # histogram(infert, age, labels = FALSE)
#' # histogram(infert, age, freq = FALSE)
#'
#' @export
histogram <- function(data, var, breaks = NULL, xlab = NULL, main = NULL,
                      sub = NULL, labels = TRUE, freq = TRUE, curve = TRUE,
                      ...)
{
    ## match call arguments
    .args <- as.list(match.call())

    ## get names of dataset and headings
    .data_name <- deparse(substitute(data))
    .vars_names <- names(data)
    .var_name <- as.character(.args$var)

    ## if input is not a data.frame, stop
    if (!is.data.frame(data)) {
        stop(paste0("`", .data_name, "` must be a data.frame"),
             call. = FALSE)
    }

    ## if var is not specified, stop
    if (length(.var_name) == 0) {
        stop("Specify a variable", call. = TRUE)
    }

    ## get data into var
    var <- eval(substitute(var), data)


    ## run histogram
    .hist <- hist(var, plot = FALSE)

    ## get individual data pieces to patch up a histogram
    .breaks <- .hist$breaks
    if (!is.null(breaks)) {
        .breaks <- breaks
    }
    .counts <- .hist$counts
    .density <- .hist$density

    ## get p value from normality test
    pvalue <- tryCatch({
        suppressWarnings(shapiro.test(var)$p.value)
    }, error = function(err) {
        return(NA)
    })
    pvalue <- sprintf(pvalue, fmt = '%#.3f')


    ## get labels and other arugment inputs.
    xlab <- ifelse(is.null(xlab), .var_name, xlab)
    main <- ifelse(is.null(main),
                   paste0("Distribution of '", .var_name, "'"),
                   main)
    sub <- ifelse(is.null(sub),
                  paste0("\nShapiro-Wilk Normality Test: ", pvalue),
                  sub)

    if (freq) {
        ylim <- c(0, max(.counts, na.rm = TRUE) +
                      mean(.counts, na.rm = TRUE))
    } else {
        ylim <- c(0, max(.density, na.rm = TRUE) +
                      mean(.density, na.rm = TRUE))
    }

    tryCatch({
        ## draw histogram again
        .df <- hist(var,
                    breaks = .breaks,
                    main = main,
                    sub = sub,
                    xlab = xlab,
                    ylim = ylim,
                    labels = labels,
                    freq = freq,
                    ...)

        ## add overlay normal curve
        if (curve) {
            ## add overlay normal curve
            xfit <- seq(min(var, na.rm = TRUE), max(var, na.rm = TRUE),
                        length = length(var))
            yfit <- dnorm(xfit, mean = mean(var, na.rm = TRUE),
                          sd = sd(var, na.rm = TRUE))
            yfit <- yfit * diff(.hist$mids[1:2]) * length(var)

            lines(xfit, yfit, col = "blue", lwd = 1)
        }

        ## print log
        cat(paste0("  (A histogram is drawn for '", .var_name, "'",
                   ifelse(curve, " with overlay normal curve", ""),
                   ")\n"))
    }, error = function(cnd) {
        stop(cnd, call. = FALSE)
    })

    invisible(.df)
}



#' @title Scatter plot matrices with histogram and
#' correlation coefficients
#'
#' @description
#'
#' A matrix of scatter plots is produced with
#' Scatter plots with smooth regression line in lower panel,
#' histograms in diagonal panel and Pearson's correlation
#' coefficients in upper panel.
#'
#' @param data data.frame.
#' @inheritParams graphics::title
#' @param pch numeric: point symbol
#' @param ... further arguments to be passed to or from methods
#'
#' @importFrom graphics pairs panel.smooth par rect text
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' ## iris data
#' # scatterPlotMatrix(iris)
#'
#' @export
scatterPlotMatrix <- function(data, main = NULL, pch = 21, ... )
{
    ## match call arguments
    .args <- as.list(match.call())

    ## get and reset graph parameter on exit
    usr <- par("usr")
    on.exit(par(usr))

    ## labels
    if (is.null(main)) {
        main <- paste0("Scatter plot matrix of '",
                       .args$data, "'")
    }
    ## histogram
    panel.hist <- function(x, ...)
    {
        usr <- par("usr")
        on.exit(par(usr))
        par(usr = c(usr[1:2], 0, 1.5) )
        h <- hist(x, plot = FALSE)
        breaks <- h$breaks; nB <- length(breaks)
        y <- h$counts; y <- y/max(y)
        rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
    }
    # Correlation panel
    panel.cor <- function(x, y, ... )
    {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- cor(x, y, use = "complete.obs")
        txt <- paste0("R = ",
                      sprintf(r, fmt = paste0("%#.", 5, "f")))
        text(0.5, 0.5, txt)
    }
    pairs(data, pch = pch,
          main = main,
          panel = panel.smooth,
          upper.panel = panel.cor,
          diag.panel = panel.hist,
          ... )

}





# REPORTING ---------------------------------------------------------------

##' @title Table Format for Publication
##'
##' @name summary
##' @rdname summary
##'
##' @description
##' \code{summary()} organizes the output and print
##' a favorable format
##' to the console, which is used with rmarkdown package
##' to produce publication-ready tables.
##'
##'
##' @param object an object for which a summary is desired.
##' @param ... additional arguments affecting the summary produced.
##'
##' @author
##'
##' Email: \email{dr.myominnoo@@gmail.com}
##'
##' Website: \url{https://myominnoo.github.io/}
##'
##' @examples
##'
##' \dontrun{
##' ## Summary for tabulation
##' x <- tab(infert, education, parity:spontaneous)
##' summary(x)
##'
##' x <- tab(infert, education, parity:spontaneous, by = case)
##' summary(x)
##'
##' ## Summary for summary statistics
##' x <- summ(iris)
##' summary(x)
##'
##' x <- summ(iris, by = Species)
##' summary(x)
##'
##' x <- summ(iris, by = Species, detail = TRUE)
##' summary(x)
##' }
##'
##'
NULL


##' @rdname summary
##'
##' @export
summary.tab <- function(object, ... )
{
    x <- object$tab_raw
    if (object$type == "tab1") {
        x$Freq. <- paste0(x$Freq., " (", x$Percent, ")")
        names(x)[3] <- "n (%)"
        x <- x[, 1:3]

        ## adding labels
        lbl <- object$lbl
        lbl$lbl[is.na(lbl$lbl)] <- lbl$var[is.na(lbl$lbl)]
        x[x$Variable != "", "Variable"] <- lbl$lbl
    } else if (object$type == "tab2p") {
        y <- x[, 3:(ncol(x) - 2)]
        r <- seq(1, ncol(y), 2)
        p <- seq(2, ncol(y), 2)
        z <- do.call(
            data.frame,
            lapply(1:length(r), function(z) {
                paste0(y[, r[z]], " (", y[, p[z]], ")")
            })
        )
        names(z) <- paste0(names(x[, 3:(ncol(x) - 2)])[r], " (%)")
        x <- cbind(x[, 1:2], z, x[, (ncol(x) - 1):ncol(x)])
    }

    return(x)
}

##' @rdname summary
##'
##' @export
summary.summ <- function(object, ... )
{
    x <- object$summ_raw
    if (object$type == "summ1") {
        x <- data.frame(
            x$Variable,
            paste0(x$Obs, " (", x$`<NA>`, ")"),
            paste0(x$Mean, " (", x$Std.Dev, ")"),
            paste0(x$Median, " (", x$`25%`, "-", x$`75%`, ")"),
            paste0(x$Min, "-", x$Max),
            x$Normal.
        )
        names(x) <- c("Variable", "Obs (<NA>)", "Mean (SD)",
                      "Median (IQR)", "Range", "Normal.")
    } else if (object$type == "summ2") {
        y <- data.frame(
            x$Variable, x$Level,
            paste0(x$Obs, " (", x$`<NA>`, ")"),
            paste0(x$Mean, " (", x$Std.Dev, ")"),
            x$Normal., x[, c(ncol(x) - 1, ncol(x))]
        )
        names(y) <- c("Variable", "Level", "Obs (<NA>)", "Mean (SD)",
                      "Normal.")
        names(y)[c(ncol(y) - 1, ncol(y))] <- names(x)[c(ncol(x) - 1, ncol(x))]
        x <- y
    } else {
        y <- data.frame(
            x$Variable, x$Level,
            paste0(x$Obs, " (", x$`<NA>`, ")"),
            paste0(x$Mean, " (", x$Std.Dev, ")"),
            paste0(x$Median, " (", x$`25%`, "-", x$`75%`, ")"),
            paste0(x$Min, "-", x$Max),
            x$Normal., x[, c(ncol(x) - 1, ncol(x))]
        )
        names(y) <- c("Variable", "Level", "Obs (<NA>)", "Mean (SD)",
                      "Median (IQR)", "Range", "Normal.")
        names(y)[c(ncol(y) - 1, ncol(y))] <- names(x)[c(ncol(x) - 1, ncol(x))]
        x <- y
    }
    return(x)
}



# HELPERS -----------------------------------------------------------------


#' @title Helper functions
#'
#' @description
#'
#' These are helper functions for `mStats`.
#'
#' @param ... further arguments to be passed to or from methods
#'
#' @export
helpers <- function(...) {}




#' @rdname helpers
#' @importFrom grDevices dev.list dev.off
#' @export
clear <- function() {
    while (!is.null(dev.list()))  dev.off()
    rm(list = ls(envir = .GlobalEnv), pos = 1)
    cat("\014")
}



#' @title Create a copy of your output in a text format
#'
#' @description
#' \code{ilog()} creates a text copy of your output.
#' \code{ilog.close()} closes the `ilog()` function.
#' \code{ilog.clear()} clears for the prompt error caused
#' when the environment is removed.
#'
#' @param logfile Name of desired log file in \code{.txt} format
#' @param append logical value
#'
#' @details
#' \code{ilog} is a two-step function that allows you a record of your console.
#' A log is a file containing what you type and console output. If a name is not
#' specified, then \code{ilog} will use the name \code{<unnamed>.txt}.
#'
#' \code{ilog} opens a log file and \code{ilog.close} close the file.
#'
#' \strong{Warnings}:
#'
#' However, clearing objects from the workspace along with hidden objects
#' removes \code{ilog}'s \code{.logenv} environment, hence throwing an error
#' when it's attempted to be closed. An error message
#' \code{Error in (function (cmd, res, s, vis)  : object '.logenv' not found}
#' will be thrown.
#'
#' In that case, console prompt is stuck at \code{log> }. If
#' this error occurs, use \code{ilog.clear()} function to revert back to
#' normal.
#'
#'
#' @author
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#'
#' \dontrun{
#' ## my first log
#' ilog("../myFirstLog.tx")
#' str(infert)
#' str(iris)
#' ilog.close()
#'
#' ## in case of error: ".logenv" not found
#' # ilog.clear()
#' }
#' @export
ilog <- function(logfile = "log.txt", append = FALSE) {
    # create a global environment which can be accessed outside
    .logenv <<- new.env()
    # <-- create a connection file -->
    if(!append) {
        # <--- if logfile exists & replace is TRUE, remove logfile --->
        if(file.exists(logfile)) {
            unlink(logfile)
        }
    }
    # write log file
    con <- file(logfile, open ='a')
    .logenv$logfile <- logfile

    if(isOpen(con)) {
        .logenv$con.close <- FALSE
    } else {
        .logenv$con.close <- TRUE
        if(append) {
            open(con, open='a')
        } else {
            open(con, open='w')
        }
    }

    .logenv$con <- con
    .logenv$cmd <- TRUE
    .logenv$res <- TRUE
    .logenv$first <- TRUE

    .logenv$con.out <- textConnection(NULL, open='a')
    sink(.logenv$con.out, split=TRUE)

    .logenv$prompt <- unlist(options('prompt'))
    .logenv$continue <- unlist(options('continue'))

    options(prompt = paste('log', .logenv$prompt, sep = ''),
            continue = paste('log', .logenv$continue,sep = '') )

    # writing log info
    sink(logfile, append = TRUE, split = TRUE)
    cat(paste0('\n      log: ', getwd(), "/", logfile,
               '\n  open on: ', Sys.time(),'\n'))
    if(append) {
        cat(paste0("     note: ", "appended\n"))
    } else {cat(paste0("     note: ", "replaced\n\n"))}
    sink()

    addTaskCallback(ilogtxt, name = "ilogtxt")
    invisible(NULL)
}

#' @rdname  ilog
#' @export
ilog.close <- function() {
    removeTaskCallback(id = "ilogtxt")
    .logenv <- as.environment(.logenv)
    if(!.logenv$con.close) {
        close(.logenv$con)
    }
    options( prompt = .logenv$prompt,
             continue = .logenv$continue )
    if(.logenv$res) {
        sink()
        close(.logenv$con.out)
        # closeAllConnections()
    }
    # writing log info
    sink(.logenv$logfile, append = TRUE, split = TRUE)
    cat(paste0('\n      log: ', getwd(), "/", .logenv$logfile,
               '\nclosed on: ',
               Sys.time(),'\n\n'))
    sink()
    eval(rm(list= ".logenv", envir = sys.frame(-1)))
    invisible(NULL)
}

ilogtxt <- function(cmd, res, s, vis) {
    if(.logenv$first) {
        .logenv$first <- FALSE
        if( .logenv$res ) {
            sink()
            close(.logenv$con.out)
            .logenv$con.out <- textConnection(NULL, open='a')
            sink(.logenv$con.out, split=TRUE)
        }
    } else {
        if(.logenv$cmd){
            cmdline <- deparse(cmd)
            cmdline <- gsub('    ', paste("\n", .logenv$continue, sep =''),
                            cmdline)
            cmdline <- gsub('}', paste("\n", .logenv$continue,"}", sep =''),
                            cmdline)
            cat(.logenv$prompt, cmdline, "\n", sep = '',
                file=.logenv$con)
        }
        if(.logenv$res) {
            tmp <- textConnectionValue(.logenv$con.out)
            if(length(tmp)) {
                cat(tmp, sep='\n', file = .logenv$con)
                sink()
                close(.logenv$con.out)
                .logenv$con.out <- textConnection(NULL, open='a')
                sink(.logenv$con.out, split=TRUE)
            }
        }
    }
    TRUE
}


#' @rdname  ilog
#' @export
ilog.clear <- function()
{
    options(prompt = "> ", continue = "+ ")
}






## Print lines based on the width of the dataset returned.
printDFlenLines <- function(data)
{
    .cols_width <- sum(sapply(names(data), nchar))
    .cols_lines <- paste0(rep("-", .cols_width + ncol(data)), collapse = "")
    cat(.cols_lines, "\n")
}



#### Add vertical lines with a plus + symbol at intersection
# with horizontal lines
addVlines <- function(data, position)
{
    .data <- data[position:ncol(data)]
    .vlines <- rep("|", nrow(data))
    data <- cbind(.vlines, data[, 1:(position - 1)], .vlines, .data, .vlines)
    data[2, c(1, position + 1, ncol(data))] <- "+"
    data[nrow(data), ] <- data[2, ]
    names(data) <- data[2, ]
    row.names(data) <- NULL
    data
}

#### Add horizontal lines
addHLines <- function(data, position)
{
    .chr_max_vars <- sapply(names(data), function(z) {
        if (is.na(z)) z <- "<NA>"
        max(nchar(z), na.rm = TRUE)
    })
    .chr_max_obs <- sapply(data, function(z)
        max(ifelse(is.na(nchar(z)), 0, nchar(z)), na.rm = TRUE))
    .cols_len <- sapply(data.frame(rbind(.chr_max_vars, .chr_max_obs)), max)
    .hlines <- lapply(.cols_len, function(z)
        paste0(rep("-", z), collapse = ""))
    .data <- data[position:nrow(data), ]
    data <- data[1:position, ]
    data[position, ] <- .hlines
    data[(position + 1):(position + nrow(.data)), ] <- .data
    row.names(data) <- NULL
    data
}

#### Move variable names to the first row
moveVars <- function(data)
{
    .data <- data
    data <- data[0, ]
    data[1, ] <- names(data)
    rbind(data, .data)
}

#### Format dataset returned by adding horizontal, vertical lines
# as well as moving variable names
# used addHlines, addVlines and moveVars
formatdf <- function(data, hpos, vpos)
{
    data <- moveVars(data)
    data <- addHLines(data, position = hpos)
    data[nrow(data) + 1, ] <- data[hpos, ]
    names(data) <- data[2, ]
    addVlines(data, vpos)
}

#### Retrieve labels: can also print these labels at the same time
# best used with apply function
getLabel <- function(vars, data, print = TRUE)
{
    .lbl <- attr(data[[vars]], "label")
    if (length(.lbl) > 1) .lbl <- NULL
    .lbl <- ifelse(is.null(.lbl), NA, .lbl)
    if (print & !is.na(.lbl)) {
        cat(paste0("  (", vars, " : ", .lbl, ")\n"))
    }
    return(.lbl)
}


#### Get names within three dots ...
enquotes <- function(.args, .args_name)
{
    .args <- .args[-1]
    .args_contain <- names(.args) %in% .args_name
    if (length(.args_contain) > 0) {
        .dots <- .args[!(names(.args) %in% .args_name)]
    } else {
        .dots <- .args
    }
    return(as.character(.dots))
}


#### check and split by colon ":" in a string vector
checkEnquotes <- function(.data, .vars)
{
    .vars_name <- names(.data)
    ## Check if colon is there.
    ## if yes, retrieve variables between the two variables
    .vars <- do.call(
        c,
        lapply(.vars, function(z) {
            .colon <- grepl(":", z)
            if (.colon) {
                .split <- unlist(strsplit(z, split = ":"))
                .split <- paste0("^", .split, "$")
                .split <- grep(.split[1],
                               names(.data)):grep(.split[2],
                                                  names(.data))
                .split <- names(.data)[.split]
                unlist(.split)
            } else {
                z
            }
        })
    )
    .vars <- unique(.vars)
    return(.vars)
}

## helpers for 2x2 tables
rowColOrder <- function (.tbl, .exp.value = NULL, .case.value = NULL)
{
    .tbl.row <- rownames(.tbl)
    .tbl.col <- colnames(.tbl)
    .tbl.dim <- names(dimnames(.tbl))
    if (!is.null(.exp.value)) {
        .tbl <- rbind(.tbl[.tbl.row == .exp.value, ], .tbl[.tbl.row !=
                                                               .exp.value, ])
        row.names(.tbl) <- c(.tbl.row[.tbl.row == .exp.value],
                             .tbl.row[.tbl.row != .exp.value])
    }
    if (!is.null(.case.value)) {
        .tbl <- cbind(.tbl[, .tbl.col == .case.value], .tbl[,
                                                            .tbl.col != .case.value])
        colnames(.tbl) <- c(.tbl.col[.tbl.col == .case.value],
                            .tbl.col[.tbl.col != .case.value])
    }
    names(dimnames(.tbl)) <- .tbl.dim
    return(.tbl)
}

splitTables <- function (.tbl, .exp.value = NULL)
{
    .row.names <- rownames(.tbl)
    .exp.value <- ifelse(is.null(.exp.value), .row.names[1],
                         .exp.value)
    .tbl <- lapply(.row.names, function(z) {
        if (z != .exp.value) {
            .tbl.new <- rbind(.tbl[.exp.value, ], .tbl[z, ])
            rownames(.tbl.new) <-
                c(.row.names[.row.names == .exp.value],
                  .row.names[.row.names == z])
            .tbl.new
        }
    })
    .tbl <- .tbl[lapply(.tbl, length) > 0]
    return(.tbl)
}

