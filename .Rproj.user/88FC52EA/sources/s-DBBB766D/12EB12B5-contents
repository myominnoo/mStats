
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
        paste0("  (", sum(!.num, na.rm = TRUE) + sum(is.na(.num)),
               " values replaced)\n")
    )

    return(data)
}



#' @title Categorizing a numerical variable
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
#' THe appending datasets must have at least one variable name
#' which is there in the master dataset.
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
#' \strong{Tabulatng the whole dataset}
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
#' \strong{Tabulatng the whole dataset}
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
    # printDFlenLines(.df)
    cat(paste0(.txt, "\n"))
    # printDFlenLines(.df)
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
