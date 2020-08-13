

# Helper functions --------------------------------------------------------

#' @title Helper functions for `mStats`
#'
#' @description
#'
#' This is a collection of all helper functions for the package.
#'
#' @param ... Arguments for documentations
#'
#' @export
helpers <- function( ... ) {}



# Get R Data types  -------------------------------------------------------

#' @describeIn helpers
#'
#' This creates four character vectors of names of data types in R.
#'
#' @export
getRDataTypes <- function()
{
    .numeric <- c("integer", "double", "numeric")
    .factor <- c("factor", "orderedfactor", "character")
    .logical <- c("logical")
    .date <- c("Date")
}




# Get names within three dots  --------------------------------------------

#' @describeIn helpers
#'
#' `enquotes` retrieves names of arguments from `as.list(match.call())` and
#' returns names within three dots `...`.
#'
#' @param .args argument lists from `as.list(match.call())`
#' @param .args_name Names of arguments to be removed.
#'
#' @export
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

#' @describeIn helpers
#'
#' `checkEnquos` checks colon separators within three dots.
#' if yes, it gets ranges of corresponding variables within the dataset.
#' if not, return the same variables.
#'
#' @inheritDotParams addDashLines .data
#' @param .vars names of variables
#' @param .return used when no variables are specified within three dots:
#' If it is set to "tab", it retrieves variables for tabulation.
#' (factors, ordered factor, characters, logical)
#'
#' If to "summ", it gets the ones for summary measures.
#' ("numeric", "double", "integer", "logical")
#'
#' @export
checkEnquos <- function(.data, .vars, .return = NULL)
{
    .vars_names <- names(.data)

    ## Check if colon is there.
    ## if yes, retrieve variables between the two variables
    if (any(grepl(":", .vars))) {
        .vars <- do.call(c,
                         lapply(.vars, function(z) {
                             .colon <- grepl(":", z)
                             if (.colon) {
                                 splitByColon(.data, z, .colon)
                             } else {
                                 z
                             }
                         })
        )
    }

    if (!is.null(.return)) {
        ## if .vars is length zero, then .vars is all variables
        if (length(.vars) == 0) {
            .vars <- .vars_names
            if (.return == "tab") {
                .types <- c("factor", "character", "orderedfactor", "logical")
            } else if (.return == "summ") {
                .types <- c("numeric", "double", "integer", "logical")
            } else {
                .types <- NULL
            }

            ## get the types of variables
            .vars_types <- unlist(lapply(.data, function(z) {
                .class <- class(unlist(z))[1]
                if (.class == "haven_labelled") {
                    .class <- typeof(unlist(z))[1]
                }
                .class
            }))

            ## if all, .vars is equal to all vars
            if (.return == "all") {
                .vars <- .vars
            } else {
                ## get only those whose type are in .tab.type
                .vars <- .vars[.vars_types %in% .types]
            }
        }
    }

    return(.vars)
}


#' @describeIn helpers
#'
#' `checkEnquos` checks colon separators within three dots.
#' if yes, it gets ranges of corresponding variables within the dataset.
#' if not, return the same variables.
#'
#' @inheritDotParams addDashLines .data
#' @param .var variable name to be checked
#'
#' @export
checkVarName <- function(.data, .var)
{
    if (!(.var %in% names(.data)))
        stop(paste0("'", .var, "' not found in the dataset."), call. = FALSE)
}



#' @describeIn helpers
#' Split variable names separated by colon
#'
#' @inheritDotParams addDashLines .data
#' @param .vars Variables
#' @param .colon Logical. `TRUE` indicates containing colon.
#'
#'
#' @export
splitByColon <- function(.data, .vars, .colon)
{
    .vars <- unlist(strsplit(.vars[.colon], split = ":"))
    .vars <- paste0("^", .vars, "$")
    .vars <- names(.data)[grep(.vars[1], names(.data)):
                                    grep(.vars[2], names(.data))]
    return(.vars)
}



# Add dash lines to data.frame --------------------------------------------


#' @describeIn helpers
#'
#' This adds dash lines to the dataframe in the first and last rows.
#'
#' @param .data Dataset
#' @param .vline positional index of Vertical Line:
#' to add `|` in dataset. If `NULL`, it adds dash lines without
#' vertical lines
#'
#' @return
#'
#' modifided \code{data.frame}
#'
#' @export
addDashLines <- function(.data, .vline = NULL)
{
    ## get headers and column widths
    .var_names <- names(.data)
    .col_len <- sapply(.data, function(z) {
        max(nchar(as.character(z)), na.rm = TRUE)
    })

    ## combine widths of headers and columns
    ## get the maximum widths for each column
    .col_len <- sapply(data.frame(
        rbind(nchar(.var_names), .col_len)
    ), function(z) max(z, na.rm = TRUE))

    ## create dash lines based on column widths
    .dash <- sapply(.col_len, function(z) {
        z <- ifelse(is.na(z), 2, z)
        paste0(rep("-", z), collapse = "")
    })



    if (is.null(.vline)) {
        ## only add dash lines without vertical lines
        .df <- .data
    } else {
        ## if vertical lines < 0 or number of columns, stop
        .ncol <- ncol(.data)
        if (.ncol < .vline | .vline < 0) {
            stop(paste0("vertical line should be greater than 0 and ",
                        "less than number of columns of data.frame."),
                 call. = FALSE)
        }
        ## add vertical lines
        ## add plus sign "+" to .dash
        if (.vline == 1) {
            .df <- cbind("|", .data)
            names(.df) <- c("|", .var_names)
            .dash <- c("+", .dash)
        } else {
            .df <- cbind(.data[, 1:(.vline-1)], "|",
                         .data[, .vline:.ncol])
            names(.df) <- c(.var_names[1:(.vline-1)], "|",
                            .var_names[.vline:.ncol])
            .dash <- c(.dash[1:(.vline-1)], "+", .dash[.vline:.ncol])
        }
    }

    ## add dash lines to modified data.frame
    .df <- rbind(.df[0, ], .dash, .df[1:nrow(.data), ], .dash)

    return(.df)
}





# Printing outputs --------------------------------------------------------


#' @describeIn helpers It calculates line width for printing lines
#'
#' @inheritDotParams addDashLines .data
#'
#' @export
findLineWidth <- function(.data)
{
    ## get headers and column widths
    .var_names <- names(.data)
    .col_len <- sapply(.data, function(z) {
        max(nchar(as.character(z)), na.rm = TRUE)
    })

    ## combine widths of headers and columns
    ## get the maximum widths for each column
    .col_len <- sapply(data.frame(
        rbind(nchar(.var_names), .col_len)
    ), function(z) max(z, na.rm = TRUE))

    ## calculate line width by summing column width and
    ## number of columns
    .line_width <- sum(.col_len, na.rm = TRUE) + ncol(.data)

    return(.line_width)
}


#' @describeIn helpers It wraps texts or messages within
#' a specified length.
#'
#' @inheritDotParams printLines .width
#' @param .txt texts
#' @param .sep separator for line break
#'
#' @export
wrapText <- function(.txt, .width = 70, .sep = "\n")
{
    # check character length
    .char.len <- nchar(.txt)
    # if character length is more than console width (70)
    # then search iteration number by dividing character length and width
    if (.char.len > .width) {
        .iterate <- floor(.char.len / .width)
        for (i in 0:.iterate) {
            if (i == 0) {
                t <- substr(.txt, (i * .width) + 1, (i + 1) * .width)
            } else {
                t <- c(t, .sep,
                       substr(.txt, (i * .width) + 1, (i + 1) * .width))
            }
        }
        .txt <- paste0(t, collapse = "")
    }

    return(.txt)
}



#' @describeIn helpers It produces lines as strings for a
#' specified length
#'
#' @param .x vector, matrix, dataframe or separator (in case of printLines)
#' @param .width desired character length to display
#'
#' @export
printLines <- function(.x = "_", .width = 70)
{
    cat(paste(rep(.x, .width), collapse = ""), "\n")
}

#' @describeIn helpers It print texts within round brackets.
#'
#' @inheritDotParams wrapText .txt
#'
#' @export
printText <- function(.txt = NULL)
{
    .txt <- paste0("(", .txt, ")\n", collapse = "")
    .txt <- wrapText(.txt, 80)
    cat(.txt)
}


#' @describeIn helpers
#'
#' It retrieves and print labels.
#' If only dataset is specified, it prints dataset label.
#' Otherwise, it prints variable or variables' labels.
#'
#' @inheritDotParams addDashLines .data
#' @param .var_name character vector indicating names of variables
#'
#' @export
printLabel <- function(.data, .var_name = NULL)
{
    ## if name of variable is not specified,
    ## then print label of dataset
    if (is.null(.var_name)) {
        .var_name <- "Dataset Label"
        .lbl <- attr(.data, "label")
        if (!is.null(.lbl)) {
            printText(paste0("Dataset Label: ", .lbl))
        }
    } else if (length(.var_name) == 1) {
        ## for one variable, do this
        .lbl <- attr(.data[[.var_name]], "label")
        if (!is.null(.lbl)) {
            printText(paste0(.var_name, ": ", list(.lbl)))
        }
        paste0(list(.lbl))
    } else {
        .lbl <- sapply(.var_name, function(z) attr(.data[[z]], "label"))
        .lbl <- sapply(1:length(.var_name), function(z) {
            if (.lbl[z] != "NULL") {
                printText(paste0(.var_name[z], ": ", .lbl[z]))
            }
        })
    }
}


#' @describeIn helpers This prints `data.frame` in a well-formatted style.
#' It also adds `printLines` for better visualization.
#'
#' @inheritDotParams addDashLines .data
#' @inheritDotParams wrapText .txt
#' @param .split a text that separate between two lines
#'
#' @export
printDF <- function(.data, .txt, .split = NULL)
{
    ## find line width to produce lines
    .line_width <- findLineWidth(.data)

    ## if split is null, just wrap the heading
    ## if not, split text based on the split texts and wrap it
    if (is.null(.split)) {
        .txt <- wrapText(.txt, .line_width)
    } else {
        ## split texts
        .txt_split <- unlist(strsplit(.txt, .split))
        ## add split texts in the second vector
        .txt_split[2] <- paste0(.split, .txt_split[2], collapse = "")
        ## wrap both texts
        .txt_split <- sapply(.txt_split,
                             function(z) wrapText(z, .line_width))
        ## combine all together
        .txt <- paste0(c(.txt_split[1], .txt_split[2]), collapse = "")
    }

    ## print lines
    printLines(.x = "_", .width = .line_width)
    cat("\n")
    cat(.txt, "\n")
    printLines(.x = "_", .width = .line_width)
    cat("\n")
    print.data.frame(.data, row.names = FALSE, max = 1e9)
    printLines(.x = "_", .width = .line_width)
}
