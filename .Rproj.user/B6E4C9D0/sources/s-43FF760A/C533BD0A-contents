#' Get names within three dots in a function
#'
#' @description
#' `enquos` retrieves names of arguments from `as.list(match.call())` and
#' returns names within three dots `...`.
#'
#' @param .args argument lists from `as.list(match.call())`
#' @param .argsName Names of arguments to be removed.
#' @param .data Dataset
#' @param .vars .vars obtained with `as.character(enquos())`
#' @param .types "tab" to get categorical data; otherwise, numerical data
#'
#' @return
#' A character vector
#'
#' @import utils
#'
#' @export
enquos <- function(.args, .argsName)
{
    .args <- .args[-1]
    .args.contain <- names(.args) %in% .argsName
    if (length(.args.contain) > 0) {
        .dots <- .args[!(names(.args) %in% .argsName)]
    } else {
        .dots <- .args
    }
    return(.dots)
}


#' @description
#' \code{checkEnquos()} checks colon separators as well as variables whether they
#' are specified or not. if not, it gets all variables within the dataset.
#'
#' @describeIn enquos
#'
#' @export
checkEnquos <- function(.data, .vars, .types = "tab")
{
    .vars.names <- names(.data)

    ## Check if colon is there.
    ## if present, retrieve variables between the two variables
    if (any(grepl(":", .vars))) {
        .vars <- do.call(
            c,
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

    ## if .vars is length zero, then .vars is all variables
    if (length(.vars) == 0) {
        .vars <- .vars.names
        if (.types == "tab") {
            .types <- c("factor", "character", "orderedfactor", "logical")
        } else {
            .types <- c("numeric", "double", "integer", "logical")
        }

        ## get the types of variables
        .vars.type <- unlist(lapply(.data, function(z) {
            .class <- class(unlist(z))[1]
            if (.class == "haven_labelled") {
                .class <- typeof(unlist(z))[1]
            }
            .class
        }))

        ## get only those whose type are in .tab.type
        .vars <- .vars[.vars.type %in% .types]
    }


    ## if no variable is available, stop
    if (length(.vars) == 0) {
        stop(" ... No variable found for tabulation ... ")
    }

    return(.vars)
}



# Functions for displaying outputs ----------------------------------------

#' @title Display functions for `mStats`
#'
#' @description
#' Printing Functions to format and display outputs from mStats package
#'
#' \code{printText}, an old version of \code{printText2} which produces
#' a better output.
#'
#' @param .x vector, matrix, dataframe or separator (in case of printLines)
#' @param .txt texts
#' @param .width desired character length to display
#' @param .sep separator for line break
#' @param .split separator for printText
#' @param .printDF If yes, print as Data.frame
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
#' @export
printText <- function(.x, .txt, .split = NULL, .printDF = FALSE) {

    vars <- names(.x)
    n.ds <- data.frame(
        rbind(sapply(vars, function(z) nchar(as.character(z))),
              sapply(.x, function(z) max(nchar(as.character(z)), na.rm = TRUE)))
    )
    n.ds <- sum(sapply(n.ds, function(z) max(z, na.rm = TRUE)), na.rm = TRUE)
    n.rnames <- max(nchar(as.character(row.names(.x))), na.rm = TRUE)
    .x.width <- n.ds + n.rnames + ncol(.x)

    if (is.null(.split)) {
        .txt <- wrapText(.txt, .x.width)
    } else {
        .txt.split <- unlist(strsplit(.txt, .split))
        .txt.split[2] <- paste0(.split, .txt.split[2], collapse = "")
        .txt.split <- sapply(.txt.split, function(z) wrapText(z, .x.width))
        .txt <- paste0(c(.txt.split[1], .txt.split[2]), collapse = "")
    }

    printLines(.x = "_", .width = .x.width)
    cat("\n", .txt, "\n")
    printLines(.x = "_", .width = .x.width)
    if (.printDF) {
        print.data.frame(.x, row.names = FALSE, max = 1e9)
    } else {
        print(.x)
    }
    printLines(.x = "_", .width = .x.width)
}



#' @describeIn printText example
#' @description
#'
#' \code{printText2()} can print data.frame in a well-formatted style.
#' This added printLines for better visualization.
#'
#' @export
printText2 <- function(.x, .txt, .split = NULL, .printDF = FALSE) {

    vars <- names(.x)
    n.ds <- data.frame(
        rbind(sapply(vars, function(z) nchar(as.character(z))),
              sapply(.x, function(z) max(nchar(as.character(z)), na.rm = TRUE)))
    )
    n.ds <- sum(sapply(n.ds, function(z) max(z, na.rm = TRUE)), na.rm = TRUE)
    n.rnames <- max(nchar(as.character(row.names(.x))), na.rm = TRUE)
    .x.width <- n.ds + n.rnames + ncol(.x)

    if (is.null(.split)) {
        .txt <- wrapText(.txt, .x.width)
    } else {
        .txt.split <- unlist(strsplit(.txt, .split))
        .txt.split[2] <- paste0(.split, .txt.split[2], collapse = "")
        .txt.split <- sapply(.txt.split, function(z) wrapText(z, .x.width))
        .txt <- paste0(c(.txt.split[1], .txt.split[2]), collapse = "")
    }

    printLines(.x = "_", .width = .x.width)
    cat("\n", .txt, "\n")
    printLines(.x = "_", .width = .x.width)
    cat("\n")
    if (.printDF) {
        print.data.frame(.x, row.names = FALSE, max = 1e9)
    } else {
        print(.x)
    }
    printLines(.x = "_", .width = .x.width)
}



#' @describeIn printText example
#' @description
#'
#' \code{printLines()} produces lines as strings for specified length.
#'
#' @export
printLines <- function(.x = "=", .width = 80)
{
    cat(paste(rep(.x, .width), collapse = ""), "\n")
}









#' @describeIn printText example
#' @description
#'
#' \code{printMsg()} produces any text withint two round brackets.
#'
#' @export
printMsg <- function(.txt = NULL)
{
    .txt <- paste0("(", .txt, ")\n", collapse = "")
    .txt <- wrapText(.txt, 80)
    cat(.txt)
}


#' @describeIn printText example
#' @description
#'
#' \code{getnPrintLabel()} extract labels and printMsg().
#'
#' @param .data Dataset
#' @param .var.name Variable name to retrieve label
#'
#' @export
getnPrintLabel <- function(.data, .var.name)
{
    .var.name <- as.character(.var.name)
    .var <- .data[[.var.name]]
    .lbl <- attr(.var, "label")
    if (!is.null(.lbl)) {
        printMsg(paste0(.var.name, ": ", .lbl))
    }
}



#' @describeIn printText example
#' @description
#'
#' \code{wrapText()} add next line to strings. This is used with \code{cat()}.
#'
#' @export
wrapText <- function(.txt, .width = 70, .sep = "\n") {
    #check character length
    .char.len <- nchar(.txt)
    # if character length is more than console width (70)
    # then search iteration number by dividing ch length and width
    if (.char.len > .width) {
        iterate <- floor(.char.len / .width)
        for (i in 0:iterate) {
            if (i == 0) {
                t <- substr(.txt, (i * .width) + 1, (i + 1) * .width)
            } else {
                t <- c(t, .sep, substr(.txt, (i * .width) + 1, (i + 1) * .width))
            }
        }
        .txt <- paste0(t, collapse = "")
    }
    return(.txt)
}








# Splitting by Colon ------------------------------------------------------

#' @title Split variable names separated by colon
#' @description
#'
#' \code{splitByColon()} split arguments by colon separator ":"
#'
#' @param .data Dataset
#' @param .vars.names Variables
#' @param .colon Logical. `TRUE` indicates containing colon.
#'
#' @return
#' character vector
#'
#' @export
splitByColon <- function(.data, .vars.names, .colon)
{
    .vars.names <- unlist(strsplit(.vars.names[.colon], split = ":"))
    .vars.names <- paste0("^", .vars.names, "$")
    .vars.names <- names(.data)[grep(.vars.names[1], names(.data)):
                                    grep(.vars.names[2], names(.data))]
    return(.vars.names)
}





# Add Dash Lines to Dataframe  --------------------------------------------

#' @title Add dash lines to `data.frame`
#'
#' @description
#'
#' This adds dash lines to the dataframe in the first and last rows.
#'
#' @param .df Dataset
#' @param .vLine positional index of Vertical Line `|` in dataset
#'
#' @return
#'
#' modifided \code{data.frame}
#'
#' @export
addDashLines <- function(.df, .vLine = 0)
{
    .names <- names(.df)
    .body.len <- sapply(.df, function(z) {
        max(nchar(as.character(z)), na.rm = TRUE)
    })

    ## get the maximum character numbers per column
    .nchar <- data.frame(rbind(nchar(.names), .body.len))
    .nchar <- sapply(.nchar, FUN = max)

    ## create dash lines
    .dash <- sapply(.nchar, function(z) paste0(rep("-", z), collapse = ""))
    .dash[.vLine] <- "+"

    rbind(.df[0, ], .dash, .df[1:nrow(.df), ], .dash)
}





# Related to processing tables --------------------------------------------



#' @title
#' Change reference level by row or column
#'
#' @description
#' \code{tblRowColOrder()} changes the order of levels in rows or
#' columns by specifying names of levels.
#'
#' @param .tbl table
#' @param .exp.value reference row
#' @param .case.value reference column
#'
#' @details
#'
#' This function is used in calculating risks, odds and associated measures.
#'
#'
#' @export
rowColOrder <- function(.tbl, .exp.value = NULL, .case.value = NULL)
{
    .tbl.row <- rownames(.tbl)
    .tbl.col <- colnames(.tbl)
    .tbl.dim <- names(dimnames(.tbl))
    if (!is.null(.exp.value)) {
        .tbl <- rbind(.tbl[.tbl.row == .exp.value, ],
                      .tbl[.tbl.row != .exp.value, ])
        row.names(.tbl) <- c(.tbl.row[.tbl.row == .exp.value],
                             .tbl.row[.tbl.row != .exp.value])
    }
    if (!is.null(.case.value)) {
        .tbl <- cbind(.tbl[, .tbl.col == .case.value],
                      .tbl[, .tbl.col != .case.value])
        colnames(.tbl) <- c(.tbl.col[.tbl.col == .case.value],
                            .tbl.col[.tbl.col != .case.value])
    }
    names(dimnames(.tbl)) <- .tbl.dim

    return(.tbl)
}




#' @title
#' Split tables to make `2x2` tables
#'
#' @description
#' \code{splitTables()} separates table with rows more than 2 into
#' several tables using a reference row.
#'
#' @param .tbl table
#' @param .exp.value reference row
#'
#' @details
#'
#' This function is used in calculating risks, odds and associated measures.
#'
#'
#' @export
splitTables <- function(.tbl, .exp.value = NULL)
{
    .row.names <- rownames(.tbl)
    .exp.value <- ifelse(is.null(.exp.value), .row.names[1], .exp.value)
    .tbl <- lapply(.row.names, function(z) {
        if (z != .exp.value) {
            .tbl.new <- rbind(.tbl[.exp.value, ], .tbl[z, ])
            rownames(.tbl.new) <- c(.row.names[.row.names == .exp.value],
                                    .row.names[.row.names == z])
            .tbl.new
        }
    })


    .tbl <- .tbl[lapply(.tbl, length) > 0]

    return(.tbl)
}





#' @title Plot estimates with 95% Confidence Interval
#'
#' @description
#' It creates a plot for estimates and their 95\% CI
#'
#' @param .p prevalence
#' @param .ll lower limit of CI
#' @param .ul upper limit of CI
#' @param .x.name xlab or x variable
#' @param .by.name ylab or y variable
#' @param .ylab ylab
#'
#'
#' @export
plotRisks <- function(.p, .ll, .ul, .x.name, .by.name, .ylab = "Risks")
{

    .df <- data.frame(by = names(.p), risks = .p, lower = .ll, upper = .ul)
    .df <- .df[order(.df$by), ]

    plot(.df$by, .df$risks, ylim = c(min(.df$lower, na.rm = TRUE),
                                     max(.df$upper, na.rm = TRUE)),
         main = paste0(.ylab, " of '", .by.name, "'"),
         xlab = .x.name,
         ylab = .ylab)
    nrow_by <- nrow(.df)
    segments(1:nrow_by, .df$risks, 1:nrow_by, .df$upper, col = "blue", lwd = 2)
    segments(1:nrow_by, .df$risks, 1:nrow_by, .df$lower, col = "blue", lwd = 2)
}
