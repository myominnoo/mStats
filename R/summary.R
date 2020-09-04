##' @title Table Format for Publication
##'
##' @name summary_mStats
##' @rdname summary_mStats
##'
##' @description
##' \code{summary()} gathers statistics
##' in table format for publication.
##'
##' @param object an object for which a summary is desired.
##' @param ... additional arguments affecting the summary produced.
##'
##'
##'
NULL


##' @rdname summary_mStats
##'
##' @description
##'
##' \code{tab1} for one-way tabulation of \code{tab()}
##'
##' @export
summary.tab1 <- function(object, ... )
{
    x <- object

    ## get tab
    .x <- x$tab

    ## get number of rows and columns
    .nr <- nrow(.x)
    .nc <- ncol(.x)

    ## index for dashed rows
    .ndash <- grep("-", .x[["Variables"]])

    ## Move variable names one row up.
    .x$Variables[.ndash] <- .x$Variables[.ndash + 1]
    ## Create N (%)
    .x$Freq <- paste0(.x$Freq, " (", .x$Percent, ")")
    ## remove dashed lines
    .x[.ndash, c(2, 4:5)] <- ""
    .x[.ndash[-length(.ndash)] + 1, 1] <- ""

    ## combine total row and the rest
    .df <- rbind(.x[.nr - 1, c(1:2, 4)],
                 .x[-c(.nr:(.nr-2)), c(1:2, 4)])
    ## rename Total row
    .df[1, 1:2] <- c("    Total", "")


    ## Change header
    names(.df)[3] <- c("N (%)")
    row.names(.df) <- NULL

    ## Process labels
    .lbl <- x$lbl
    .lbl.null <- sapply(.lbl$lbl, FUN = is.null)
    .lbl$lbl[.lbl.null] <- .lbl$var[.lbl.null]
    .lbl <- .lbl[.lbl$var %in% .df$Variables, ]

    ## get label index
    .index <- sapply(.lbl$var, function(z) {
        grep(paste0("^", z, "$"), .df$Variables, useBytes = TRUE)
    })
    .index <- unlist(.index[sapply(.index, FUN = length) > 0])

    ## Change var to lbl names
    .df[.index, 1] <- unlist(.lbl$lbl)

    ## Align the dataset
    .df$Variables <- format(.df$Variables, justify = "left")
    .df$Category <- format(.df$Category, justify = "left")

    return(.df)
}

##' @rdname summary_mStats
##'
##' @description
##'
##' \code{tab2p} for two-way tabulation with percentages of \code{tab}
##'
##' @param p Percentage columns present - TRUE or NOT - FALSE
##'
##' @export
summary.tab2p <- function(object, p = 2, ... )
{
    x <- object
    ## get tab
    .x <- x$tab

    ## get number of rows and columns
    .nr <- nrow(.x)
    .nc <- ncol(.x)

    ## index for dashed rows
    .ndash <- grep("-", .x[["Variables"]])

    ## get indexes for n and % columns
    .ni <- seq(4, .nc - 2, p)
    .pi <- seq(5, .nc - 2, p)

    ## combine n (%)
    .df <- do.call(
        cbind,
        lapply(1:length(.ni), function(z) {
            if (p == 2) {
                .df <- data.frame(paste0(
                    .x[[.ni[z]]], " (", .x[[.pi[z]]], ")"
                ))
                names(.df) <- paste0(names(.x)[.ni[z]], " (%)")
            } else {
                .df <- data.frame(.x[[.ni[z]]])
                names(.df) <- names(.x)[.ni[z]]
            }
            .df
        })
    )

    ## remove n % columns
    .x <- .x[, c(1:2, .nc - 1, .nc)]
    ## Move variable names one row up.
    .x$Variables[.ndash] <- .x$Variables[.ndash + 1]

    ## combine all and process dashlines
    .df <- cbind(.x[, 1:2], .df, .x[, 3:4])
    .df[.ndash, 2:ncol(.df)] <- .df[.ndash[-length(.ndash)] + 1, 1] <- ""

    .nr <- nrow(.df)
    .df <- rbind(.df[.nr - 1, ], .df[-c((.nr - 2):.nr), ])
    row.names(.df) <- NULL

    ## Process labels
    .lbl <- x$lbl
    .lbl.null <- sapply(.lbl$lbl, FUN = is.null)
    .lbl$lbl[.lbl.null] <- .lbl$var[.lbl.null]
    .lbl <- .lbl[.lbl$var %in% .df$Variables, ]

    ## get label index
    .index <- sapply(.lbl$var, function(z) {
        grep(paste0("^", z, "$"), .df$Variables, useBytes = TRUE)
    })
    .index <- unlist(.index[sapply(.index, FUN = length) > 0])

    ## Change var to lbl names
    .df[.index, 1] <- unlist(.lbl$lbl)

    ## Align the dataset
    .df$Variables <- format(.df$Variables, justify = "left")
    .df$Category <- format(.df$Category, justify = "left")

    return(.df)
}



##' @rdname summary_mStats
##'
##' @description
##'
##' \code{tab2} for two-way tabulation without
##' percentages of \code{tab}
##'
##' @export
summary.tab2 <- function(object, ... )
{
    summary.tab2p(object, p = 1)
}


##' @rdname summary_mStats
##'
##' @description
##'
##' \code{summ1} for summary measures of \code{summ}
##'
##' @export
summary.summ1 <- function(object, ... )
{
    x <- object
    ## get tab
    .x <- x$summ

    ## get number of rows and columns
    .nr <- nrow(.x)
    .nc <- ncol(.x)

    ## index for dashed rows
    .ndash <- grep("-", .x[["Variables"]])

    ## total obs.
    .n_ <- max(as.numeric(.x$Obs.[-c(1, .nr)]),
               na.rm = TRUE)

    ## number of obs. and NA
    .obs <- paste0(.x$Obs., " (", .x$NA.,")")
    .mean <- paste0(.x$Mean, " (", .x$Std.Dev, ")")
    .med <- paste0(.x$Median, " (", .x$Q1, " - ", .x$Q3, ")")
    .range <- paste0(.x$Min, " - ", .x$Max)

    ## combine all statistics
    .t <- data.frame(.var = .x$Variables,
                     .obs, .mean, .med, .range)
    ## remove first and last row
    .df <- .t[-c(1, nrow(.t)), ]
    names(.df) <- c("Variables", "n (NA)", "mean (SD)",
                    "median (IQR)", "min - max")
    row.names(.df) <- NULL

    ## Process labels
    .lbl <- x$lbl
    .lbl.null <- sapply(.lbl$lbl, FUN = is.null)
    .lbl$lbl[.lbl.null] <- .lbl$var[.lbl.null]
    .lbl <- .lbl[.lbl$var %in% .df$Variables, ]

    ## get label index
    .index <- sapply(.lbl$var, function(z) {
        grep(paste0("^", z, "$"), .df$Variables, useBytes = TRUE)
    })
    .index <- unlist(.index[sapply(.index, FUN = length) > 0])

    ## Change var to lbl names
    .df[.index, 1] <- unlist(.lbl$lbl)


    ## Align the dataset
    .df$Variables <- format(.df$Variables, justify = "left")
    .df[[2]] <- format(.df[[2]], justify = "left")

    return(.df)
}


##' @rdname summary_mStats
##'
##' @description
##'
##' \code{summ2} for summary measures of \code{summ}
##' @param stats Statistics to be generated.
##'
##' "all" - all statistics
##' "mean" - mean (Standard Deviation)
##' "median" - median (Interquartile Range)
##' "range" - minimum - maximum
##'
##' @export
summary.summ2 <- function(object, stats = "all", ... )
{
    summary.summ2t(object, stats = stats, test = FALSE)
}

##' @rdname summary_mStats
##'
##' @description
##'
##' \code{summ2} for summary measures of \code{summ}
##' @param test TRUE reports p-values from statistical tests
##'
##' @export
summary.summ2t <- function(object, stats = "all", test = TRUE, ... )
{
    x <- object
    ## get tab
    .x <- x$summ

    ## get number of rows and columns
    .nr <- nrow(.x)
    .nc <- ncol(.x)

    ## index for dashed rows
    .ndash <- grep("-", .x[["Obs."]])

    ## get variable index and variable names
    .vari <- .ndash + 1
    .vari <- .vari[seq(2, length(.vari), 2)]
    .vars_names <- .x[.vari, "Variables"]

    ## get levels names
    .x$Variables <- gsub(
        paste0(.vars_names, collapse = "|"),
        "", .x$Variables
    )

    ## number of obs. and NA
    .obs <- paste0(.x$Obs., " (", .x$NA.,")")
    .mean <- paste0(.x$Mean, " (", .x$Std.Dev, ")")
    .med <- paste0(.x$Median, " (", .x$Q1, " - ", .x$Q3, ")")

    ## create all dataset, transpose index, stats name
    all <- data.frame(lvl = .x$Variables, .obs, .mean, .med)
    .ti <- 3
    .stats <- c("", "n (NA)", "mean (SD)",
                "median (IQR)")

    ## based on test TRUE OR FALSE
    if (test) {
        .test <- .x[!grepl("-", .x[[11]]) &
                        .x[[11]] != "", 11:12]
    } else {
        .range <- paste0(.x$Min, " - ", .x$Max)
        all <- cbind(all, .range)
        .ti <- 4
        .stats <- c(.stats, "min - max")
    }

    ## get length of levels
    .lvl_len <- (.ndash[2] - 2)
    ## remove dashed lines from all
    .t <- cbind(var = rep(.vars_names, each = .lvl_len + 1),
                all[-.ndash, ])

    ## matrix index to transpose
    .mi <- seq(0, nrow(.t), .lvl_len + 1)
    .mi <- cbind(.mi[-length(.mi)] + 1, .mi[-1])

    ## get tranposed statistics
    .df <- do.call(
        rbind,
        lapply(1:nrow(.mi), function(z) {
            .seq <- seq(.mi[z, 1], .mi[z, 2])
            .t <- t(.t[.seq, -c(1, 2)])
            names(.t) <- 1:ncol(.t)
            rbind("", .t)
        })
    )
    ## get variable column
    .vars <- do.call(
        c,
        lapply(1:length(.vars_names), function(z) {
            c(.vars_names[z], rep("", .ti))
        })
    )

    ## combine all
    .df <- data.frame(.vars, .stats, .df)
    names(.df) <- c("Variables", "Statistics",
                    .x[seq(2, .ndash[2] - 1, 1), 1],
                    "Total")

    ## set row names
    row.names(.df) <- NULL

    ## if test is TRUE, combine test p-value
    if (test) {
        .test <- do.call(
            rbind,
            lapply(1:nrow(.mi), function(z) {
                .t <- rbind(.test[z, ], "", "", "")
                rbind(.t[2, ], .t[-2, ])
            })
        )
        .df <- cbind(.df, .test)
    }

    ## Process labels
    .lbl <- x$lbl
    .lbl.null <- sapply(.lbl$lbl, FUN = is.null)
    .lbl$lbl[.lbl.null] <- .lbl$var[.lbl.null]
    .lbl <- .lbl[.lbl$var %in% .df$Variables, ]

    ## get label index
    .index <- sapply(.lbl$var, function(z) {
        grep(paste0("^", z, "$"), .df$Variables, useBytes = TRUE)
    })
    .index <- unlist(.index[sapply(.index, FUN = length) > 0])

    ## Change var to lbl names
    .df[.index, 1] <- unlist(.lbl$lbl)

    ## check stats
    .blk <- .df$Statistics == ""
    .chk_mean <- .df$Statistics == "mean (SD)"
    .chk_med <-  .df$Statistics == "median (IQR)"

    ## Get conditional dataset based on stats
    .df <- switch(stats,
                  all = .df,
                  mean = .df[(.blk | .chk_mean), ],
                  median = .df[(.blk | .chk_med), ],
                  range = {
                      if (test) {
                          stop(paste0(
                              "No Range reported. ",
                              "Set 'test` to FALSE in summ()."
                          ),
                          call. = FALSE)
                      } else {
                          .chk_ran <-  .df$Statistics == "min - max"
                          .df[(.blk | .chk_ran), ]
                      }
                  })

    ## Align the dataset
    .df$Variables <- format(.df$Variables, justify = "left")
    .df[[2]] <- format(.df[[2]], justify = "left")

    ## Set row names
    row.names(.df) <- NULL

    return(.df)
}
