#' @title Population Pyramid Graph
#'
#' @description
#'
#' \code{pyramid()} draws a population pyramid graph of age and sex. Other similar
#' types of variables can be used.
#'
#' @param data Dataset
#' @param age age variable or a continuous variable
#' @param sex sex variable or a binary variable
#' @param cut either a single number or a numeric vector.
#' @inheritParams graphics::title
#' @param label_left title of left plot
#' @param label_right title of right plot
#' @param label_center title of center plot
#' @param center_width width between two plots: Default value is calculated
#' on the maximum character number of labels in the center.
#' @param na.rm A logical value to specify missing values
#'
#'
#' @importFrom graphics rect segments text
#' @importFrom utils capture.output
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
#' ## use infert data
#'
#' data(infert)
#' # pyramid(infert, age, case)
#' # pyramid(infert, age, case, cut = 5)
#' # pyramid(infert, age, case, cut = 2)
#' # pyramid(infert, age, case, cut = 2,
#' #         label_left = "No", label_right = "yes")
#'
#' @export
pyramid <- function(data, age, sex, cut = NULL, main = NULL,
                    label_left = NULL, label_right = NULL,
                    label_center = NULL, center_width = NULL, na.rm = FALSE)
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

    ## get x and y names
    .x_name <- .args$age
    .y_name <- .args$sex

    .x <- .y <- .x_cat <- NULL


    ## process x into categories by using egen with inherent argument option
    .data$.x <- .data[[.x_name]]
    invisible(capture.output(.data <- egen(.data, .x, cut = cut)))
    .data$.x_cat <- gsub("lbl\\[|\\]", "", .data$.x_cat)

    ## get y
    .data$.y <- .data[[.y_name]]

    ## cross-tabulate x and y
    invisible(capture.output(.tab <- tab(.data, .x_cat, by = .y, na.rm = na.rm)))

    ## remove percentages and other columns
    .tab <- .tab[ -c(1, (nrow(.tab)-2):nrow(.tab)), c(2, 4, 6)]

    ## get individual columns and percentage
    .lbl <- .tab[[1]]
    .f <- as.numeric(.tab[[2]])
    .m <- as.numeric(.tab[[3]])

    ## get labels
    label_left <- ifelse(is.null(label_left), names(.tab)[2], label_left)
    label_right <- ifelse(is.null(label_right), names(.tab)[3], label_right)
    label_center <- ifelse(is.null(label_center), .x_name, label_center)

    .xaxis <- pretty(c(0, max(c(.f, .m), na.rm = TRUE)))
    .xaxis_min <- min(.xaxis, na.rm = TRUE)
    .xaxis_max <- max(.xaxis, na.rm = TRUE)
    .xaxis_range <- .xaxis_max - .xaxis_min
    .xaxis_scale <- (.xaxis - .xaxis_min) / .xaxis_range

    ## constant numbers : change later <<<<---  ------
    ## constant variables
    ## center width
    .cwidth <- ifelse(is.null(center_width),
                      max(nchar(.lbl), na.rm = TRUE) / 50, center_width)


    ## main title label
    main <- ifelse(is.null(main),
                   paste0("Population pyramid of '", .x_name,
                          "' by '", .y_name, "'"), main)
    x <- c(-1 - .cwidth, 1 + .cwidth)
    y <- c(-0.05, 1.1)
    plot(x, y, type = "n", axes = FALSE, xlab = "Frequency", ylab = "",
         main = main)

    segments((-.xaxis_scale) - .cwidth, -0.01,
             (-.xaxis_scale) - .cwidth, 0.01)
    segments((.xaxis_scale) + .cwidth, -0.01,
             (.xaxis_scale) + .cwidth, 0.01)

    ## create x and y lines on left
    lines(c(-1 - .cwidth, -.cwidth), c(0 , 0), lty = 1)
    lines(c(-.cwidth, -.cwidth), c(0, 1 + .cwidth/2), lty = 1)

    ## create x and y lines on right
    lines(c(1 + .cwidth, .cwidth), c(0, 0), lty = 1)
    lines(c(.cwidth, .cwidth), c(0, 1 + .cwidth/2), lty = 1)

    ## label axis and ticks
    text(-0.5 - .cwidth, 1, label_left, pos = 3)
    text(0.5 + .cwidth, 1, label_right, pos = 3)
    text(0, 1 + .cwidth/2, label_center, pos = 3)

    ## at center texts for age
    .lbl_len <- length(.lbl)
    for (i in 0:(.lbl_len - 1)) {
        if ((i%%1) == 0) {
            text(0, (i/.lbl_len) + (.cwidth / (.lbl_len)),
                 paste(.lbl[i + 1]), pos = 3,
                 cex = 1)
        }
    }

    ## add text for axis ticks
    text((-.xaxis_scale) - .cwidth, rep(0, length(.xaxis)),
         .xaxis, pos = 1)
    text((.xaxis_scale) + .cwidth, rep(0, length(.xaxis)),
         .xaxis, pos = 1)

    .y_start <- 0:(.lbl_len - 1)/.lbl_len
    .y_end <- 1:.lbl_len/.lbl_len
    .Left_box <- -(.f - .xaxis_min) / .xaxis_range - .cwidth
    rect(.Left_box, .y_start, rep(-.cwidth, .lbl_len), .y_end,
         col = "blue")

    .Right_box <- (.m - .xaxis_min) / .xaxis_range + .cwidth
    rect(rep(.cwidth, .lbl_len), .y_start, .Right_box, .y_end,
         col = "red")


    ## print text notification
    names(.tab)[1] <- as.character(.x_name)
    .tab <- addDashLines(.tab, .vline = 2)
    printText("Graphing population pyramid ... ")
    invisible(.tab)
}
