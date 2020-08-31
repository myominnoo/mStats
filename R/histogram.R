#' @title Histograms with overlay normal curve
#'
#' @description
#'
#' \code{histogram()} draws a histogram with formatted texts and
#' adds a normal curve over the histogram.
#'
#' @param data Dataset
#' @param var variable
#' @inheritParams graphics::hist
#' @inheritParams graphics::title
#' @param curve logical. If `TRUE` (default), a normal curve is
#' overlaid over the histogram.
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
#' ## use infert data
#'
#' data(infert)
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

    ## copy data to .data
    .data <- data

    ## get names of dataset and headings
    .data_name <- deparse(substitute(data))
    .vars_names <- names(.data)
    .var_name <- as.character(.args$var)

    ## if input is not a data.frame, stop
    if (!is.data.frame(.data)) {
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
        printText(paste0("A histogram is drawn for '", .var_name, "'",
                         ifelse(
                             curve,
                             " with overlay normal curve", ""
                         )))
    }, error = function(cnd) {
        stop(cnd, call. = FALSE)
    })

    invisible(.df)
}
