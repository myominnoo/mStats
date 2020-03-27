#' @title Codebook: Detailed information about variables
#'
#' @description
#' \code{codebook()} provides detailed information about each variable
#' within the dataset.
#'
#' @param data dataframe
#'
#' @details
#' \code{codebook} generates the report of data structure with names,
#' data lables, types,
#' number of observations, number of observations with missing values and
#' percentage of observations with missing values.
#'
#' \strong{ANNOTATIONS}:
#'
#' Variable - Names of variables
#'
#' Label     - Labels of variables
#'
#' Type     - Types of variables
#'
#' _Obs - Counts of valid observations
#'
#' _NA  - Counts of observations with missing value
#'
#' _NA(%)    - Percentage of observations with missing value
#'
#'
#' @return
#' Codebook in data.frame format
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
#'
#' ## use infert data
#' data(infert)
#' codebook(infert)
#'
#' ## add labels
#' infert.new <- labelVar(infert,
#'                        c(education, age, parity, induced, case, spontaneous,
#'                          stratum, pooled.stratum),
#'                        c("Education", "Age in years of case", "Count",
#'                          "# of prior induced abortions", "case status",
#'                          "# of prior spon. abortions",
#'                          "Matched set number", "Stratum Number"))
#' infert.new <- labelData(infert.new,
#'                         "Infertility after Spontaneous and Induced Abortion")
#' codebook(infert.new)
#'
#'
#' @export
codebook <- function(data)
{
    .data.name <- deparse(substitute(data))
    .vars.names <- names(data)
    .vars.numeric <- c("integer", "double", "numeric")
    .vars.factor <- c("factor", "character")
    .vars.logical <- c("logical")
    .vars.date <- c("Date")

    ## if data is not data.frame, stop
    if (!is.data.frame(data))
        stop(paste0(" ... '", .data.name, "' is not data.frame ... "))

    ## get the types of variables
    .vars.type <- unlist(lapply(data, function(z) {
        .class <- class(unlist(z))[1]
        if (.class == "haven_labelled") {
            .class <- typeof(unlist(z))[1]
        }
        .class
    }))

    ## get the labels
    .vars.lbl <- paste(sapply(.vars.names, function(z) {
        .lbl <- attr(data[[z]], "label")
        .lbl <- ifelse(is.null(.lbl), "<NA>",
                       ifelse(nchar(.lbl) > 32,
                              paste0(strtrim(paste0(.lbl, collapse = ""), 32), "..."),
                              paste0(.lbl, collapse = "")))
    }))

    ## get count numbers for all observations, obs. without NA and NAs
    .obs.counts <- sapply(.vars.names, function(z)
        sum(as.numeric(!is.na(data[, z])), na.rm = TRUE))
    .na.counts <- sapply(.vars.names, function(z)
        sum(as.numeric(is.na(data[, z])), na.rm = TRUE))

    .df <- data.frame(1:length(.vars.names),
                      .vars.names, "|",
                      .vars.lbl,
                      .vars.type,
                      .obs.counts,
                      .na.counts,
                      round(.na.counts / nrow(data) * 100, 1),
        stringsAsFactors = FALSE,
        row.names = NULL
    )

    names(.df) <- c("No", "Variable", "|", "Label", "Type", "_Obs", "_NA", "_NA(%)")

    ## add dash lines
    .df <- addDashLines(.df, .vLine = 3)

    ## display output
    .txt <- paste0("Codebook: '", .data.name, "'")

    printText2(.df, .txt, .printDF = TRUE)

    ## display dataset label
    .data.lbl <- attr(data, "label")
    if (!is.null(.data.lbl)) {
        printMsg(paste0("Dataset label: ", .data.lbl))
    }

    invisible(.df)
}
