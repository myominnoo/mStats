
#' @title Detailed information of a dataset
#' and its variables
#'
#' @description
#'
#' \code{codebook()} provides detailed information about the dataset
#' itself and its variables.
#'
#' @param data name of a dataset
#'
#' @details
#' It reports data structure of the dataset specified with names of
#' variables, their labels, data types, number of observations,
#' number of observations with missing values and
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
#' Obs_Num - Counts of valid observations
#'
#' NA  - Counts of observations with missing value
#'
#' NA(%)    - Percentage of observations with missing value
#'
#'
#' @return
#'
#' `data.frame`
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
#' data(infert)
#' codebook(infert)
#'
#'
#' ## add labels
#' infert.new <- labelVar(infert,
#'                        education = "EDUCATION",
#'                        age = "AGE IN YEARS",
#'                        parity = "count",
#'                        case = "case status",
#'                        induced = "# of prior induced abortions",
#'                        spontaneous = "# of prior spon. abortions",
#'                        stratum = "Matched set number",
#'                        pooled.stratum = "Stratum number")
#'
#' infert.new <- labelData(infert.new,
#'     "Infertility after Spontaneous and Induced Abortion")
#' codebook(infert.new)
#'
#'
#' @export
codebook <- function(data)
{
    ## copy data to .data
    .data <- data

    ## get names of dataset and headings
    .data_name <- deparse(substitute(data))
    .vars_names <- names(.data)

    ## if input is not a data.frame, stop
    if (!is.data.frame(.data)) {
        stop("`.data` must be a data.frame", call. = FALSE)
    }

    ## check each variable's type
    .vars_type <- sapply(.data, function(z) {
        .class <- class(unlist(z))[1]
        if (.class == "haven_labelled") {
            .class <- typeof(unlist(z))[1]
        }
        .class
    })

    ## get label of each variable
    ## paste each label if not a single vector
    .vars_lbl <- sapply(.vars_names, function(z) attr(.data[[z]], "label"))
    .vars_lbl <- sapply(1:length(.vars_names), function(z) {
        if (.vars_lbl[z] == "NULL") {
            .lbl <- "<NA>"
        } else {
            .lbl <- paste0(.vars_lbl[z])
        }
        .lbl <- ifelse(nchar(.lbl) > 30,
                       paste0(strtrim(.lbl, 30), "..."),
                       .lbl)
        .lbl
    })

    ## get count numbers for all observations, obs. without NA and NAs
    .obs_num <- sapply(.vars_names, function(z) sum(!is.na(.data[[z]])))
    .na_num <- sapply(.vars_names, function(z) sum(is.na(.data[[z]])))

    ## create data.frame to put all information together
    .df <- data.frame(1:length(.vars_names),
                      .vars_names,
                      .vars_lbl,
                      .vars_type,
                      .obs_num,
                      .na_num,
                      round(.na_num / nrow(.data) * 100, 1))
    names(.df) <- c("No", "Variable", "Label", "Type", "Obs_Num",
                    "<NA>", "<NA>(%)")

    ## add dash lines to data.frame
    ## remove row names
    .df <- addDashLines(.df, .vline = 3)
    row.names(.df) <- NULL

    ## print outputs
    ## create texts for output display
    .txt <- paste0("Codebook: '", .data_name, "'")

    ## print data.frame
    printDF(.df, .txt)

    ## print label of the dataset
    printLabel(.data)


    ## return
    invisible(.df)
}
