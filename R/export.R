#' @title Export outputs
#'
#' @description
#'
#' \code{export()} writes to `.csv` file
#'
#' @param .data Outputs as `data.frame` or `list`
#' @param file file name
#' @param append logical.
#'
#' If `TRUE`, the output is appended to the file.
#'
#' If `FALSE`, any existing file of the name is destroyed.
#'
#' @details
#' Outputs from the following functions can be exported.
#'
#' \code{\link{tab}}
#'
#' \code{\link{summ}}
#'
#' \code{\link{tabOdds}}
#'
#' \code{\link{tabRisks}}
#'
#' \code{\link{mhor}}
#'
#' \code{\link{mhrr}}
#'
#' \code{\link{logistic}}
#'
#' \code{\link{regress}}
#'
#' \code{\link{strate}}
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
export <- function(.data, file = "", append = FALSE)
{
    if (file == "") {
        file <- file.path(tempdir(), "output.csv")
    }

    if (!is.list(.data)) {
        .data <- list(.data)
    }

    .len <- length(.data)
    tryCatch({
        lapply(1:.len, function(z) {
            if (!any(class(.data[[z]]) %in% c("glm", "lm"))) {
                if (z == 1) {
                    if (append) {
                        suppressWarnings(
                            write.table(rep("-", 2), file, append = TRUE, sep = ",",
                                        row.names = F, col.names = F)
                        )
                    }
                    suppressWarnings(
                        write.table(.data[[z]], file, append = append,
                                    sep = ",", row.names = FALSE)
                    )
                } else {
                    suppressWarnings(
                        write.table(rep("-", 2), file, append = TRUE, sep = ",",
                                    row.names = F, col.names = F)
                    )
                    suppressWarnings(
                        write.table(.data[[z]], file, append = TRUE, sep = ",",
                                    row.names = F)
                    )
                }
            }

        })

        if (append) {
            printMsg(paste0("appended to '", file, "'"))
        } else {
            printMsg(paste0("saved as '", file, "'"))
        }
    }, error = function(cnd) {
        warning(cnd)
        printMsg(paste0("failed to save '", file, "'"))
    })
}
