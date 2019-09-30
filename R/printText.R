#' @title Support Print Functions for mStats package
#'
#' @description
#' A collection of Support Print Functions for mStats package
#'
#' @param x vector, matrix, dataframe or separator (in case of printDeco)
#' @param txt texts
#' @param width desired character length to display
#' @param sep separator for line break
#' @param split separator for printText
#'
#' @keywords support
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#' x <- rnorm(100)
#' plot(x)
#' clear()
#' }

#' @export
printText <- function(x, txt, split = NULL) {
    vars <- names(x)
    n.ds <- data.frame(
        rbind(sapply(vars, function(z) nchar(as.character(z))),
              sapply(x, function(z) max(nchar(as.character(z)), na.rm = TRUE)))
    )
    n.ds <- sum(sapply(n.ds, function(z) max(z, na.rm = TRUE)), na.rm = TRUE)
    n.rnames <- max(nchar(as.character(row.names(x))), na.rm = TRUE)
    x.width <- n.ds + n.rnames + ncol(x)

    if (is.null(split)) {
        txt <- wrapText(txt, x.width)
    } else {
        txt.split <- unlist(strsplit(txt, split))
        txt.split[2] <- paste0(split, txt.split[2], collapse = "")
        txt.split <- sapply(txt.split, function(z) wrapText(z, x.width))
        txt <- paste0(c(txt.split[1], txt.split[2]), collapse = "")
    }

    printDeco(x = "=", width = x.width)
    cat(txt, "\n")
    printDeco(x = "~", width = x.width)
    print(x)
    printDeco(x = "=", width = x.width)
}


#' @rdname printText
#' @export
printDeco <- function(x = "=", width = 80)
{
    cat(paste(rep(x, width), collapse = ""), "\n")
}


#' @rdname printText
#' @export
printMsg <- function(txt = NULL)
{
    txt <- paste0("(", txt, ")\n", collapse = "")
    txt <- wrapText(txt)
    cat(txt)
}


#' @rdname printText
#' @export
warningText <- function(txt = NULL)
{
    txt <- paste0("*** ", txt, " ***\n", collapse = "")
    txt <- wrapText(txt)
    cat(txt)
}



#' @rdname printText
#' @export
wrapText <- function(txt, width = 80, sep = "\n") {
    #check character length
    ch.len <- nchar(txt)
    # if character length is more than console width (80)
    # then search iteration number by dividing ch length and width
    if (ch.len > width) {
        iterate <- floor(ch.len / width)
        for (i in 0:iterate) {
            if (i == 0) {
                t <- substr(txt, (i * width) + 1, (i + 1) * width)
            } else {
                t <- c(t, sep, substr(txt, (i * width) + 1, (i + 1) * width))
            }
        }
        txt <- paste0(t, collapse = "")
    }
    return(txt)
}


