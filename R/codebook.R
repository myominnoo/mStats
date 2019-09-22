#' @title Codebook
#' @description
#' \code{codebook} displays structure of a data frame
#' @param x Data Frame
#' @param f data frame
#' @param title label
#' @param txt character vector
#' @param width desired console width
#' @param sep wrapper
#' @details
#' \code{codebook}
#'
#' generates the report of data structure with names, data lables, types,
#' number of observations, number of observations with missing values and
#' percentage of observations with missing values.
#'
#' ANNOTATIONS:
#'
#' VARS_NAME - Names of variables
#' LABEL     - Labels of variables
#' TYPE      - Types of variables
#' OBS_COUNT - Counts of valid observations
#' NA_COUNT  - Counts of observations with missing value
#' (NA %)    - Percentage of observations with missing value
#'
#' @seealso \code{\link{ilog}}
#' @keywords codebook, summary, structure, layout
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' str(infert)
#' codebook(infert)
#' str(iris)
#' codebook(iris)
#'
#' # if something else
#' codebook(iris$Species)

#' @export
codebook <- function(x) {
  UseMethod("codebook")
}

#' @rdname codebook
#' @export
codebook.data.frame <- function(x) {
  var.names <- names(x)
  type.numeric <- c("integer", "double", "numeric")
  type.factor <- c("factor", "character", "logical")
  type.date <- c("Date")

  var.type <- sapply(var.names, function(z) class(unlist(x[ , z])))

  lbl <- paste(sapply(var.names, function(z) {
    lbl.attr <- attr(x[[z]], "label")
    lbl.attr <- ifelse(is.null(lbl.attr), "NULL", lbl.attr)
    lbl.attr <- strtrim(lbl.attr, 40)
  }
  ))

  na.counts <- sapply(var.names, function(z) sum(as.numeric(is.na(x[, z]) )))
  obs.counts <- sapply(var.names, function(z) sum(as.numeric(!is.na(x[, z]) )))

  f <- data.frame(names(var.type), lbl, var.type, obs.counts, na.counts,
                  paste(round(na.counts / nrow(x) * 100, 1), "%"),
                  row.names = NULL)
  names(f) <- c("VARS_NAME", "LABEL", "TYPE", "OBS_COUNT", "NA_COUNT", "(NA %)")

  df.lbl <- attr(x, "label")
  df.lbl <- ifelse(length(df.lbl) > 0,
                   paste0("\nlabel: ", attr(x, "label")), "\nlabel: NULL")
  # print formating
  output.format(f, paste0("Codebook of ", deparse(substitute(x)), df.lbl))

  invisible(f)
}

#' @rdname codebook
#' @export
codebook.default <- function(x) {
  cat(paste(rep(" ... ", 11), collapse = ""), "\n")
  cat("Try codebook(iris) \n")
  cat(paste(rep(" ... ", 11), collapse = ""), "\n")
  f <- summary(x)
  print(f)
  invisible(f)
}

#' @rdname codebook
#' @export
output.format <- function(f, title) {
  vn <- names(f)
  vn[is.na(vn)] <- "<NA>"
  names(f) <- vn
  f.width <- sum(
    sapply(vn, function(z)
      max(rbind(nchar(z), nchar(as.character(f[,z]))), na.rm = TRUE)),
    ncol(f), max(nchar(row.names(f)), na.rm = TRUE),
    na.rm = TRUE
  )
  f.splt <- paste(rep("=", f.width), collapse = "")

  title <- wrap.output(title, f.width, "\n")

  cat(paste0(f.splt, "\n", title, "\n",
             f.splt, "\n"))
  print(f)
  cat(f.splt, "\n")
}


#' @rdname codebook
#' @export
wrap.output <- function(txt, width = 90, sep = "\n") {
  txt.len <- nchar(txt)
  if (txt.len > width) {
    iterate <- floor(txt.len / width)
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
