#' @title Manipulate labels: Label columns or dataframe
#' @description
#' \code{labelVars} attaches a label to variables or a dataframe
#' @param x a variable or a list or a dataframe
#' @param lbl specify a string
#' @param data a dataframe object (Optional)
#' @details
#'
#' Dataframe and variable labels are displayed when you \code{\link{codebook}}
#' the dataframe.
#' If a label is not specified, existing label is removed.
#'
#' If data is specified, it returns the whole dataframe with recoded variables.
#'
#' @seealso \code{\link{codebook}}, \code{\link{labelData}}, \code{\link{labelValue}}
#' @keywords dataframe label, data frame, label
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' codebook(iris)
#'
#' # label one variable
#' iris1 <- labelVars(Sepal.Length, "Length of Sepal", iris)
#' codebook(iris1)
#' # label multiple variables in a list
#' iris2 <- labelVars(list(Sepal.Width, Petal.Length, Petal.Width, Species),
#'                    list("Width of Sepal", "Length of Petal",
#'                         "Width of Petal", "Type of Species"), iris1)
#' codebook(iris2)
#'
#' # label dataframe
#' labelVars(iris2, "Fisher's Iris data set")
#' iris3 <- labelData(iris2, "Fisher's Iris data set")
#' codebook(iris3)


#' @export
labelVars <- function(x, lbl = NULL, data = NULL)
{
  arguments <- as.list(match.call())
  if (!is.null(data)) {
    x <- eval(substitute(x), data)
    if (as.character(arguments$x)[1] %in% c("list"))
      x <- list(as.character(arguments$x)[-1])
  }
  UseMethod("labelVars", x)
}

#' @rdname labelVars
#' @export
labelVars.default <- function(x, lbl = NULL, data = NULL)
{
  arguments <- as.list(match.call())
  x.name <- deparse(substitute(x))
  # if (is.null(lbl)) stop(" ... Specify a label ... ")

  if (!is.null(data)) {
    attr(data[, x.name], "label") <- lbl
    f <- data
  } else {
    attr(x, "label") <- lbl
    f <- x
  }
  cat(paste0("\tvariable: labelled '", x.name, "' as '",
             lbl, "' ... \n"))
  invisible(f)
}

#' @rdname labelVars
#' @export
labelVars.list <- function(x, lbl = NULL, data = NULL)
{
  arguments <- as.list(match.call())
  if (is.null(lbl)) stop(" ... Specify labels in a list ... ") else
    lbl <- as.character(arguments$lbl)[-1]

  if (!is.null(data)) {
    vn <- as.character(arguments$x)[-1]
  } else {
    vn <- as.character(arguments$x)[-1]
    data <- data.frame(x)
    names(data) <- vn
  }

  if (length(lbl) != length(vn)) stop(" ... lists must have the same length ... ")

  for (i in 1:length(vn)) {
    attr(data[, vn[i]], "label") <- lbl[i]
    cat(paste0("\tvariable: labelled '", vn[i], "' as '",
               lbl[i], "' ... \n"))
  }
  f <- data

  invisible(f)
}

#' @rdname labelVars
#' @export
labelVars.data.frame <- function(x, lbl = NULL, data = NULL)
{
  warning(paste0(" ... Variable labels cannot be used in data frame ... \n",
             "   ... Try labelData() ... "))
}

#' @rdname labelVars
#' @export
labelData <- function(x, lbl = NULL)
{
  if (!is.data.frame(x)) stop(" ... x must be a dataframe ... ")
  x.name <- deparse(substitute(x))
  f <- x
  attr(f, "label") <- lbl
  cat(paste0("\tdataframe: labelled '", x.name, "' as '",
             lbl, "' ... \n"))
  invisible(f)
}
