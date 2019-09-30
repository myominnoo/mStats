#' @title Tag, report, delete or keep duplicate observations
#' @description
#' \code{duplicates} displays structure of a data frame
#' @param data dataframe
#' @param ... any variables within dataframe for unique id
#' @details
#' \code{duplicates}
#'
#' tags duplicate observations within dataframe with a new variable called
#' \code{dupID_} and reports statistics.
#' Duplicates are observations with identical values either on all variables
#' if no variable is specified in the optional argument \code{...}
#' or on a specified list of variable
#'
#' ANNOTATIONS:
#'
#' Copies       - Number of duplicates
#' Observations - Count of duplicate observations
#' dupID Count  - Count of observations by dupID
#'
#' \code{keepUnique}
#'
#' delete all but the first occurrence of each group of duplicated observations.
#'
#' \code{keepNonUnique}
#' keep all but the first occurrence of each group of duplicated observations.
#' This function returns the opposite dataset generated from \code{keepUnique}.
#'
#' @seealso \code{\link{keep}}, \code{\link{lose}}
#' @keywords duplicates, report
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#' duplicates(infert)
#' duplicates(iris)
#'
#' keepUnique(infert)
#' keepUnique(infert, education, age)
#' keepUnique(iris)
#'
#' keepNonUnique(infert)
#' keepNonUnique(infert, education, age)
#' keepNonUnique(iris)
#' }

#' @export
duplicates <- function(data, ... )
{
  data <- data
  arguments <- as.list(match.call())
  id <- as.character(arguments[c(-1,-2)])
  id <- gsub(" ", "", id)

  if (length(id) == 0) {
    texts <- paste0("data[with(data, order(",
                    paste0(names(data), collapse = ", "),
                    ")), ]", collapse = "")
    data <- eval(parse(text = texts))
    dupID_ <- unlist(lapply(1:nrow(data), function(z) {
      paste0(data[z, ], collapse = "")
    }))
    dupID_ <- ave(dupID_, dupID_, FUN = seq_along)
  } else {
    if (length(id) > 1) {
      texts <- paste0("data[with(data, order(",
                      paste0(id, collapse = ", "),
                      ")), ]", collapse = "")
      data <- eval(parse(text = texts))
      t <- data[, id]
      dupID_ <- unlist(lapply(1:nrow(t), function(z) {
        paste0(t[z, ], collapse = "")
      }))
      dupID_ <- ave(dupID_, dupID_, FUN = seq_along)
    } else {
      texts <- paste0("data[with(data, order(", id,
                      ")), ]", collapse = "")
      data <- eval(parse(text = texts))
      dupID_ <- ave(data[, id], data[, id], FUN = seq_along)
    }
  }

  f <- cbind(data, dupID_ = as.numeric(dupID_))
  row.names(f) <- 1:nrow(f)

  t <- table(f$dupID_)
  t.counts <- as.numeric(t)
  t.constant_ <- NULL
  for (i in 1:(length(t.counts) - 1)) {
    t.constant_ <- c(t.constant_, t.counts[i] - t.counts[i + 1])
  }
  t.constant_ <- c(t.constant_, t.counts[length(t.counts)])
  t.dup.num <- 1:length(t.constant_) * t.constant_
  t <- data.frame(cbind("|", copies = 1:length(t.dup.num), "|",
                        observations = t.dup.num,
                        "|", surplus = t.counts, "|"))
  colnames(t) <- c("+", "Copies", "+", "Observations", "+", "dupID Count", "+")

  #### printing
  printText(t, paste0("Duplicates in terms of ",
                      ifelse(length(id) == 0, "all variables",
                             paste0(id, collapse = " + "))))
  printMsg(paste0("Note:"))
  printMsg(paste0("Total No. of obs: ", nrow(data)))
  return(f)
}

#' @rdname duplicates
#' @export
keepUnique <- function(data, ... )
{
  arguments <- as.list(match.call())
  id <- as.character(arguments[c(-1,-2)])
  id <- gsub(" ", "", id)
  vars <- names(data)
  data.nrow <- nrow(data)

  if (length(id) == 0) {
    t <- duplicates(data)
  } else {
    t <- data[, id]
    t <- duplicates(t)
    t <- cbind(data, dupID_ = t[, ncol(t)])
  }

  f <- t[t$dupID_ == 1, ]
  f <- f[, -ncol(f)]

  printMsg(paste0(data.nrow - nrow(f), " observations deleted"))
  return(f)
}

#' @rdname duplicates
#' @export
keepNonUnique <- function(data, ... )
{
  arguments <- as.list(match.call())
  id <- as.character(arguments[c(-1,-2)])
  id <- gsub(" ", "", id)
  vars <- names(data)
  data.nrow <- nrow(data)

  if (length(id) == 0) {
    t <- duplicates(data)
  } else {
    t <- data[, id]
    t <- duplicates(t)
    t <- cbind(data, dupID_ = t[, ncol(t)])
  }

  f <- t[t$dupID_ != 1, ]
  f <- f[, -ncol(f)]

  printMsg(paste0(data.nrow - nrow(f), " observations deleted"))
  return(f)
}
