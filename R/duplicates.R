#' @title Tag, report, delete or keep duplicate observations
#' @description
#' \code{duplicates} displays structure of a data frame
#' @param data dataframe
#' @param print.table logical value to display formatted outputs
#' @param ... any variables within dataframe for unique id
#' @details
#' \code{duplicates}
#'
#' tags duplicate observations within dataframe with a new variable called
#' \code{dupID_} and reports statistics.
#' Duplicates are observations with identical values either on all variables
#' if no variable is specified in the optional argument \code{...}
#' or on a specified list of variables.
#'
#' ANNOTATIONS:
#'
#' Copies       - Number of duplicates
#'
#' Observations - Number of records per Copies
#'
#' Surplus  - Number of surplus copies
#'
#' \code{keepUnique}
#'
#' delete all but the first occurrence of each group of duplicated observations.
#'
#' \code{keepDup}
#' keep all but the first occurrence of each group of duplicated observations.
#' This function returns the opposite dataset generated from \code{keepUnique}.
#'
#' @seealso \code{\link{keep}}, \code{\link{lose}}
#' @keywords duplicates, report, distinct, unique
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#' # finding duplicates across all variables
#' duplicates(iris)
#'
#' # finding duplicates on variables of interest
#' duplicates(iris, Sepal.Length, Sepal.Width)
#' duplicates(iris, Species)
#' duplicates(iris, Sepal.Length, Sepal.Width, print.table = FALSE)
#'
#' # Keep Unique records
#' keepUnique(iris, Sepal.Length)
#' keepUnique(iris, Species)
#' keepUnique(infert, case)
#'
#' # Keep duplicated records (opposite of keep unique records)
#' keepDup(iris, Sepal.Length)
#' keepDup(iris, Species)
#' keepDup(infert, case)
#' }

#' @export
duplicates <- function(data, ... , print.table = TRUE)
{
  data <- data
  arguments <- as.list(match.call())
  lastArg <- grep("^TRUE$|^FALSE$|^T$|^F$", arguments)
  id <- arguments[-c(1,2, ifelse(length(lastArg) == 0, 1, lastArg))]
  id <- gsub(" ", "", id)

  if (any(names(data) %in% "dupID_"))
    data <- data[, !(names(data) %in% "dupID_")]

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
      dupID_ <- ave(1:nrow(data), data[, id], FUN = seq_along)
    } else {
      texts <- paste0("data[with(data, order(", id,
                      ")), ]", collapse = "")
      data <- eval(parse(text = texts))
      dupID_ <- ave(1:nrow(data), data[, id], FUN = seq_along)
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
                        "|", surplus = c(0, t.constant_[-1]), "|"))
  colnames(t) <- c("+", "Copies", "+", "Observations", "+",
                   "Surplus", "+")

  #### printing
  if (print.table) {
    printText(t, paste0("Duplicates in terms of ",
                        ifelse(length(id) == 0, "all variables",
                               paste0(id, collapse = " + "))))
    printMsg(paste0("Note:"))
    printMsg(paste0("Total No. of obs: ", nrow(data)))
  }
  invisible(f)
}



#' @rdname duplicates
#' @export
keepUnique <- function(data, ... , print.table = TRUE)
{
  arguments <- as.list(match.call())
  lastArg <- grep("^TRUE$|^FALSE$|^T$|^F$", arguments)
  id <- arguments[-c(1,2, ifelse(length(lastArg) == 0, 1, lastArg))]
  id <- gsub(" ", "", id)
  data.nrow <- nrow(data)

  if (any(names(data) %in% "dupID_"))
    data <- data[, !(names(data) %in% "dupID_")]

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
      dupID_ <- ave(1:nrow(data), data[, id], FUN = seq_along)
    } else {
      texts <- paste0("data[with(data, order(", id,
                      ")), ]", collapse = "")
      data <- eval(parse(text = texts))
      dupID_ <- ave(1:nrow(data), data[, id], FUN = seq_along)
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
                        "|", surplus = c(0, t.constant_[-1]), "|"))
  colnames(t) <- c("+", "Copies", "+", "Observations", "+",
                   "Surplus", "+")

  f <- f[f$dupID_ == 1, ]
  f <- f[, -ncol(f)]
  #### printing
  if (print.table) {
    printText(t, paste0("Duplicates in terms of ",
                        ifelse(length(id) == 0, "all variables",
                               paste0(id, collapse = " + "))))
    printMsg(paste0("Note:"))
    printMsg(paste0("Total No. of obs: ", nrow(data)))
    printMsg(paste0(data.nrow - nrow(f), " observations deleted"))
  }
  invisible(f)
}



#' @rdname duplicates
#' @export
keepDup <- function(data, ... , print.table = TRUE)
{
  arguments <- as.list(match.call())
  lastArg <- grep("^TRUE$|^FALSE$|^T$|^F$", arguments)
  id <- arguments[-c(1,2, ifelse(length(lastArg) == 0, 1, lastArg))]
  id <- gsub(" ", "", id)
  data.nrow <- nrow(data)

  if (any(names(data) %in% "dupID_"))
    data <- data[, !(names(data) %in% "dupID_")]

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
      dupID_ <- ave(1:nrow(data), data[, id], FUN = seq_along)
    } else {
      texts <- paste0("data[with(data, order(", id,
                      ")), ]", collapse = "")
      data <- eval(parse(text = texts))
      dupID_ <- ave(1:nrow(data), data[, id], FUN = seq_along)
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
                        "|", surplus = c(0, t.constant_[-1]), "|"))
  colnames(t) <- c("+", "Copies", "+", "Observations", "+",
                   "Surplus", "+")

  f <- f[f$dupID_ != 1, ]
  f <- f[, -ncol(f)]
  #### printing
  if (print.table) {
    printText(t, paste0("Duplicates in terms of ",
                        ifelse(length(id) == 0, "all variables",
                               paste0(id, collapse = " + "))))
    printMsg(paste0("Note:"))
    printMsg(paste0("Total No. of obs: ", nrow(data)))
    printMsg(paste0(data.nrow - nrow(f), " observations deleted"))
  }
  invisible(f)
}
