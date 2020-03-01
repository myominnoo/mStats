#' @title Tag, report, delete or keep duplicated observations
#'
#' @description
#' \code{duplicates} reports numbers of duplicated observations
#'
#' @param data dataframe
#' @param ... any variables within dataframe for unique id
#' @param print.table logical value to display formatted outputs
#'
#' @details
#' \code{duplicates} tags duplicate observations within dataframe
#' with a new variable called \code{dupID_} and reports statistics.
#' Duplicates are observations with identical values either on
#' all variables
#' if no variable is specified in the optional argument \code{...}
#' or on a specified list of variables.
#'
#' \strong{ANNOTATIONS}:
#'
#' \code{Copies}       - Number of duplicates
#'
#' \code{Observations} - Number of records per Copies
#'
#' \code{Surplus}  - Number of surplus copies
#'
#' \code{dupID_} - indicates copies within the dataset. Example, 1 indicates
#' that there is no duplicates, 2 = two identical record, 3 = three records,
#' so on.
#'
#' \code{keepUnique} delete all but the first occurrence of each group of
#' duplicated observations.
#'
#' \code{keepDup} keep all but the first occurrence of each group of
#' duplicated observations. This function returns the opposite dataset
#' generated from \code{keepUnique}
#'
#' @return
#'
#' \code{data.frame}: original dataset plus \code{dupID_}
#'
#' \code{formated_table}: display information about duplicated observations
#'
#' @seealso
#'
#' \code{\link{expand2}}, \code{\link{keep}}
#'
#' @keywords duplicates, report, distinct, unique
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
#' \dontrun{
#' # finding duplicates across all variables
#' iris_dup <- duplicates(iris)
#' str(iris_dup)
#'
#' # finding duplicates on variables of interest
#' iris_dup <- duplicates(iris, Sepal.Length, Sepal.Width)
#' str(iris_dup)
#'
#' iris_dup <- duplicates(iris, Species)
#' str(iris_dup)
#'
#' iris_dup <- duplicates(iris, Sepal.Length, Sepal.Width, print.table = FALSE)
#' str(iris_dup)
#'
#' # Keep Unique records
#' iris_dup <- keepUnique(iris, Sepal.Length)
#' str(iris_dup)
#'
#' iris_dup <- keepUnique(iris, Species)
#' str(iris_dup)
#'
#' iris_dup <- keepUnique(infert, case)
#' str(iris_dup)
#'
#'
#' # Keep duplicated records (opposite of keep unique records)
#' iris_dup <- keepDup(iris, Sepal.Length)
#' str(iris_dup)
#'
#' iris_dup <- keepDup(iris, Species)
#' str(iris_dup)
#'
#' iris_dup <- keepDup(infert, case)
#' str(iris_dup)
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
