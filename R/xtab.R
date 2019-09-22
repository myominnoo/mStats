#' @title Cross tabulation, contigency table or two-by-two table
#' @description
#' \code{xtab} generates cross tabulation of two variables.
#' @param x a character, factor or logical object OR list OR data frame
#' @param y a character, factor or logical object
#' @param data a data frame object (Optional)
#' @param row.pct a logical value: if NULL, a default table without any percentages
#' is produced. If TRUE, row percentages are shown and if FALSE, column percentages.
#' @param na.rm A logical value to specify missing values, <NA> in the table
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#' @param ... optional arguments
#' @details
#' Exploring data before jumping into complex analysis is always a necessity.
#' The first step of an analysis is always to summarize and display data.
#'
#' \code{xtab}
#' produce contigency table.
#'
#' \strong{References:}
#' \enumerate{
#'   \item Essential Medical Statistics, Betty R. Kirkwood & Jonathan A.C. Sterne,
#'   Second Edition. Chapter 3
#'   \item An Introduction to MEdical Statistics, Martin Bland, Thrid Edition,
#'   Chapter 4
#' }
#'
#' @seealso \code{\link{tab}}
#' @keywords two-by-two table, 2x2 table, two-way table
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' xtab(education, induced, infert)
#' xtab(infert$education, infert$induced)
#'
#' xtab(spontaneous, induced, infert, row.pct = FALSE) # column percentage
#' xtab(spontaneous, induced, infert, row.pct = NULL) # DO NOT SHOW PERCENTAGE
#'
#' # multiple variables
#' xtab(list(spontaneous, case, induced), education, infert)
#'
#' # whole dataset
#' xtab(infert, case)
#' xtab(infert, induced)

#' @export
xtab <- function(x, y, data = NULL, row.pct = TRUE, na.rm = FALSE, rnd = 1)
{
  arguments <- as.list(match.call())
  if (!is.null(data)) {
    if (as.character(arguments$x)[1] %in% c("list"))
      x <- list(as.character(arguments$x)[-1]) else
        x <- as.character(eval(arguments$x, data))
  } else {
    if (is.factor(x) | is.logical(x) | is.numeric(x))
      x <- as.character(arguments$x)
  }
  UseMethod("xtab", x)
}

#' @rdname xtab
#' @export
xtab.default <- function(...) {
  warning(' ... Wrong Data Type ... ')
}

#' @rdname xtab
#' @export
xtab.character <- function(x, y, data = NULL, row.pct = TRUE, na.rm = FALSE, rnd = 1)
{
  arguments <- as.list(match.call())
  x.name <- deparse(substitute(x))
  y.name <- deparse(substitute(y))
  if (!is.null(data)) {
    x <- eval(substitute(x), data)
    y <- eval(substitute(y), data)
  }
  na.rm <- ifelse(na.rm, "no", "ifany")
  row.pct <- ifelse(is.null(row.pct), "none",
                    ifelse(row.pct, "row",
                           ifelse(!row.pct, "column", NULL)))

  # get tables
  t <- table(x, y, useNA = na.rm)
  t.c <- rbind(t, Total = colSums(t))
  t.r <- cbind(t, Total = rowSums(t))
  t.f <- cbind(t.c, Total = rowSums(t.c))

  p.r <- round(t.f / rowSums(t.c) * 100, rnd)
  p.c <- round(t(t(t.f) / colSums(t.r)) * 100, rnd)

  t.c.p <- NULL; t.r.p <- NULL
  n.c.p <- NULL; n.r.p <- NULL # names for headers
  for (i in seq_len(ncol(t.f)))
  {
    t.c.p <- cbind(t.c.p, cbind(t.f[,i], p.c[,i]))
    n.c.p <- c(n.c.p, c(colnames(t.f)[i], "(c%)"))
    t.r.p <- cbind(t.r.p, cbind(t.f[,i], p.r[,i]))
    n.r.p <- c(n.r.p, c(colnames(t.f)[i], "(r%)"))
  }
  colnames(t.c.p) <- n.c.p
  colnames(t.r.p) <- n.r.p
  names(attributes(t.f)$dimnames) <- c(x.name, y.name)
  names(attributes(t.c.p)$dimnames) <- c(x.name, y.name)
  names(attributes(t.r.p)$dimnames) <- c(x.name, y.name)

  f <- switch(row.pct,
              none = t.f,
              row = t.r.p,
              column = t.c.p)

  if (na.rm == "no") {
    data <- data.frame(cbind(x = x, y = y))
    data <- na.omit(data)
    x <- data$x
    y <- data$y
  }

  pvalue <- tryCatch({
    suppressWarnings(chisq.test(x, y, correct = FALSE)$p.value)
  }, error = function(err) {
    return(NA)
  })
  pvalue <- c(
    pvalue,
    tryCatch({
      suppressWarnings(fisher.test(x, y, simulate.p.value = FALSE)$p.value)
    }, error = function(err) {
      return(NA)
    })
  )

  pvalue <- sprintf(pvalue, fmt = '%#.5f')
  f <- cbind(as.data.frame(f),
             as.data.frame(rbind(pvalue,
                                 matrix(rep("", 2 * (nrow(f) - 1)), ncol = 2))))
  names(f)[(ncol(f)-1):ncol(f)] <- c("Chi.Square", "F.Exact")
  # print formating
  output.format(f, paste0("Cross Tabulation: ", x.name, " ~ ", y.name))

  invisible(f)
}

#' @rdname xtab
#' @export
xtab.list <- function(x, y, data = NULL, row.pct = TRUE, na.rm = FALSE, rnd = 1)
{
  arguments <- as.list(match.call())
  y.name <- deparse(substitute(y))
  if (!is.null(data)) {
    y <- eval(substitute(y), data)
    vn <- as.character(arguments$x)[-1]
    df <- sapply(
      paste(deparse(substitute(data)), "$", vn, sep = ""),
      function(z) eval(parse(text = z))
    )
    data <- data.frame(unlist(df))
    names(data) <- vn
  } else {
    vn <- as.character(arguments$x)[-1]
    data <- data.frame(do.call(cbind, x))
    names(data) <- vn
  }

  sink(tempfile())
  f <- lapply(data, function(z){
    xtab.character(z, y, row.pct = row.pct, na.rm = na.rm, rnd = rnd)
  })
  sink()

  for (i in 1:length(vn)) {
    t <- f[[i]]
    output.format(t, paste0("Cross Tabulation: ", vn[i], " ~ ", y.name))
  }

  invisible(f)
}

#' @rdname xtab
#' @export
xtab.data.frame <- function(x, y, data = NULL, row.pct = TRUE, na.rm = FALSE, rnd = 1)
{
  arguments <- as.list(match.call())
  y <- eval(substitute(y), x)
  y.name <- arguments$y

  data <- as.data.frame(x)
  var.names <- names(data)

  type.factor <- c("factor", "character", "logical")
  type.numeric <- c("integer", "double", "numeric")
  type.date <- c("Date")

  var.type <- sapply(var.names, function(z) class(unlist(x[ , z])))
  var.factor <- names(var.type[!(var.type %in% type.numeric) &
                                 !(var.type %in% type.date)])
  data <- data[, var.factor]
  names.invalid <- grep("^([[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*$",
                        var.factor, value = TRUE, invert = TRUE)
  var.factor[var.factor %in% names.invalid] <- paste0("v", names.invalid)
  names(data) <- var.factor

  if (length(var.factor) == 0) stop(" ... no categorical variables found ... ")

  sink(tempfile())
  if (length(var.factor) > 1) {
    f <- lapply(data, function(z){
      xtab.character(z, y, row.pct = row.pct, na.rm = na.rm, rnd = rnd)
    })
  } else {
    f <- xtab.character(data, y, row.pct = row.pct, na.rm = na.rm, rnd = rnd)
  }
  sink()

  if (length(var.factor) > 1) {
    for (i in 1:length(var.factor)) {
      t <- f[[i]]
      output.format(t, paste0("Cross Tabulation: ", var.factor[i], " ~ ", y.name))
    }
  } else {
    output.format(f, paste0("Cross Tabulation: ", var.factor, " ~ ", y.name))
  }

  invisible(f)
}


