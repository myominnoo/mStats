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
#' @keywords two-by-two table, 2x2 table, two-way table, statistics, descriptive
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
#' xtab(c(spontaneous, case, induced), education, infert)
#'
#' \dontrun{
#' # variables' labels as footnote
#' infert.new <- labelVars(infert, c(spontaneous, case, induced),
#'                c("SPONTANEOUS", "CASE YES OR NO", "INDUCED"))
#' xtab(c(spontaneous, case, induced), education, infert.new)
#' }
#'
#' # whole dataset
#' xtab(infert, case)
#' xtab(infert, induced)


#' @export
xtab <- function(x, y, data = NULL, row.pct = TRUE, na.rm = FALSE, rnd = 1)
{
  arguments <- as.list(match.call())
  x.name <- (deparse(substitute(x)))
  x.name <- unlist(strsplit(gsub("^c\\(|\\)$", "", x.name), ","))

  catch <- tryCatch(is.data.frame(x), error=function(e) {})
  x <- as.character()
  if (is.null(catch)) catch <- FALSE
  if (catch) x <- data.frame()
  if (length(x.name) > 1) x <- list()
  UseMethod("xtab", x)
}


#' @rdname xtab
#' @export
xtab.default <- function(...) {
  stop("... Wrong Data Type ...")
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

  texts <- paste0("Tabulation: ", x.name, " ~ ", y.name, collapse = "")
  printText(f, texts)

  if (!is.null(attr(x, "label")) | !is.null(attr(y, "label"))) {
    printMsg("Labels:")
    printMsg(paste0(x.name, ": ", attr(x, "label"), collapse = ""))
    printMsg(paste0(y.name, ": ", attr(y, "label"), collapse = ""))
  }

  invisible(f)
}



#' @rdname xtab
#' @export
xtab.list <- function(x, y, data = NULL, row.pct = TRUE, na.rm = FALSE, rnd = 1)
{
  arguments <- as.list(match.call())
  y.name <- deparse(substitute(y))

  if (is.null(data)) {
    x.names <- as.character(arguments$x)[-1]
    data <- data.frame(do.call(cbind, x))
    names(data) <- x.names
  } else {
    y <- eval(substitute(y), data)
    x.names <- as.character(arguments$x)[-1]
    data <- data[, x.names]
  }

  sink(tempfile())
  f <- lapply(data, function(z){
    xtab.character(z, y, row.pct = row.pct, na.rm = na.rm, rnd = rnd)
  })
  sink()

  x.lbl <- sapply(data, function(z) attr(z, "label"))
  y.lbl <- attr(y, "label")

  for (i in 1:length(x.names)) {
    t <- f[[i]]
    texts <- paste0("Tabulation: ", x.names[i], " ~ ", y.name, collapse = "")
    printText(t, texts)
    if (!is.null(unlist(x.lbl[i]))) {
      printMsg("Labels:")
      printMsg(paste0(x.names[i], ": ", x.lbl[i], collapse = ""))
    }
    if (!is.null(y.lbl))
      printMsg(paste0(y.name, ": ", y.lbl, collapse = ""))
  }

  invisible(f)
}


#' @rdname xtab
#' @export
xtab.data.frame <- function(x, y, data = NULL, row.pct = TRUE,
                            na.rm = FALSE, rnd = 1)
{
  data <- x
  vars <- names(x)
  y.name <- deparse(substitute(y))
  y <- eval(substitute(y), x)

  type.character <- c("factor", "character")
  type.logical <- c("logical")

  vars.type <- sapply(vars, function(z) class(unlist(x[ , z])))
  vars.names <- vars[(vars.type %in% type.character) |
                       (vars.type %in% type.logical)]
  data <- data[, vars.names]

  if (length(vars.names) == 0)
    stop("... no categorical variables found ...")
  names.invalid <- grep("^([[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*$",
                        vars.names, value = TRUE, invert = TRUE)
  if (length(names.invalid) > 0) {
    vars.names[vars.names %in% names.invalid] <- paste0("v", names.invalid)
    names(data) <- vars.names
  }

  sink(tempfile())
  if (length(vars.names) > 1) {
    f <- lapply(data, function(z){
      xtab.character(z, y, row.pct = row.pct, na.rm = na.rm, rnd = rnd)
    })
  } else {
    f <- xtab.character(data, y, row.pct = row.pct, na.rm = na.rm, rnd = rnd)
  }
  sink()


  x.lbl <- sapply(data, function(z) attr(z, "label"))
  y.lbl <- attr(y, "label")

  for (i in 1:length(vars.names)) {
    if (length(vars.names) > 1) {
      t <- f[[i]]
    } else t <- f
    texts <- paste0("Tabulation: ", vars.names[i], " ~ ", y.name, collapse = "")
    printText(t, texts)
    if (!is.null(unlist(x.lbl[i]))) {
      printMsg("Labels:")
      printMsg(paste0(vars.names[i], ": ", x.lbl[i], collapse = ""))
    }
    if (!is.null(y.lbl))
      printMsg(paste0(y.name, ": ", y.lbl, collapse = ""))
  }

  invisible(f)
}
