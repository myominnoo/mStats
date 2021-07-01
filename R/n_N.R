#' Count from `n_` to `N_`
#'
#' @description
#'
#' \Sexpr[results=rd]{lifecycle::badge("stable")}
#'
#' `n_()`, `N_()` and `n_all()` are  for indexing observations
#' or generating sequences of numbers.
#'
#' * `n_()` generates a running counter within a group of variables, and
#' contains the number of the current observation.
#' * `N_()` provide the total number within each group of variables.
#' * `n_()` generates a sequence from 1 to number of observations in the
#' dataset.
#'
#' @param ... One or more unquoted variables separated by commas. `x:y` pattern
#' cannot be used.
#'
#' @importFrom stats ave
#'
#' @family data management
#' @export
#' @section Examples:
#'
#' Here we show an example adopted from IRDE website.
#'
#' ```
#' ## Example adapted from
#' ## https://stats.idre.ucla.edu/stata/seminars/notes/counting-from-_n-to-_n/
#'
#' ex <- data.frame(rbind(c(72, 1), c(84, 2), c(76, 1), c(89, 3), c(82, 2),
#'                        c(90, 1), c(85, 1)))
#' names(ex) <- c("score", "group")
#'
#' ex <- generate(ex, id_n, n_(group))
#' ex <- generate(ex, id_N, N_(group))
#' ex <- generate(ex, id_n_all, n_all())
#' ex
#' ```
#'
#' To counter within all variables in the dataset, you can leave the varibles
#' unspecified. Alternatively, `x:y` pattern can be used.
#' ```
#' iris <- generate(iris, id_n, n_())
#' iris <- generate(iris, id_N, N_())
#' iris <- generate(iris, id_n_all, n_all())
#' head(iris)
#' ```
#'
n_ <- function(...) {
  vars <- list(...)
  args <- as.character(match.call())[-1]
  if (length(vars) == 0) {
    vars <- ls(envir = sys.frame(-1))
    vars <- sapply(sapply(vars, as.symbol), eval, envir = sys.frame(-1))
    txt  <- "all variables"
  } else {
    txt  <- paste(args, collapse = ", ")
  }
  vars <- as.data.frame(vars, col.names = args)

  id   <- apply(vars, 1, paste, collapse = " ")
  n_   <- ave(id, id, FUN = seq_along)
  n_   <- as.numeric(n_)

  attr(n_, "label") <- paste0("n_(", txt, ")")

  return(n_)
}


#' @rdname n_
#' @export
N_ <- function(...) {
  vars <- list(...)
  args <- as.character(match.call())[-1]
  if (length(vars) == 0) {
    vars <- ls(envir = sys.frame(-1))
    vars <- sapply(sapply(vars, as.symbol), eval, envir = sys.frame(-1))
    txt  <- "all variables"
  } else {
    txt  <- paste(args, collapse = ", ")
  }
  vars <- as.data.frame(vars, col.names = args)

  id   <- apply(vars, 1, paste, collapse = " ")
  N_   <- ave(id, id, FUN = seq_along)
  N_   <- as.numeric(N_)
  N_   <- sapply(id, function(z) {
    t <- N_[z == id]
    t[length(t)]
  })

  ## assign the id back to the original dataset
  N_ <- as.numeric(N_)
  attr(N_, "label") <-  paste0("N_(", txt, ")")

  return(N_)
}



#' @rdname n_
#' @export
n_all <- function() {
  vars <- ls(envir = sys.frame(-1))[1]
  vars <- eval(as.symbol(vars), envir = sys.frame(-1))
  out  <- 1:length(vars)

  return(out)
}
