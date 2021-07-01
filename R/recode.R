#' Recode variables
#'
#' @description
#'
#' \Sexpr[results=rd]{lifecycle::badge("stable")}
#'
#' `recode()` changes the values of numeric variables
#' according to the values specified.
#' Values that does not meet any of the conditions, they are left unchanged.
#' Operator `:` can be used as `x:y` to indicate a range of numeric numbers.
#'
#' @inheritParams codebook
#' @param vars Variable to be recoded.
#' `list()` can be used to recode multiple
#' variables.
#' @param ... Old and new values to be recoded: `old_value`/`new_value`
#'
#' @section Examples:
#'
#' ## Recode one variable at a time.
#'
#' ```
#' infert <- recode(infert, case, 0/"No", 1/"Yes")
#' tab(infert, case)
#' ```
#'
#' ## Recode multiple variables at the same time, using `list`.
#' ```
#' infert <- recode(infert, list(parity, induced), 1:2/1)
#' tab(infert, parity, induced)
#' ```
#'
#' ## Recode a range of numeric values using operator `:`.
#'
#' ```
#' infert <- recode(infert, age, 21:28.9/1, 29:34.9/2, 35:44/3)
#' tab(infert, age)
#' ```
#'
#' ## Recode value to NA or NA to a new value.
#'
#' ```
#' infert <- recode(infert, parity, 4:6/NA)
#' tab(infert, parity)
#' ```
#'
#' ## Recode `NA` back to 4
#' ```
#' infert <- recode(infert, parity, NA/4)
#' tab(infert, parity)
#' ```
#'
#' ## Using `mvdecode` and `mvrecode`
#' ```
#' ## Example from A Gentle Introduction to STATA 4th Edition
#' ## Chapter 3 Preparing data for analysis, Page 54
#' relate <- haven::read_dta("http://www.stata-press.com/data/agis4/relate.dta")
#'
#' ## recode all values from -5 to -1 into NA
#' relate <- mvdecode(relate, -5:-1)
#'
#' ## recode multiple values of multiple variables
#' related <- recode(related, list(R3483700, R3483900, R3485300, R3485500),
#'                   0/4, 1/3, 2/2, 3/1, 4/0
#'
#' ## recode NA to 99
#' relate <- mvrecode(relate, 99)
#' ```
#'
#'
#' @family data management
#' @export
recode <- function(data, vars = list(), ... ) {
  data_name <- deparse(substitute(data))
  .check_dataframe(data, data_name)

  args      <- as.list(match.call())
  vars <- as.character(args$vars)
  vars_len  <- length(vars)
  if (vars_len > 1) {
    vars <- vars[-1]
  } else if (vars_len < 1) {
    stop("Specify one variable.", call. = FALSE)
  }

  lapply(vars, function(var_name) {
    .check_vars(data, var_name)
    var <- data[[var_name]]
    label <- attr(var, "label")

    ## change double to numeric | factor to character
    if (is.double(var)) {
      var <- as.numeric(var)
    } else if (is.factor(var)) {
      var <- as.character(var)
    }

    vals <- do.call(
      rbind,
      lapply(args[-c(1:3)], function(val) {
        val <- as.character(val)[-1]
        old <- ifelse(val[1] == "NA", NA, val[1])
        new <- ifelse(val[2] == "NA", NA, ifelse(val[2] == "NULL", NULL, val[2]))

        if (is.numeric(var)) {
          old_test <- tryCatch({
            old <- eval(parse(text = old))
          }, error = function(cond){
            stop("  <`", var_name, "`: `", old,
                    "` cannot be recoded into `",
                    new, "` ... skipping ... >", call. = FALSE)
            return(NA)
          })
          old <- suppressWarnings(as.numeric(old))

          new_test <- tryCatch({
            eval(parse(text = new))
          }, error = function(cond){
            return("error")
          })
          if (!identical(new_test, "error", single.NA = TRUE)) {
            new <- eval(parse(text = new))
            new <- suppressWarnings(as.numeric(new))
          }
          if (length(old) > 1) new <- rep(new, length(old))

          # # if values are the same or both are NA
          chk <- mapply(identical, old, new,
                        MoreArgs = list(num.eq = TRUE, single.NA = TRUE),
                        SIMPLIFY = TRUE)
          old <- old[!chk]
          new <- new[!chk]
        }
        cbind(old, new)
      })
    )

    check <- apply(vals, 1, function(vals) {
      old <- vals[1]
      new <- vals[2]
      if (is.na(old)) {
        chk <- which(is.na(var))
      } else {
        chk <- which(var == old)
      }
      attr(chk, "label") <- c(old, new)
      chk
    }, simplify = FALSE)

    lapply(check, function(chk) {
      vals <- attr(chk, "label")
      old <- vals[1]
      new <- vals[2]
      var[chk] <<- new
      message("  (`", var_name, "`: ", length(var[chk]),
              " values recoded - `", old, "` >>> `", new, "`)")
    })

    if (is.factor(data[[var_name]])) {
      var <- factor(var)
    }
    attr(var, "label") <- label
    data[[var_name]] <<- var
  })

  return(data)
}




#' @description
#'
#' `mvrecode()` converts all missing values `NA` in the data.frame to
#' a specified value.
#'
#' @param value a value to be recoded for missing values in the data.frame
#' @rdname recode
#' @export
mvrecode <- function(data, value) {
  data_name <- deparse(substitute(data))
  .check_dataframe(data, data_name)

  lapply(names(data), function(var_name) {
    chk <- is.na(data[[var_name]])
    data[[var_name]][chk] <<- value
    changed <- sum(chk)
    if (changed > 0) {
      message("  (`", var_name, "`: ", changed, " missing values recoded to `",
              value, "`)")
    }
  })

  return(data)
}




#' @description
#'
#' `mvdecode()` converts all coded missing values in the data.frame to `NA`.
#'
#' @rdname recode
#' @export
mvdecode <- function(data, ... ) {
  data_name <- deparse(substitute(data))
  .check_dataframe(data, data_name)

  args <- as.list(match.call())
  vals <- do.call(c, lapply(args[-c(1:2)], eval))

  lapply(names(data), function(var_name) {
    chk <- data[[var_name]] %in% vals
    data[[var_name]][chk] <<- NA
    changed <- sum(chk)
    if (changed > 0) {
      message("  (`", var_name, "`: ", changed, " missing values decoded)")
    }
  })

  return(data)
}
