#' Convert a continuous variable into groups
#'
#' @description
#'
#' \Sexpr[results=rd]{lifecycle::badge("stable")}
#'
#' `egen()` categorize a numerical variable into a factor.
#' If only a number is specified in `cut`, it categorizes
#' into equal intervals based on that number. If no value is set
#' for `cut`, the default interval is `10`.
#'
#' If `new_var` is not specified, a new name is automatically
#' assigned by appending `_cat` to the original name: `VARNAME_cat`
#' If `label` is not specified, a set of labels are automatically
#' constructed in the format, `##-##`.
#'
#'
#' @inheritParams replace
#' @param cut either a number or a numeric vector
#' @param label Labels for the groups
#' @param new_var Name of the new variable
#'
#' @family data management
#' @export
#' @section Examples:
#'
#' Here is how we can use `egen`.
#' ```
#' infert <- egen(infert, age, c(26, 31, 36, 41))
#' table(infert$age_cat)
#'
#'
#' ## Add labels and give a new name
#' infert <- egen(infert, age, cut = c(26, 31, 36, 41),
#'                label = c("<=25", "26-30", "31-35", "36-40", "41+"),
#'                new_var = "age_grp")
#' table(infert$age_grp)
#' ```
egen <- function(data, var, cut = NULL, label = NULL, new_var = NULL) {
  data_name <- deparse(substitute(data))
  .check_dataframe(data, data_name)

  args      <- as.list(match.call())
  var_name  <- as.character(args$var)
  .check_vars(data, var_name)

  # ## if variable already exisits, then stop
  if (is.null(new_var)) {
    new_var <- paste0(var_name, "_cat")
  } else {
    if (new_var %in% names(data)) {
      stop(paste0("'", new_var, "' already defined."), call. = FALSE)
    }
  }

  var <- data[[var_name]]
  ## create break / cut-off labels
  brk <- cut
  if (is.null(cut)) {
    ## if cut is not specified, cut <- 10
    if (is.null(cut)) cut <- 10L
    brk <- seq(min(var, na.rm = TRUE), max(var, na.rm = TRUE), cut)
    if (length(brk) == 1) {
      stop(paste0("cut = ", cut, " is too large to create numeric intervals."),
           call. = FALSE)
    } else {
      brk <- c(brk[1], brk[2:(length(brk)-1)] - 1, brk[length(brk)] - 1)
    }
  }

  ## get minimum value, check and add to the cut sequence
  brk.min <- min(var, na.rm = TRUE)
  brk <- brk[brk.min < brk]
  if (brk.min != brk[1]) {
    brk <- c(brk.min, brk)
  }


  ## get minimum value, check and add to the cut sequence
  brk.max <- max(var, na.rm = TRUE)
  brk <- brk[brk.max > brk]
  if (brk.max != brk[length(brk)]) {
    brk <- c(brk, ceiling(brk.max))
  }

  ## remove duplicated category
  brk <- brk[!duplicated(brk)]

  ## check decimals
  check_decimals <- function(x)
  {
    ## check if there are any decimal values
    decimal <- grepl("\\.", x)
    if (any(decimal)) {
      decimal <- strsplit(as.character(x[decimal][1]),
                          "\\.")[[1]][2]
      decimal <- nchar(decimal)
    } else {
      decimal <- 0
    }
    decimal
  }
  decimal <- check_decimals(cut)

  ## change numbers to decimal values
  brk <- c(floor(brk[1]),
           round(brk[-c(1, length(brk))], decimal),
           ceiling(brk[length(brk)]))

  ## construct labels
  if (is.null(label)) {
    last_sec_pos <- length(brk) - 1
    first_vec    <- brk[1:last_sec_pos]
    second_vec   <- c(brk[2:last_sec_pos] - (1 / (10 ^ decimal)), brk[length(brk)])
    if (decimal > 0) {
      first_vec    <- sprintf(first_vec, fmt = paste0("%#.", decimal, "f"))
      second_vec   <- sprintf(second_vec, fmt = paste0("%#.", decimal, "f"))
    }
    label <- paste(first_vec, "-", second_vec, sep = "")
  }

  ## create new var
  new_var_name <- new_var
  new_var      <- cut(var, breaks = as.numeric(brk), labels = label,
                      right = FALSE, include.lowest = TRUE)
  attr(new_var, "label") <- paste0("Categories of ", var_name)
  data$new_var <- new_var
  names(data)[ncol(data)] <- new_var_name

  check <- is.na(new_var)
  na_num <- ifelse(any(check), paste0("& ", sum(check), " missing "), "")
  message("  (", sum(!check), " valid ", na_num, "values generated in '",
          new_var_name, "')")

  return(data)
}
