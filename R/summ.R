#' Summary statistics
#'
#' @description
#'
#' \Sexpr[results=rd]{lifecycle::badge("stable")}
#'
#' `summ()` calculates and displays a variety of univariate summary statistics.
#'
#' * `obs`      - Number of observations
#' * `NA`       - Number of observations with missing value
#' * `mean`     - Mean
#' * `SD`       - Standard deviation
#' * `median`   - Median
#' * `p25`       - First quartile or percentile
#' * `p75`       - Third quartile or percentile
#'
#' ## `detail`: `TRUE`
#'
#' * `min`      - Minimum value
#' * `max`      - Maximum value
#' * `normal`   - p-value from `Shapiro-Wilk Normality Test` using [shapiro.test]
#' * `ttest`   - p-value from `independent t-test` using [t.test]
#' * `wilcox`   - p-value from `Wilcoxon Rank-Sum test` using [wilcox.test]
#' * `anova`    - p-value from Analysis of variance using [aov]
#' * `kwallis` - p-value from `Kruskal-Wallis Rank Sum Test` using [kruskal.test]
#'
#' ## Summarizing the whole dataset
#'
#' This is helpful when the dataset has been processed and finalized.
#' The final dataset can be summarized without specifying any variables.
#' This automatically filters and generates summary statistics
#' for variables that are of type `numeric`, `double`, `integer`, or `logical`.
#'
#' @inheritParams tab
#' @inheritParams stats::t.test
#' @param test name of test
#'
#' * `ttest`   - p-value from `independent t-test` using [t.test]
#' * `wilcox`   - p-value from `Wilcoxon Rank-Sum test` using [wilcox.test]
#' * `anova`    - p-value from Analysis of variance using [aov]
#' * `kwallis` - p-value from `Kruskal-Wallis Rank Sum Test` using [kruskal.test]
#'
#' @param detail logical: `TRUE` displays a full spectrum of
#' summary statistics and includes p-values from hypothesis tests.
#'
#' @importFrom stats aov kruskal.test quantile sd shapiro.test t.test wilcox.test
#' @section Examples:
#'
#' Summarize variable by name:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' summ(iris, Petal.Length)
#' ```
#'
#' Summarize multiple variables by separating them with commas:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' summ(iris, Sepal.Length, Petal.Length)
#' ```
#'
#' The `:` operator selects a range of consecutive variables:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' summ(iris, Sepal.Length:Petal.Length)
#' ```
#'
#' Summarize variables with a stratification variable `by`:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' summ(iris, Sepal.Length:Petal.Length, by = Species)
#' ```
#'
#' Summarize all categorical variables without specifying any variables:
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' summ(iris)
#' summ(iris, by = Species)
#' ```
#'
#' @family statistical analysis
#' @export
summ <- function(data, ... , by = NULL, na.rm = FALSE, test = "auto",
                 paired = FALSE, digits = 1, detail = FALSE) {
  vars_type <- c("numeric", "double", "integer", "logical")
  data_name <- deparse(substitute(data))
  .check_dataframe(data, data_name)

  args      <- as.list(match.call())
  vars_name <- .dots(args, c("data", "by", "na.rm", "test", "paired",
                             "digits", "detail"))
  vars_name <- .check_dots(data, vars_name)

  ## stop if nothing can be found
  if (length(vars_name) == 0) {
    vars_name <- names(data)
    vars_type <- .get_vars_type(data, vars_name) %in% vars_type
    vars_name <- vars_name[vars_type]
    if (length(vars_name) == 0) {
      stop(paste0(
        "'", data_name, "' does not contain variables for summary measures."
      ), call. = FALSE)
    }
  }

  ## Summary measures
  y_name <- as.character(args$by)
  vars_detail <- c("variable", "level", "obs", "NA", "mean",
                   "sd", "median", "p25", "p75")
  if (test != "none")
    vars_detail <- c(vars_detail, c("ttest", "wilcox", "anova", "kwallis"))
  if (length(y_name) == 0) {
    out <- do.call(rbind, lapply(vars_name, summ1, data, na.rm, digits))
    if (!detail) {
      output <- out[, names(out) %in% vars_detail]
    } else {output <- out}
    output <- .add_hv_lines(output, 1, 2)
    message("  Summary statistics")
  } else {
    .check_vars(data, y_name)
    y <- data[[y_name]]
    out <- do.call(
      rbind,
      lapply(vars_name, summ2, y_name, data, na.rm, test, paired, digits, detail)
    )
    if (!detail) {
      output <- out[, names(out) %in% vars_detail]
    } else {output <- out}
    output <- .format_tab(output)
    message(" Summary statistics\n      by levels of `", y_name, "`")
  }

  print.data.frame(output, row.names = FALSE, max = 1e9)
  .print_vars_label(data, c(vars_name, y_name))

  attr(out, "dataname") <- data_name

  invisible(out)
}



# Helper functions --------------------------------------------------------


summ1 <- function(x, data, na.rm, digits) {
  x_name <- x
  x      <- data[[x]]
  obs <- ifelse(na.rm, length(x[!is.na(x)]), length(x))
  na <- length(x[is.na(x)])

  ## construct 7 number summary statistics
  mu  <- mean(x, na.rm = TRUE)
  std <- sd(x, na.rm = TRUE)
  qtl <- quantile(x, probs = c(0, .25, .5, .75, 1), na.rm = TRUE)
  mu  <- sprintf(round(c(mu, std, qtl), digits), fmt = paste0("%#.", digits, "f"))

  ## get p value from normality test
  pvalue <- tryCatch({
    suppressWarnings(shapiro.test(x)$p.value)
  }, error = function(err) {
    return(NA)
  })
  pvalue <- sprintf(pvalue, fmt = '%#.3f')

  out <- data.frame(x_name, obs, na, mu[1], mu[2], mu[5], mu[4],
                    mu[6], mu[3], mu[7], pvalue)
  names(out) <- c("variable", "obs", "NA", "mean", "sd",
                  "median", "p25", "p75", "min", "max", "normal")

  row.names(out) <- NULL
  attr(out, "type") <- "summ1"

  return(out)
}

summ2 <- function(x, y, data, na.rm, test, paired, digits, detail) {
  if (na.rm) data <- na.omit(data[, c(x, y)])

  d <- split(data[, c(x, y)], factor(data[[y]], exclude = NULL))
  e <- do.call(
    rbind,
    lapply(d, function(splt) {
      l <- splt[[y]][1]
      l <- ifelse(is.na(l), "<NA>", l)
      s <- summ1(x, splt, na.rm, digits)
      s$variable <- l
      s
    })
  )
  # if (test != "none")
  e <- rbind(e[0, ], "", e)

  s <- summ1(x, data, na.rm, digits)
  s$variable <- "Total"
  out <- rbind(e, s)
  out <- data.frame(c(x, rep("", nrow(out)-1)), out)
  names(out) <- c("variable", "level", names(s)[-1])

  row.names(out) <- NULL
  attr(out, "type") <- "summ2"

  ## calculate p-value from hypothesis test
  x <- data[[x]]
  y <- data[[y]]
  # y <- factor(data[[y]], exclude = NULL)

  # test can be c("auto", "ttest", "wilcox", "anova", "kwallis")
  test_chk <- test == "auto"
  len_chk  <- length(d) > 2
  test     <- ifelse(
    test_chk,
    ifelse(len_chk, "anova", "ttest"),
    ifelse(len_chk == 3 & any(is.na(names(d))), test,
           ifelse(len_chk & (test %in% c("ttest", "wilcox")),
                  stop("more than 2 groups found, only 2 allowed",
                       call. = FALSE),
                  test))
  )

  pvalue <- switch (
    test,
    none = NULL,
    ttest = tryCatch(
      {suppressWarnings(t.test(x ~ y, paired = paired)$p.value)},
      error = function(cnd) {return(NA)}
    ),
    wilcox = tryCatch(
      {suppressWarnings(suppressWarnings(wilcox.test(x ~ y, paired = paired)$p.value))},
      error = function(cnd) {return(NA)}
    ),
    anova = tryCatch(
      # {suppressWarnings(summary(aov(x ~ y))[[1]][1,5])},
      {suppressWarnings(summary(aov(x ~ y))[[1]]$`Pr(>F)`[1])},
      error = function(cnd) {return(NA)}
    ),
    kwallis = tryCatch(
      {suppressWarnings(kruskal.test(x ~ y)$p.value)},
      error = function(cnd) {return(NA)}
    )
  )

  pvalue <- sprintf(pvalue, fmt = '%#.3f')
  if (test != "none") {
    out$pvalue <- c(pvalue, rep("", nrow(out)-1))
    names(out)[ncol(out)] <- test
  }

  return(out)
}

