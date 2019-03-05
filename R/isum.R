#' @title Number Summary, Correlation, Grouped and Stratified Summary
#' @description
#' \code{isum} generates tables that display univariate, bivariate and stratified
#' summaries. 
#' @param x a numeric object
#' @param y a numeric or factor object
#' @param by a factor object
#' @param data an optional data frame 
#' @param rnd specify rounding of numbers
#' @param na.rm A logical value to specify missing values, <NA> in the table
#' @param plot.show display bar plots of the data
#' @details 
#' Exploring data before jumping into complex analysis is always a necessity. 
#' The first step of an analysis should be to summarize and display data.
#' By doing so, invaluable insight can be gained through the familiarity with the data.
#' 
#' \code{isum} allows to summarize the distribution of data taken by a single variable, 
#' the association between two numeric variables as well as between a numeric variable
#' and a factor variable, and finally these associations stratified by a third factor 
#' variable. 
#' 
#' This function also generate different data visualization for better exploration by 
#' using \code{iboxplot}. The plot display can be switched off by setting 
#' \code{plot.show = FALSE}.
#' 
#' \strong{References:}
#' \enumerate{
#'   \item Essential Medical Statistics, Betty R. Kirkwood & Jonathan A.C. Sterne, 
#'   Second Edition. Chapter 3
#'   \item An Introduction to MEdical Statistics, Martin Bland, Thrid Edition, 
#'   Chapter 4
#' }
#' 
#' @import ggplot2
#' @seealso \code{\link{itab}}, \code{\link{ibarplot}}, \code{\link{strate}}, 
#' @keywords distribution, number summary, correlation
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' data(infert)
#' str(infert)
#' 
#' ## univariate analysis 
#' isum(infert$age)
#' isum(pooled.stratum, data = infert)
#' 
#' ## bivariate: numeric x factor
#' isum(age, education, data = infert)
#' isum(parity, education, data = infert)
#' isum(pooled.stratum, education, data = infert)
#' isum(infert$parity, factor(infert$case))
#' 
#' ## bivariate: numeric x numeric 
#' isum(age, parity, data = infert)
#' isum(age, stratum, data = infert)
#' isum(age, pooled.stratum, data = infert)
#' 
#' ## stratified: numeric x factor | factor 
#' infert$case <- factor(infert$case)
#' isum(age, education, case, data = infert)
#' 
#' ## stratified: numeric x numeric | factor 
#' isum(age, parity, education, data = infert)
#' isum(age, stratum, education, data = infert)

#' @export 
isum <- function(x, y = NULL, by = NULL, data = NULL, rnd = 1, na.rm = TRUE, 
                 plot.show = TRUE)
{
  if (!is.null(data)) {
    arguments <- as.list(match.call())
    x <- eval(substitute(x), data)
    y <- eval(substitute(y), data)
    by <- eval(substitute(by), data)
    
    x.name <- arguments$x
    y.name <- arguments$y
    by.name <- arguments$by
  } else {
    x.name <- deparse(substitute(x))
    if (!is.null(y)) y.name <- deparse(substitute(y))
    if (!is.null(by)) by.name <- deparse(substitute(by))
  }
  
  if (!is.numeric(x)) {
    stop("x must be a numeric object.")
  }
  
  type <- ifelse(is.null(y), "uni", ifelse(is.null(by), "bi", "strata"))
  
  res <- switch(
    type,
    uni = {
      df <- uni.sum(x, rnd, na.rm)
      row.names(df) <- x.name
      cat(paste0('\nSummary: ', x.name, '\n\n'))
      uni <- df
    }, 
    bi = {
      if (is.factor(y) | is.character(y) | is.logical(y)) {
        df <- bi.sum.cat(x, y, rnd, na.rm)
        row.names(df)[1] <- as.character(x.name)
        cat(paste0('\nSummary: ', x.name, ' ~ ', y.name, '\n\n'))
        bi = df
      } else if (is.numeric(y)) {
        df <- bi.sum.num(x, y, rnd, na.rm)
        row.names(df) <- c(x.name, y.name)
        cat(paste0('\nCorrelation: ', x.name, ' ~ ', y.name, '\n\n'))
        bi <- df
      } else {
        message('y is not specified as factor or as number.')
      }
    }, 
    strata = {
      if (na.rm) lvl <- as.character(unique(na.omit(by))) else 
        lvl <- as.character(unique(by))
      lvl <- ifelse(is.na(lvl), "<NA>", lvl)
      df.lvl <- unlist(lapply(lvl, function(z) paste(c(x.name, y.name), z, sep = '_')))
      
      if (is.factor(y) | is.character(y) | is.logical(y)) {
        df <- str.sum.cat(x, y, by, rnd, na.rm)
        for (i in 1:length(lvl)) row.names(df[[lvl[i]]])[1] <- as.character(x.name)
        cat(paste0('\nSummary: ', x.name, ' ~ ', y.name, ' | ', by.name, '\n\n'))
        strata <- df
      } else if (is.numeric(y)) {
        df <- str.sum.num(x, y, by, rnd, na.rm)
        row.names(df)[1:2] <- c(x.name, y.name)
        row.names(df)[-c(1:3)] <- df.lvl
        cat(paste0('\nCorrelation: ', x.name, ' ~ ', y.name, ' | ', by.name, '\n\n'))
        strata <- df
      } else {
        message('y is not specified as factor or as number.')
      } 
    }
  )
  
  if (plot.show) {
    main <- ifelse(is.null(y), paste0('Plot: ', x.name), 
                   ifelse(is.null(by), 
                          paste0('Plot: ', x.name, ' ~ ', y.name), 
                          paste0('Plot: ', x.name, ' ~ ', y.name, 
                                 ' | ', by.name)))
    plot(
      iboxplot(x, y, by, na.rm = na.rm, main = main, xlab = x.name, ylab = y.name,
               legend.text = by.name)
    )
  }
  
  return(res) 
}

uni.sum <- function(x, rnd = 1, na.rm = TRUE) {
  len <- ifelse(na.rm, length(x[!is.na(x)]), length(x))
  na <- ifelse(na.rm, length(x[is.na(x)]), 0)
  na.rm <- TRUE
  mu <- mean(x, na.rm = na.rm)
  std <- sd(x, na.rm = na.rm)
  q <- round(quantile(x, probs = c(0, .25, .5, .75, 1), na.rm = na.rm), rnd)
  v <- round(c(mu, std, q), rnd)
  pvalue <- tryCatch({
    suppressWarnings(shapiro.test(x)$p.value)
  }, error = function(err) {
    return(NA)
  })
  pvalue <- ifelse(pvalue < 0.001, "< 0.001", round(pvalue, 3))
  df <- data.frame(Obs. = len, NA. = na, Mean = v[1], Std.Dev = v[2],
                   Median = v[5], Q_1 = v[4], Q_3 = v[6],
                   Min = v[3], Max = v[7], Normality = pvalue, 
                   stringsAsFactors = FALSE)
  return(df)
}

bi.sum.num <- function(x, y, rnd = 1, na.rm = TRUE) {
  df <- rbind(uni.sum(x, rnd, na.rm), 
              uni.sum(y, rnd, na.rm))
  df <- data.frame(df, Corr = c(htest.pvalue(x, y, test = 'corr'), ''), 
                   stringsAsFactors = FALSE)
  return(df)
}

bi.sum.cat <- function(x, y, rnd = 1, na.rm = TRUE) {
  if (na.rm) lvl <- as.character(unique(na.omit(y))) else 
    lvl <- as.character(unique(y))
  df <- uni.sum(x, rnd, na.rm)
  df.lvl <- do.call(rbind, lapply(lvl, function(z) 
    uni.sum(x[y == z], rnd, na.rm)))
  lvl <- ifelse(is.na(lvl), "<NA>", lvl)
  row.names(df.lvl) <- lvl
  df <- rbind(df, df.lvl)[,-c(8:9)]
  df <- cbind(df, SE = round(df$Std.Dev / sqrt(df$Mean), rnd))
  df <- cbind(df,
              ci.Lwr = round(df$Mean - (1.96 * df$SE), rnd),
              ci.Upr = round(df$Mean + (1.96 * df$SE), rnd))
  df <- do.call(rbind, list(df[1,], "_" = "_", df[-1,]))
  
  pvalue <- htest.num(x, y, na.rm)
  df <- data.frame(df, c(pvalue[3], rep("", nrow(df) - 1)))
  colnames(df)[12] <- pvalue[2]
  return(df)
}

str.sum.num <- function(x, y, by, rnd = 1, na.rm = TRUE) 
{
  if (na.rm) lvl <- as.character(unique(na.omit(by))) else 
    lvl <- as.character(unique(by))
  df <- bi.sum.num(x, y, rnd, na.rm)
  df.lvl <- do.call(rbind, lapply(lvl, function(z) {
    bi.sum.num(x[by == z], y[by == z], rnd, na.rm)  }))
  lvl <- ifelse(is.na(lvl), "<NA>", lvl)
  rownames(df.lvl) <- unlist(lapply(lvl, function(z) paste(c('x', 'y'), z, sep = '_')))
  df <- do.call(rbind, list(df, "_" = "_", df.lvl))
  return(df)
}

str.sum.cat <- function(x, y, by, rnd = 1, na.rm = TRUE) 
{
  if (na.rm) lvl <- as.character(unique(na.omit(by))) else 
    lvl <- as.character(unique(by))
  df <- lapply(lvl, function(z) 
    bi.sum.cat(x[by == z], y[by == z], rnd, na.rm))
  df <- structure(df, names = lvl)
  return(df)
}

htest.pvalue <- function(x, y = NULL, test = 'spw') 
{
  pvalue <- switch (test,
    spw = {tryCatch({
      suppressWarnings(shapiro.test(x)$p.value)
    }, error = function(err) {
      return(NA)
    })}, 
    corr = {tryCatch({
      suppressWarnings(cor(x, y, use = 'na.or.complete'))
    }, error = function(err) {
      return(NA)
    })}, 
    ttest = {tryCatch({
      suppressWarnings(t.test(x ~ y)$p.value)
    }, error = function(err) {
      return(NA)
    })}, 
    wilcox = tryCatch({
      suppressWarnings(wilcox.test(x ~ y)$p.value)
    }, error = function(err) {
      return(NA)}), 
    kwallis = tryCatch({
      suppressWarnings(kruskal.test(x ~ y)$p.value)
    }, error = function(err) {
      return(NA)}), 
    anova = tryCatch({
      suppressWarnings(summary(aov(x ~ y))[[1]][1,5])
    }, error = function(err) {
      return(NA)}), 
    fisher = tryCatch({
      suppressWarnings(fisher.test(x ~ y, simulate.p.value = TRUE)$p.value)
    }, error = function(err) {
      return(NA)}), 
    chisq = tryCatch({
      suppressWarnings(chisq.test(x ~ y)$p.value)
    }, error = function(err) {
      return(NA)})
    
  )
  pvalue <- ifelse(pvalue < 0.001, "< 0.001", round(pvalue, 3))
  return(pvalue)
}


htest.num <- function(x, y, na.rm = TRUE) 
{
  if (na.rm) lvl <- as.character(unique(na.omit(y))) else 
    lvl <- as.character(unique(y))
  if (length(lvl) > 2) {
    if (shapiro.test(x)$p.value < 0.05) {
      pvalue <- htest.pvalue(x, y, test = 'kwallis')
      pvalue.name <- 'K-Wallis test'
    } else {
      pvalue <- htest.pvalue(x, y, test = 'anova')
      pvalue.name <- 'ANOVA test'
    }
  } else {
    if (shapiro.test(x)$p.value < 0.05) {
      pvalue <- htest.pvalue(x, y, test = 'wilcox')
      pvalue.name <- 'Wilcox test'
    } else {
      pvalue <- htest.pvalue(x, y, test = 'ttest')
      pvalue.name <- 't-test'
    }
  } 
  df <- c('htest' = paste0('p-value (', pvalue.name, '): ', pvalue), 
          'htest.name' = pvalue.name, 
          'p.value' = pvalue)
  return(df)
}

