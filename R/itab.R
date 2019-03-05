#' @title Frequency distribution, cross-tabulation and stratified tabulation
#' @description
#' \code{itab} generates Frequency distribution, cross-tabulation and stratified 
#' tabulations. 
#' @param x a factor object
#' @param y a factor object; if ignored, frequency distribution on x is generated. 
#' @param by a factor object; if ignored, cross-tabulation on x and y is generated.
#' @param data an optional data frame 
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#' @param na.rm A logical value to specify missing values, <NA> in the table
#' @param pct type of percentages in cross-tabulation: by default, it's row percentage.
#' @param plot.show display bar plots of the data
#' @details 
#' Exploring data before jumping into complex analysis is always a necessity. 
#' The first step of an analysis should be to summarize and display data.
#' By doing so, invaluable insight can be gained through the familiarity with the data.
#' 
#' \code{itab} 
#' uses simple tabular techniques for displaying the distribution of
#' values taken by a single variable, the association between two variables as well as 
#' stratified tabulation. 
#' 
#' In addition, \code{itab} generates delightful bar plots of the data, using another 
#' function called \code{\link{ibarplot}}. This function also make use of a powerful 
#' and famous package, \code{\link{ggplot2}}, hence the plots generated 
#' by \code{itab} are just beautiful and delightful. If you want to save the plot, 
#' see \code{\link{ibarplot}} for full details. 
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
#' @import graphics
#' @seealso \code{\link{isum}}, \code{\link{ibarplot}}
#' @keywords frequency distribution, cross-tabulation, stratified tabulation
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' str(infert)
#' 
#' ## univariate analysis
#' itab(infert$education) 
#' itab(education, data = infert)
#' itab(spontaneous, data = infert)
#' 
#' ## bivariate analysis
#' itab(infert$education, infert$case) 
#' itab(education, case, data = infert, pct = 'col') # col percentage
#' itab(education, case, data = infert, pct = 'all')
#' 
#' ## stratified analysis 
#' itab(education, case, parity, data = infert)
#' itab(education, case, parity, data = infert, na.rm = TRUE)

#' @export
itab <- function(x, y = NULL, by = NULL, data = NULL, rnd = 1, na.rm = FALSE,
                 pct = "row", plot.show = TRUE)
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
  
  include.na <- ifelse(na.rm, "no", "ifany")
  type <- ifelse(is.null(y), "uni", ifelse(is.null(by), "bi", "strata"))
  
  res <- switch(
    type,
    uni = {
      cat(paste0('\nFrequency distribution: ', x.name, '\n\n'))
      unitab(x, rnd, include.na)}, 
    bi = {
      cat(paste0('\nCross-tabulation: \"', x.name, '\" ~ \"', y.name, '\"\n',
                     'Note: showing ', toupper(pct), ' percentages\n\n'))
      bitab(x, y, rnd, include.na, pct)}, 
    strata = {
      cat(paste0('\nStratified cross-tabulation: \"', x.name, '\" ~ \"', y.name, 
                     '\", by \"', by.name, '\"\n',
                     'Note: showing ', toupper(pct), ' percentages\n\n'))
      strtab(x, y, by, rnd, include.na, pct)}
  )
  
  if (plot.show) {
    main <- ifelse(is.null(y), paste0('Plot: ', x.name), 
                               ifelse(is.null(by), 
                                      paste0('Plot: ', x.name, ' ~ ', y.name), 
                                      paste0('Plot: ', x.name, ' ~ ', y.name, 
                                             ' | ', by.name)))
    plot(
      ibarplot(x, y, by, na.rm = na.rm, main = main, xlab = x.name, 
               legend.text = y.name)
    )
    
  }
  
  return(res)
}

unitab <- function(x, rnd, include.na) {
  tbl <- table(x, useNA = include.na)
  t.count <- c(tbl, Total = sum(tbl))
  c.count <- c(cumsum(tbl), sum(tbl))
  r.freq <- t.count / sum(tbl) * 100
  c.freq <- c.count / sum(tbl) * 100
  df <- cbind(t.count, round(r.freq, rnd), round(c.freq, rnd))
  colnames(df) <- c("Freq.", "(Percent.)", "(Cum.Percent)")
  return(df)
}

bitab <- function(x, y, rnd = 1, include.na, pct = 'row') {
  tab <- table(x, y, useNA = include.na)
  tabr <- rbind(tab, Total = colSums(tab))
  tabc <- cbind(tab, Total = rowSums(tab))
  tbl <- cbind(tabr, Total = rowSums(tabr))
  
  rpct <- round(tbl / rowSums(tabr) * 100, rnd) # row pct
  cpct <- round(t(t(tbl) / colSums(tabc)) * 100, rnd) # col pct
  tpct <- round(tbl / sum(tab) * 100, rnd) # total pct
  
  if (any(tab < 5)) 
    pvalue <- tryCatch({
      suppressWarnings(fisher.test(x, y, simulate.p.value = TRUE)$p.value)
    }, error = function(err) {
      return(NA)
    }) else 
      pvalue <- tryCatch({
        suppressWarnings(chisq.test(x, y, correct = FALSE)$p.value)
      }, error = function(err) {
        return(NA)
      })
  pvalue <- c(ifelse(pvalue < 0.001, "<0.001", round(pvalue, 3)),
              rep("", nrow(tbl) - 1))
  pvalue.name <- ifelse(any(tab < 5), 'p-value (Exact)', 'p-value (Chi)')
  
  tblr <- NULL; tblc <- NULL; tblt <- NULL
  var.r <- NULL; var.c <- NULL; var.t <- NULL
  for ( i in seq_len(ncol(tbl))) {
    tblt <- data.frame(cbind(tblt, tbl[,i], tpct[,i]))
    var.t <- c(var.t, colnames(tbl)[i], "(t%)")
    tblr <- data.frame(cbind(tblr, tbl[,i], rpct[,i]))
    var.r <- c(var.r, colnames(tbl)[i], "(r%)")
    tblc <- data.frame(cbind(tblc, tbl[,i], cpct[,i]))
    var.c <- c(var.c, colnames(tbl)[i], "(c%)")
  }
  
  tblt <- cbind(tblt, pvalue = pvalue)
  tblr <- cbind(tblr, pvalue = pvalue)
  tblc <- cbind(tblc, pvalue = pvalue)
  
  colnames(tblt) <- c(var.t, pvalue.name)
  colnames(tblr) <- c(var.r, pvalue.name)
  colnames(tblc) <- c(var.c, pvalue.name)
  
  df <- list(no.percentage = tbl,
             total.percentage = tblt,
             row.percentage = tblr,
             column.percentage = tblc)
  
  return(switch(pct,
                no = tbl,
                total = tblt,
                row = tblr,
                col = tblc,
                all = df))
}

strtab <- function(x, y, by, rnd = 1, include.na = 'ifany', pct = 'row') 
{
  if (include.na == 'no') lvl <- unique(na.omit(by)) else lvl <- unique(by)
  df <- lapply(lvl, function(z) bitab(x[by == z], y[by == z], 
                                      rnd, include.na, pct))
  df <- structure(df, names = lvl)
  return(df)
}