#' @title Summarise a Date variable
#' @description
#' \code{dateSum} tabulates dates as factor, based on date components: Day, Month, Year. 
#' The frequency distribution can be also displayed. 
#' @param x a Date vector
#' @param groupBy grouping by one of three date components: 
#' \enumerate{
#'   \item \strong{y}: Year alone
#'   \item \strong{ym}: Year and Month
#'   \item \strong{ymd}: Year, Month and Day
#' }
#' @param decreasing order the data by year, month and day sequence
#' @param rnd specify rounding of numbers
#' @param plot.show display bar plots of the data
#' @param xlab x-axis label
#' @import ggplot2
#' @import stats
#' @seealso \code{\link{itab}}, \code{\link{ibarplot}}, \code{\link{isum}}, 
#' \code{\link{iboxplot}}
#' @keywords date summary, date aggregate
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' # random dates generation
#' set.seed(1)
#' dates <- toDate(rnorm(100, mean = 4000, sd = 100), "e")
#' dateSum(dates) # by default, pattern is set to Year alone.
#' dateSum(dates, "y")
#' dateSum(dates, "ym")
#' dateSum(dates, "ym", xlab = "Random Dates")
#' dateSum(dates, "ym", xlab = "Random Dates", plot.show = FALSE)
#' dateSum(dates, "ymd", plot.show = FALSE)
#' dateSum(dates, "ymd") # this can take times.

#' @export
dateSum <- function(x, groupBy = "y", decreasing = FALSE, rnd = 1,
                    plot.show = TRUE, xlab = NULL)
{
  if (is.null(xlab)) xlab <- deparse(substitute(x))
  if (!is.Date(x)) stop('x must be a Date vector.')
  
  x <- na.omit(x)
  n <- length(x)
  y <- format(x,"%Y")
  m <- format(x,"%m") 
  d <- format(x, "%d") 
  
  df <- switch(
    groupBy,
    y = { ds <- aggregate(seq_len(n), by = list(Year = y), FUN = length)
    ds <- ds[order(ds$Year, decreasing = decreasing), ] },
    ym = { ds <- aggregate(seq_len(n), by = list(Year = y, Month = m), FUN = length) 
    ds <- ds[order(ds$Year, ds$Month, decreasing = decreasing), ] },
    ymd = { ds <- aggregate(seq_len(n), by = list(Year = y, Month = m, Day = d),
                            FUN = length)
    ds <- ds[order(ds$Year, ds$Month, ds$Day, decreasing = decreasing), ] })
  names(df)[ncol(df)] <- "Count"
  row.names(df) <- seq_len(nrow(df))
  df$Rel.Freq <- round(df$Count / sum(df$Count) * 100, digits = rnd)
  
  data <- df
  data$freq.lbl <- paste0(data$Rel.Freq, '%')
  data$Year <- factor(data$Year)
  data$yr.mnth <- factor(paste(data$Year, data$Month, sep = "-"))
  data$yr.mnth.day <- factor(paste(data$Year, data$Month, data$Day, sep = "-"))
  
  if (plot.show) {
    p <- dateSum.plot(data, groupBy) + 
      labs(title = paste0('Plot: ', xlab)) +
      xlab(label = xlab) +
      ylab(label = 'Count') + 
      theme_classic() +
      theme(panel.border = element_rect(
        linetype = "solid",
        colour = "black",
        fill = "NA"))
    data <- list(df, p)
    data <- structure(data, names = c('data', 'plot'))
  } else {data <- df}
  data
}

dateSum.plot <- function(data, groupBy = 'y') {
  p <- switch(
    groupBy,
    y = {
      ggplot2::ggplot(data, aes(x = data$Year, y = data$Count, fill = data$Year)) + 
        geom_text(aes(label = data$freq.lbl, vjust = -0.5)) + 
        ylim(0, (max(data$Count) + (max(data$Count) * 0.1)))
    }, 
    ym = {
      ggplot2::ggplot(data, aes(x = data$yr.mnth, y = data$Count, fill = data$yr.mnth)) +
        geom_text(aes(label = data$freq.lbl), hjust = -0.2) + 
        coord_flip()
    }, 
    ymd = {
      ggplot2::ggplot(data, aes(x = data$yr.mnth.day, y = data$Count, 
                                fill = data$yr.mnth.day)) +
        geom_text(aes(label = data$freq.lbl), hjust = -0.2) + 
        coord_flip()
    })
  p <- p +
    geom_bar(position = 'dodge', stat = "identity") 
  return(p)
}