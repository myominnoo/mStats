#' @title Date Summary
#'
#' @description
#' idatesum conceptuzlizes dates as factor variable and tabulate the data. The tabulation can be grouped by combinations of the three date components: Year, Month, Day. It also generates a barplot corresponding to the dates split by Year, Year + Month or a combination of three components.
#'
#' @param x a vector describing the bars which make up the plot. It is usually on x axis.
#' @param groupby a character value, indicating patterns of aggregation either by one of the followings:
#'
#' "y" - Year along
#'
#' "ym" - Year + Month
#'
#' "ymd" - Year + Month + Day
#'
#' @seealso itab, ibarplot, inumsum, isum, ikdplot
#' @examples
#' # random dates generation
#' dates <- to.Date(rnorm(100, mean = 4000, sd = 100), "e")
#' idatesum(dates) # by default, pattern is set to Year alone.
#' idatesum(dates, "y")
#' idatesum(dates, "ym")
#' idatesum(dates, "ym", xlab = "Random Dates")
#' idatesum(dates, "ym", xlab = "Random Dates", plot.display = FALSE)
#' idatesum(dates, "ymd") # this can take times.

#' @export
idatesum <- function(x, groupby = "y", decreasing = FALSE, rnd = 1,
                     plot.display = TRUE, xlab = NULL, show.legend = FALSE)
{
  xlab <- ifelse(is.null(xlab), deparse(substitute(x)), xlab)
  x <- na.omit(x)
  yr <- format(x,"%Y")
  mnth <- format(x,"%m")
  day <- format(x, "%d")

  df <- switch(groupby,
        y = aggregate(x = 1:length(x), by = list(Year = yr), FUN = length),
        ym = aggregate(x = 1:length(x), by = list(Year = yr, Month = mnth),
                    FUN = length),
        ymd = aggregate(x = 1:length(x), by = list(Year = yr, Month = mnth, Day = day),
                    FUN = length))

  df <- switch(groupby,
        y = df[order(df$Year, decreasing = decreasing), ],
        ym = df[order(df$Year, df$Month, decreasing = decreasing), ],
        ymd = df[order(df$Year, df$Month, df$Day, decreasing = decreasing), ])

  names(df)[ncol(df)] <- "Count"
  row.names(df) <- 1:nrow(df)
  df$Rel.Freq <- round(df$Count / sum(df$Count) * 100, digits = rnd)
  result <- df

  df$yr.mnth <- paste(df$Year, df$Month, sep = "-")
  df$yr.mnth.day <- paste(df$Year, df$Month, df$Day, sep = "-")

  # plot display
  if (plot.display) {
    p <- switch(groupby,
                y = ggplot2::ggplot(data = df, aes(x = Year, y = Count, fill = Year)) +
                  geom_bar(position = 'dodge', stat = "identity") +
                  geom_text(aes(label = paste0(Rel.Freq, '%')), vjust = -0.5) +
                  ylim(0, (max(df$Count) + (max(df$Count) * 0.1))) ,
                ym = ggplot2::ggplot(data = df,
                                     aes(x = yr.mnth, y = Count, fill = yr.mnth)) +
                  geom_bar(position = 'dodge', stat = "identity") +
                  geom_text(aes(label = paste0(Rel.Freq, '%')), hjust = -0.2) +
                  ylim(0, (max(df$Count) + (max(df$Count) * 0.1))) +
                  coord_flip(),
                ymd = ggplot2::ggplot(data = df,
                                      aes(x = yr.mnth.day, y = Count, fill = yr.mnth.day)) +
                  geom_bar(position = 'dodge', stat = "identity") +
                  geom_text(aes(label = paste0(Rel.Freq, '%')), hjust = -0.2) +
                  ylim(0, (max(df$Count) + (max(df$Count) * 0.1))) +
                  coord_flip())
    p <- p +
      labs(title = paste0('Plot of ', xlab)) +
      xlab(label = xlab) +
      ylab(label = 'Count') +
      theme_light()
    if (!show.legend) p <- p + guides(fill = FALSE)
    plot(p)
  }
  return(result)
}

