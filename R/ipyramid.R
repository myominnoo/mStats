#' @title The Population Pyramid 
#'
#' @description
#' \code{ipyramid} generates a population pyramid graph and data output.
#'
#' @param x a numeric vector: age in a typical Population Pyramid plot.
#' @param y a binary vector: sex in a typical Population Pyramid plot.
#' @param data an optional data frame
#' @param na.rm A logical value indicating to remove NA values
#' @param binwidth specify the cut points of x: 
#' By default, cut-off is set at 10 years interval from the lowest value. 
#' A sequence of cut-off points can also be speicified. For example, 
#' \code{binwidth = seq(1:100, 10)} or \code{binwidth = c(18, 20, 25, 30, 60, 90)}
#' @param main title of the plot
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param legend.text title of legend
#' @param text.size size of text in the plot. Font size for data level is 14:5 ratio 
#' to axis font size
#' @param getdata generate cross-tabulation, data frame in addition to plot.
#' @param plot.save save the plot
#' @param plot.name specify for name of the plot file to be saved. 
#' Filename can be in either of these extension formats: ".tiff", ".png", and ".pdf"
#' @param width integer measured in inches
#' @param height integer measured in inches
#' @param dpi specify resolution of the plot to be saved
#' @details 
#' Histogram is typically used to display the age distribution of a population. However, 
#' it is usual to present age ditribution separately for two sexes because mortality tends to
#' be different. In a population pyramid, age is scaled vertically and sex horizontally. 
#' The number or frequency scale of sex is zero at the middle and increases or decreases to 
#' both sides. 
#' 
#' \strong{Usage in other types of data} 
#' 
#' It is interesting to see the application of age-sex pyramid population plot for other 
#' types of data. I would like to hear more comments about this. 
#' 
#' \strong{References:}
#' \enumerate{
#'   \item An Introduction to MEdical Statistics, Martin Bland, Thrid Edition, 
#'   Chapter 16, page 303
#' }
#'   
#' @import ggplot2
#' @seealso \code{\link{ibarplot}}, \code{\link{iboxplot}}, \code{\link{isum}}, 
#' \code{\link{toDate}}
#' @keywords population pyramid, mortality statistics, cross-tabulation, contigency table
#' @examples
#' ## create random vectors
#' set.seed(100)
#' age <- sample(11:100, 1000, replace = TRUE)
#' sex <- factor(sample(c("male", "female"), 1000, replace = TRUE))
#' 
#' ipyramid(age, sex)
#' ipyramid(age, sex, binwidth = 5)
#' ipyramid(age, sex, binwidth = 20, getdata = TRUE)
#' 
#' data <- data.frame(age, sex)
#' ipyramid(age, sex, data = data)
#' ipyramid(age, sex, data = data, binwidth = 5)

#' @export
ipyramid <- function(x, y, data = NULL, na.rm = FALSE, binwidth = NULL, 
                     main = NULL, xlab = NULL, ylab = NULL, legend.text = NULL, 
                     text.size = 12, getdata = FALSE,
                     plot.save = FALSE, plot.name = '<unnamed>.tiff',
                     width = 5, height = 4, dpi = 150)
{
  if (!is.null(data)) {
    arguments <- as.list(match.call())
    x <- eval(substitute(x), data)
    y <- eval(substitute(y), data)
    
    lab.x <- arguments$x
    lab.y <- arguments$y
    
  } else {
    lab.x <- deparse(substitute(x))
    lab.y <- deparse(substitute(y))
  }
  
  main <- ifelse(is.null(main),
                 paste0("Plot: ", lab.x, " ~ ", lab.y), main)
  xlab <- ifelse(is.null(xlab), "Count", xlab)
  ylab <- ifelse(is.null(ylab), lab.x, ylab)
  legend.text <- ifelse(is.null(legend.text), lab.y, legend.text)
  
  data <- data.frame(x = x, y = y)
  if (na.rm) data <- na.omit(data)
  data <- data[!is.na(y), ]
  x <- data$x
  y <- data$y
  y.cat <- as.character(unique(y))
  if (length(y.cat) > 2) stop('y must be binary.')
  
  if (is.null(binwidth)) {
    binwidth <- 10
    x.brk <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), binwidth)
    x.brk <- c(x.brk[1], x.brk[2:(length(x.brk)-1)] - 1, x.brk[length(x.brk)])
  }  else {
    if (length(binwidth) == 1) {
      x.brk <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), binwidth)
      x.brk <- c(x.brk[1], x.brk[2:(length(x.brk)-1)] - 1, x.brk[length(x.brk)])
    } else {
      x.brk <- binwidth
    }
  }
  
  if (x.brk[length(x.brk)] < max(x, na.rm = TRUE))
    x.brk <- c(x.brk, max(x, na.rm = TRUE))
  
  x.lbl.lwr <- c(x.brk[1], x.brk[-c(1, length(x.brk))] + 1)
  x.lbl.upr <- x.brk[-1]
  x.lbl <- paste(x.lbl.lwr, x.lbl.upr, sep = "-")
  
  data$x.cut <- cut(data$x, breaks = x.brk, labels = x.lbl, right = TRUE,
                    include.lowest = TRUE)
  
  data$count <- 1
  df <- with(data, aggregate(count, by = list(x.cut = x.cut, y = y), sum))
  df$rx <- ifelse(df$y == y.cat[1], -df$x, df$x)
  y.min <- -(max(df$x) + (max(df$x) * 0.1))
  y.max <- (max(df$x) + (max(df$x) * 0.1))
  y.brk <- pretty(seq(y.min, y.max))
  y.limits <- c(y.min + (y.min * 0.1), y.max + (y.max * 0.1))
  
  p <- ggplot2:: ggplot(df,  aes(x = df$x.cut, y = df$rx, fill = y)) +
    geom_col() +
    geom_hline(yintercept = 0, color = "black", size = 0.2) +
    scale_x_discrete(labels = x.lbl) +
    scale_y_continuous(breaks = y.brk, limits = y.limits, labels = abs) +
    geom_text(aes(label = x, hjust = ifelse(df$rx >= 0, -0.25, 1.25)), 
              size = text.size / (14/5)) +
    labs(title = main, x = ylab, y = xlab, fill = legend.text) +
    coord_flip() +
    theme_classic() +
    theme(text = element_text(size = text.size),
          panel.border = element_rect(linetype = "solid", colour = "black", fill = "NA"))
  
  tbl <- table(data$x.cut, data$y)
  tbl <- cbind(tbl, Total = rowSums(tbl))
  tbl <- rbind(tbl, Total = colSums(tbl))
  names(dimnames(tbl)) <- c(paste0(lab.x, '.cat'), lab.y)
  data <- data[,1:3]
  names(data) <- c(lab.x, lab.y, paste0(lab.x, '.cat'))
  
  if (getdata) {
    data <- list('crossTab' = tbl,
                 'data' = data, 
                 'plot' = p)
  } else {data <- p}
  
  if (plot.save) {
    plot(p)
    ggplot2::ggsave(plot.name, width = width, height = height, dpi = dpi)
    dev.off()
    cat(paste0("note: plot saved to \"", getwd(), "/", plot.name, "\"\n\n"))
  }
  return(data)
}