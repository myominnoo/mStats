# numeric vector example
x <- 1:5
cut(x, NA)
cut(x, 1)
cut(x, 2)
cut(x, 5)
cut(x, c(3, 5))
cut(x, c(-Inf, 2, Inf))
cut(x, 1:5)
cut("x", 1)

# date example
x <- Sys.Date() - 1:5
cut(x, 2)

x <- Sys.time() - 1:5
cut(x, 2)
