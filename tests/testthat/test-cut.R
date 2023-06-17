
library(testthat)

# Numeric vector example
x <- 1:5

test_that("Cutting x with invalid argument: NA, 1", {
	expect_error(cut(x, NA))
	expect_error(cut(x, 1))
})

test_that("Cutting numeric vector with single cut point", {
	expect_equal(cut(x, 2),
							 as.factor(c("1-2", "1-2", "3-5", "3-5", "3-5")))
	expect_equal(cut(x, 5),
							 as.factor(c("1-1.7", "1.8-2.5", "2.6-3.3", "3.4-4.1", "4.2-5")))
	expect_equal(cut(x, c(3, 5)),
							 as.factor(c("1-2", "1-2", "3-5", "3-5", "3-5")))
})

test_that("Cutting numeric vector with infinite values", {
	expect_equal(cut(x, c(-Inf, 3, Inf)),
							 as.factor(c("1-2", "1-2", "3-5", "3-5", "3-5")))
})

test_that("Cutting numeric vector with vector-based cut points", {
	## unexpected behaviours here !!
	expect_equal(cut(x, 1:5), as.factor(c("1-1", "2-2", "3-3", "4-5", "4-5")))
})

