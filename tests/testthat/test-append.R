test_that("append correctly", {
	x <- data.frame(x = 1)
	y <- data.frame(x = 2, y = 3)
	z <- data.frame(y = 4, z = 5)
	expect_equal(append(x, y, z),
							 data.frame(x = c(1, 2, NA),
							 					 y = c(NA, 3, 4),
							 					 z = c(NA, NA, 5)))
})



