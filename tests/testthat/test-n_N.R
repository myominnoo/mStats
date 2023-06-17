library(dplyr)

df <- data.frame(
	x = c(1,1,2,2,2,3,4,4,4,4),
	y = letters[1:10]
)


test_that("n_N counts correctly", {
	expect_equal(mutate(df, n = n_(everything()))$n |> setNames(NULL), rep(1, 10))
	expect_equal(mutate(df, n = n_(x))$n |> setNames(NULL), c(1,2,1,2,3,1,1,2,3,4))
	expect_equal(mutate(df, n = n_(y))$n |> setNames(NULL), rep(1, 10))

	expect_equal(mutate(df, N = N_(everything()))$N |> setNames(NULL), rep(1, 10))
	expect_equal(mutate(df, N = N_(x))$N |> setNames(NULL), c(2,2,3,3,3,1,4,4,4,4))
	expect_equal(mutate(df, N = N_(y))$N |> setNames(NULL), rep(1, 10))

})

