
test_that("label works", {
	x <- 1:3
	expect_equal(label(x, "something") |> attr("label"), "something")
	expect_equal(label(iris, "something") |> attr("label"), "something")
})
