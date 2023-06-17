

test_that("encode decode works", {

	df <- data.frame(x = 1, y = "1")

  expect_equal(class(decode(df, x)$x), "character")
  expect_equal(class(encode(df, y)$y), "numeric")
})
