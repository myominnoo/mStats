
test_that("codebook returns data type correctly", {
	expect_true(inherits(codebook(data.frame(1)), "data.frame"))
	expect_true(inherits(codebook(AirPassengers), "ts"))
	expect_true(inherits(codebook(6L), "integer"))
	expect_true(inherits(codebook(5), "numeric"))
})
