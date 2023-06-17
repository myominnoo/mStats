
library(dplyr)

test_that("tagging duplicates works", {
	data <- data.frame(
		x = c(1, 1, 2, 2, 3, 4, 4, 5),
		y = letters[1:8]
	)

	expect_equal(mutate(data, tag_duplicates(x, .add_tags = TRUE))$.n_ |>
							 	setNames(NULL), c(1,2,1,2,1,1,2,1))
	expect_equal(mutate(data, tag_duplicates(x, .add_tags = TRUE))$.N_ |>
							 	setNames(NULL), c(2,2,2,2,1,2,2,1))
	expect_equal(mutate(data, tag_duplicates(x, .add_tags = TRUE))$.dup_ |>
							 	setNames(NULL), c(FALSE, TRUE, FALSE, TRUE, FALSE,
							 										FALSE, TRUE, FALSE))
})
