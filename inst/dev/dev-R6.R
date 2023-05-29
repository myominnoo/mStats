library(tidyverse)
devtools::load_all()


iris |>
	mutate(tag_duplicates(everything()))
iris |>
	mutate(tag_duplicates(Sepal.Length, Sepal.Width, Petal.Length,
												Petal.Width, Species))
