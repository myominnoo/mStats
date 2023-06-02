library(tidyverse)
devtools::load_all()




iris |>
	mutate(Species = label(Species, 'Species of iris flower')) |>
	codebook()
iris |>
	label("Iris dataset") |>
	codebook()


