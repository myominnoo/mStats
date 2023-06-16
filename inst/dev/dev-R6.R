library(tidyverse)
devtools::load_all()




iris |>
	mutate(Species = label(Species, 'Species of iris flower')) |>
	codebook()
iris |>
	label("Iris dataset") |>
	codebook()


dupxmpl <- haven::read_dta("https://www.stata-press.com/data/r18/dupxmpl.dta")
dupxmpl |> mutate(tag_duplicates(everything()))
dupxmpl |> mutate(tag_duplicates(everything(), .add_tags = TRUE))
