
# install.packages("mStats")

library(pillar)
library(mStats)
library(dplyr)


mStats::codebook(iris)
pillar::glimpse(iris)
skimr::skim(iris)


codebook(df)
codebook(iris)
codebook(airmiles)
iris |> codebook()
infert %>% codebook()

nycflights13::airlines |> codebook()

df



library(dplyr)
library(labelled)
df <- tibble(
	id = 1:3,
	happy = factor(c('yes', 'no', 'yes')),
	gender = labelled(c(1, 1, 2), c(female = 1, male = 2))
) %>%
	set_variable_labels(
		id = "Individual ID",
		gender = "Gender of respondent"
	)
df



# Alternative syntax
fdf <- subset(df, id < 3)
fdf
fdf <- copy_labels(from = df, to = fdf)

fdf <- mutate(df, gender = as.numeric(gender),
							gender2 = as.numeric(gender))
fdf

copy_labels(df, fdf)






