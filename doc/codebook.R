## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# install.packages("mStats")
library(mStats)
library(labelled)

## -----------------------------------------------------------------------------
# Generate a codebook for the 'iris' dataset
codebook(iris)

# Label their variables
labelled::var_label(iris) <- c(
  "sepal length", "sepal width", "petal length",
  "petal width", "species"
)

# Generate codebook again
codebook(iris)

## -----------------------------------------------------------------------------
# Generate a codebook for a piped dataset
mtcars %>% codebook()

## -----------------------------------------------------------------------------
# Generate a codebook using the native pipe operator
mtcars |> codebook()

