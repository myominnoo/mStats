% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/n_N.R
\name{count_functions}
\alias{count_functions}
\alias{n_}
\alias{N_}
\title{Count from n to N}
\usage{
n_(...)

N_(...)
}
\arguments{
\item{...}{<\code{\link[dplyr:dplyr_tidy_select]{tidy-select}}>

Columns to pick.

You can't pick grouping columns because they are already automatically
handled by the verb (i.e. \code{\link[dplyr:summarise]{summarise()}} or \code{\link[dplyr:mutate]{mutate()}}).}
}
\value{
A numeric vector representing the count from \code{n} to \code{N}.
}
\description{
\Sexpr[results=rd]{lifecycle::badge("stable")}
}
\details{
These functions are used for indexing observations or
generating sequences of numbers.
\itemize{
\item \code{n_()} generates a running counter within a group of variables and
represents the number of the current observation.
\item \code{N_()} provides the total count within each group of variables.
}

You can do these operations using \code{\link[dplyr:context]{dplyr::n()}} in this way.
See examples below using \link{iris} dataset.

iris |> mutate(.N_ = n()) |> head()
iris |> mutate(.n_ = 1:n()) |> head()
iris |> group_by(Species) |> mutate(.n_ = 1:n()) |> slice(1:5) |> ungroup()
}
\examples{

# Example with a custom dataset
df <- data.frame(
  x = c(1, 1, 2, 2, 2, 3, 4, 4, 4, 4),
  y = letters[1:10]
)

library(dplyr)

# Generate a running counter for each observation within the "x" group using mutate()
mutate(df, n = n_(x))

# Generate a running counter for each observation for all columns using mutate()
mutate(df, n = n_(everything()))

# Generate the total count of observations using summarise()
reframe(df, n = n_(x))

# Generate the total count of observations within the "x" group using summarise()
mutate(df, N = N_(everything()))
mutate(df, N = N_(x))
reframe(df, N = N_(x))

# iris dataset
mutate(iris, n = n_(everything()))
mutate(iris, N = N_(everything()))
}
\seealso{
Other Data Management: 
\code{\link{append}()},
\code{\link{codebook}()},
\code{\link{cut}()},
\code{\link{tag_duplicates}()}
}
\concept{Data Management}
