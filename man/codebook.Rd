% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/codebook.R
\name{codebook}
\alias{codebook}
\title{Generate a codebook}
\usage{
codebook(data)
}
\arguments{
\item{data}{The dataset for which the codebook is to be generated.}
}
\value{
The input dataset is returned invisibly,
allowing \code{codebook()} to be used within a data pipe line.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

The \code{codebook} function generates a codebook for the given dataset. It provides a summary
of the dataset's structure and characteristics, including variable names, types, missing
values, completeness percentages, unique value counts, and variable labels (if available).
}
\examples{
codebook(mtcars)

codebook(iris)

labelled::var_label(iris) <- c(
	"sepal length", "sepal width", "petal length",
	"petal width", "species"
)
codebook(iris)

}
\seealso{
Other Data Management: 
\code{\link{append}()},
\code{\link{count_functions}},
\code{\link{cut}()},
\code{\link{tag_duplicates}()}
}
\concept{Data Management}
