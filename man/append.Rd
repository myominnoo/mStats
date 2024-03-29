% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/append.R
\name{append}
\alias{append}
\title{Append datasets}
\usage{
append(...)
}
\arguments{
\item{...}{Data frames to combine.}
}
\value{
A data frame
}
\description{
\Sexpr[results=rd]{lifecycle::badge("deprecated")}
}
\details{
\code{append} stacks multiple datasets.
}
\examples{
append(airquality, mtcars)

}
\seealso{
Other Data Management: 
\code{\link{codebook}()},
\code{\link{count_functions}},
\code{\link{cut}()},
\code{\link{tag_duplicates}()}
}
\concept{Data Management}
