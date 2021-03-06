% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mStats.R
\name{svytab}
\alias{svytab}
\title{Tabulation for several binary variables}
\usage{
svytab(data, ..., value = NULL)
}
\arguments{
\item{data}{data.frame}

\item{...}{variable name or names of multiple variables}

\item{value}{character indicating the level of the variables}
}
\description{
\code{svytab()} generates a summary of several variables of a
survey dataset.
}
\examples{

svytab(infert, case, induced, spontaneous)
svytab(infert, case, induced, spontaneous, value = 1)

}
\author{
Email: \email{dr.myominnoo@gmail.com}

Website: \url{https://myominnoo.github.io/}
}
