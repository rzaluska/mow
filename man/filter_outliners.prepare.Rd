% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_outliers.R
\name{filter_outliners.prepare}
\alias{filter_outliners.prepare}
\title{filter_outliners.prepare - compute first and third quadrile values per column}
\usage{
filter_outliners.prepare(data, columnIndices)
}
\arguments{
\item{columnIndices}{- columns selected for transformation}
}
\value{
first and third quadrile values per column
}
\description{
filter_outliners.prepare - compute first and third quadrile values per column
}
\examples{
q <- filter_outliners.prepare(iris, columnIndices = c(1,2,3,4))
}
