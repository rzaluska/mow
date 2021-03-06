% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discretization.R
\name{discretization.prepare}
\alias{discretization.prepare}
\title{discretization.prepare - compute discreatization thresholds}
\usage{
discretization.prepare(data, columnIndices, columnOptions)
}
\arguments{
\item{columnOptions}{list of options in followin form per column
[0] numSlices - number of discrete values to produce
[1] type - type of discreatization, one of:
    - 'even' - split domain by even slices, where slice size is (max - min)/numSlices
    - 'size' - split domain by slices of even numer of examples in them}
}
\value{
list of threshold values of size (numSlices - 1)
}
\description{
discretization.prepare - compute discreatization thresholds
}
\examples{
t = discretization.prepare(iris, c(1,2), list(list(5, 'even'),list(5, 'size')))
}
