% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ndviForestTS.R
\docType{data}
\name{ndviForestTS}
\alias{ndviForestTS}
\title{Simulated NDVI dataset}
\format{
A \code{list} containing matrices:
\describe{
  \item{ndviForestTS}{List of matrixes of 9 cells simulating NDVI of a patch of forests over 3 years. Each matrix represents a day in the time series.}
}
}
\description{
A \code{list} of 8-bit matrices.
}
\details{
This list represents a time series of NDVI values of a patch of forest over 3 years. 
It is stored as a \code{list}, suitable for explaining how to make helical plots.
}
\examples{
ndviForestTS <- readRDS(system.file("extdata", "ndviForestTS.rds", package = "rasterdiv"))
}
\keyword{datasets}
