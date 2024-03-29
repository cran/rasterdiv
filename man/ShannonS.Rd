% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ShannonS.R
\name{ShannonS}
\alias{ShannonS}
\title{Calculate Shannon-Wiener Index on a Matrix}
\usage{
ShannonS(x, window = 1, na.tolerance = 1, debugging = FALSE)
}
\arguments{
\item{x}{A numeric matrix representing the data on which the index is to be calculated.}

\item{window}{The width of the moving window to consider for each cell. The actual window size 
will be `(2 * window + 1) x (2 * window + 1)`. Default is 1.}

\item{na.tolerance}{The tolerance level for missing data within the moving window. 
A window will be processed only if the proportion of non-missing data is above this threshold. 
Value should be between 0 and 1. Default is 1.}

\item{debugging}{Boolean flag to enable or disable debugging messages. Default is FALSE.}
}
\value{
A matrix of the same dimensions as `x`, where each cell contains the 
        Shannon-Wiener Index calculated for the window around the cell.
}
\description{
This function calculates the Shannon-Wiener Index for each cell in a matrix,
considering a specified moving window around each cell.
}
\examples{
data <- matrix(runif(100), nrow = 10)
shannon_index <- ShannonS(data, window = 1)

}
