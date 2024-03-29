% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_world.R
\name{load_world}
\alias{load_world}
\title{Load Natural Earth world dataset}
\source{
\code{https://www.naturalearthdata.com/}
}
\usage{
load_world()
}
\value{
A `SpatVector` object representing the World vector data.
}
\description{
This function loads and returns the World Vector data stored within the package.
}
\details{
SpatVector (EPSG: 4326) of the world dissolved on continents.
}
\examples{
\dontrun{
world_data <- load_world_vector()
plot(world_data)
}

}
\seealso{
\code{\link[terra]{vect}}
}
