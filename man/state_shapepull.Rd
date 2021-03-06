% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/state_shapepull.R
\name{state_shapepull}
\alias{state_shapepull}
\title{A function to import state shapefiles}
\format{Simple feature collection with 4 fields: \describe{
  \item{State.Post}{State postal code, character string} \item{FIPSn}{State ID, character}
   \item{Name}{State name, character string} \item{geometry}{sfc_MULTIPOLYGON}}}
\source{
\url{https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/State+Shapes/US_States_Viz_2020_high_res.zip}
}
\usage{
state_shapepull()
}
\value{
A spatial object where each row is a state.
}
\description{
This function allows you to import a simplified version of state shapefiles which match the the US Census
Bureau, Education Demographic and Geographic Estimates Program (EDGE),
Composite School District Boundaries File.
}
\examples{
\donttest{state_shp <- state_shapepull()}
}
\keyword{EdBuild}
\keyword{shapefile}
