% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/raster_cut.R
\name{raster_cut}
\alias{raster_cut}
\title{raster_cut}
\usage{
raster_cut(
  rst,
  breaks,
  include.lowest = TRUE,
  ...,
  na.rm = TRUE,
  verbose = getOption("verbose", default = FALSE)
)
}
\arguments{
\item{rst}{\href{raster::Raster-class}{RasterLayer} object}

\item{breaks}{passed to \code{\link[=cut]{cut()}}}

\item{include.lowest}{passed to \code{\link[=cut]{cut()}}}

\item{...}{passed to \code{\link[=cut]{cut()}}}

\item{na.rm}{logical; passed to \code{\link[=cut]{cut()}}}

\item{verbose}{logical}
}
\value{
a new \href{raster::Raster-class}{RasterLayer} object, of the same dimensions, etc., with values cut accordingly
}
\description{
raster_cut
}
