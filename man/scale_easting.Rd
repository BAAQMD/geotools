% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_easting.R
\name{scale_easting}
\alias{scale_easting}
\title{scale_easting}
\usage{
scale_easting(
  name = "Easting",
  ...,
  unit = "km",
  expand = ggplot2::expansion(mult = 0, add = 0),
  labels = purrr::partial(qtytools::convert_qty, from = "m", to = unit)
)
}
\arguments{
\item{name}{character}

\item{...}{passed to \code{\link[ggplot2:scale_continuous]{ggplot2::scale_x_continuous()}}}

\item{unit}{character, like "km"}

\item{expand}{like that returned by \code{ggplot2::expansion()}}

\item{labels}{function that generates labels}
}
\value{
ggplot2 scale object
}
\description{
scale_easting
}
