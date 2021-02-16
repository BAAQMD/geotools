#' st_extent
#'
#' @param x `sf` or `sfc` object
#' @param ... passed to [raster::extent()]
#'
#' @return `raster::Extent` object
#' @export
#'
st_extent <- function (x, ...) {

  if (is.numeric(x)) {

    coords <- c(x, ...)
    if (isFALSE(length(coords) == 4)) {
      stop("[st_extent] four values are needed")
    }

  } else {

    bb <- sf::st_bbox(x)
    coords <- bb[c("xmin", "xmax", "ymin", "ymax")]

  }

  extent <-
    raster::extent(
      coords,
      ...)

  return(extent)

}
