#' st_extent
#'
#' @param x `sf` or `sfc` object
#' @param ... passed to [raster::extent()]
#'
#' @return `raster::Extent` object
#' @export
#'
st_extent <- function (x, ...) {
  bb <- sf::st_bbox(x)
  coords <- bb[c("xmin", "xmax", "ymin", "ymax")]
  return(raster::extent(coords, ...))
}
