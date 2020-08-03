#' as_raster
#'
#' Rasterize geodata
#'
#' @param x passed to `as_gridded()`
#' @param layer layer name
#' @param coord_vars names of coordinates (optional)
#' @param crs passed to `as_gridded()`
#' @param ... passes to `raster::raster()`
#'
#' @importFrom raster raster
#'
#' @export
as_raster <- function (
  x,
  layer,
  coord_vars = NULL,
  crs = NULL,
  ...
) {

  gridded_geodata <-
    as_gridded(
      x,
      layer = layer,
      coord_vars = coord_vars,
      crs = crs)

  raster_object <-
    raster::raster(
      gridded_geodata,
      layer = layer,
      ...)

  return(raster_object)

}
