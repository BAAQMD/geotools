#' @export
as_raster <- function (x, layer, coord_vars = NULL, crs = NULL, ...) {
  grd <- as_gridded(x, layer = layer, coord_vars = coord_vars, crs = crs)
  raster::raster(grd, layer = layer, ...)
}
