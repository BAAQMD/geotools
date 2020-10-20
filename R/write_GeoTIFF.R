#' write_GeoTIFF
#'
#' Thin wrapper around [raster::writeRaster()]
#'
#' @param raster_obj
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
write_GeoTIFF <- function (
  raster_obj,
  ...,
  suffix = "names",
  overwrite = TRUE
) {

  raster::writeRaster(
    raster_obj,
    ...,
    format = "GTiff",
    suffix = suffix,
    overwrite = overwrite)

}
