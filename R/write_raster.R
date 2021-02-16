#' write_raster
#'
#' @param raster_obj [Raster*](raster::Raster-class) object
#' @param path character
#' @param ... passed to [raster::writeRaster()]
#' @param format character (defaults to "raster"); see [raster::writeRaster()]
#' @param overwrite logical
#'
#' @return result of [raster::writeRaster()], most likely a handle to an on-disk raster
#' @export
#'
write_raster <- function (
  raster_obj,
  path,
  ...,
  format = "raster",
  overwrite = TRUE
) {

  written <-
    raster::writeRaster(
      raster_obj,
      filename = path,
      format = format,
      overwrite = overwrite,
      ...)

  return(written)

}
