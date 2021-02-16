#' write_raster
#'
#' @param raster_obj
#' @param path
#' @param ...
#' @param format
#' @param overwrite
#'
#' @return
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
