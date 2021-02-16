#' write_GeoTIFF
#'
#' Thin wrapper around [write_raster()]
#'
#' @param raster_obj [Raster*](raster::Raster-class) object
#' @param ... passed to [write_raster()]
#'
#' @seealso
#' - [write_raster()]
#'
#' @return result of `write_raster()`
#' @export
#'
write_GeoTIFF <- function (
  raster_obj,
  ...,
  suffix = "names",
  overwrite = TRUE
) {

  written <-
  write_raster(
    raster_obj,
    ...,
    format = "GTiff",
    suffix = suffix,
    overwrite = overwrite)

  return(written)

}
