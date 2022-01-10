#' raster_mask
#'
#' Thin wrapper around [raster::mask()]. Works with an `sf` object, and automatically reprojects it to match the CRS of `x`.
#'
#' @param rst `Raster*` object
#' @param mask `sf` object
#' @param ... reserved for future use
#'
#' @return masked `Raster*` object
#' @export
raster_mask <- function (rst, mask, ...) {
  raster::mask(rst, as(st_transform(mask, st_crs(x)), "Spatial"))
}
