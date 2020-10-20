#' mask_raster
#'
#' Thin wrapper around [raster::mask()]. Works with an `sf` object, and automatically reprojects it to match the CRS of `x`.
#'
#' @param x `Raster*` object
#' @param mask `sf` object
#' @param ... reserved for future use
#'
#' @return masked `Raster*` object
#' @export
mask_raster <- function (x, mask, ...) {
  raster::mask(x, as(st_transform(mask, st_crs(x)), "Spatial"))
}
