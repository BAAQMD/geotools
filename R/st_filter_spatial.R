#' Filter one set of geometries based on another
#'
#' @param x geometries to filter
#' @param y geometries to filter by
#' @param datum intermediate datum for projection
#' @param view logical; show results in Rstudio viewer?
#' @param verbose logical
#'
#' @note
#'
#' @note FIXME: move to `geotools` package
#'
#' @export
st_filter_spatial <- function (x, y, datum = st_crs(32610), fun = st_intersection, view = FALSE, verbose = TRUE) {

  msg <- function (...) if (isTRUE(verbose)) message("[st_filter_spatial] ", ...)

  msg("spatially filtering n = ", nrow(x), " geometries based on m = ", nrow(y), " geometries")
  filtered <- fun(st_transform(x, datum), st_transform(y, datum))

  msg("repairing spatially filtered geometries")
  repaired <- st_repair(filtered)

  reprojected <- st_transform(repaired, st_crs(x))

  if (isTRUE(view)) mapview::mapview(reprojected)

  return(reprojected)

}
