#' Use IDW to interpolate the given variable (with values and locations found in 'spdf')
#' at locations given by "spobj" (typically a SpatialPolygons or SpatialGrid* object).
#'
#' @export
idw_interpolation <- function (spdf, spobj, variable="mean", ...) {
  require(gstat)
  idw(
    as.formula(str_c(variable, " ~ 1")),
    non_missing(spdf, variable),
    newdata = spobj,
    ...
  )$var1.pred
}
