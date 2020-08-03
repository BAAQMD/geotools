#' idw_interpolation
#'
#' Use IDW to interpolate the given variable (with values and locations found in 'spdf')
#' at locations given by "spobj" (typically a SpatialPolygons or SpatialGrid* object).
#'
#' @param spdf `Spatial*DataFrame` object (to learn from)
#' @param spobj `Spatial*` object (to predict onto)
#' @param variable name of variable to interpolate
#' @param ... passed to `gstat::idw()`
#'
#' @importFrom gstat idw
#' @importFrom stringr str_c
#' @importFrom stats as.formula
#'
#' @export
idw_interpolation <- function (
  spdf,
  spobj,
  variable = "mean",
  ...
) {

  idw_formula <-
    stats::as.formula(
      stringr::str_c(variable, " ~ 1"))

  gstat_result <-
    gstat::idw(
      idw_formula,
      non_missing(spdf, variable),
      newdata = spobj,
      ...)

  prediction <-
    gstat_result[["var1.pred"]]

  return(prediction)

}
