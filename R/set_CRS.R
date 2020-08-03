#' set_CRS
#'
#' Manually set coordinate reference system
#'
#' @param x `sf` or `Spatial*` object
#' @param new_CRS character string or object created via `CRS()`
#' @note The inventory package provides a few: `WGS84` (lng/lat), `UTM10_NAD83`, `UTM10_NAD27`, and `UTM10_WGS84`. For others, see
#'
#' @export
set_CRS <- function (x, new_CRS) {

  if (!inherits(new_CRS, "CRS")) {
    new_CRS <- CRS(new_CRS)
  }

  if (inherits(x, "sf")) {
    st_crs(x) <- new_CRS
  } else if (inherits(x, "Spatial")) {
    proj4string(x) <- new_CRS
  } else {
    stop("Don't know how to handle that class of object")
  }

  return(x)

}
