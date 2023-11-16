#' set_CRS
#'
#' Manually set coordinate reference system
#'
#' @param x `sf` or `Spatial*` object
#' @param new_CRS character string or object created via `CRS()`
#'
#' @importFrom sf st_crs
#' @importFrom sp CRS proj4string
#'
#' @export
set_CRS <- function (x, new_CRS) {

  .Defunct("sf::st_set_crs()")

  if (!inherits(new_CRS, "CRS")) {
    new_CRS <- sp::CRS(new_CRS)
  }

  if (inherits(x, "sf")) {
    sf::st_crs(x) <- new_CRS
  } else if (inherits(x, "Spatial")) {
    sp::proj4string(x) <- new_CRS
  } else {
    stop("Don't know how to handle that class of object")
  }

  return(x)

}
