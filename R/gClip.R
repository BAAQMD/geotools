#' Clip one SpatialPolygons* object with another SpatialPolygons* object
#'
#' @param spobj1 `SpatialPolygons*` object
#' @param spobj2 `SpatialPolygons*` object
#' @param ... reserved for future use
#'
#' @details `spobj1` will be clipped to the boundary of `spobj2`.
#'
#' @note You're responsible for proper handling of numeric attributes.
#'
#' @importFrom sf st_as_sf st_transform st_intersection st_crs
#'
#' @export
gClip <- function (spobj1, spobj2, ...) {
  sf1 <- sf::st_as_sf(spobj1)
  sf2 <- sf::st_transform(sf::st_as_sf(spobj2), sf::st_crs(sf1))
  clipped <- sf::st_intersection(sf1, sf2)
  return(clipped)
}
