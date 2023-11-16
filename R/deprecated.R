#' Coerce an `sf` object to a `tibble`
#'
#' @param geodata an `sf` object
#' @return a `tibble`
#' @export
drop_geometry <- function (geodata) {
  .Deprecated("st_drop_geometry")
  return(sf::st_drop_geometry(geodata))
}

#' fortify_xy
#'
#' For use with ggplot2 (plotting polygons)
#'
#' @note Consider using `ggplot2::geom_sf()` instead.
#'
#' @param spobj `Spatial*` object
#'
#' @importFrom ggplot2 fortify
#'
#' @export
fortify_xy <- function (spobj) {
  .Deprecated("geom_sf", msg = "use geom_sf(data = ...) instead")
  spobj@data$id <- rownames(spobj@data)
  fortified <- ggplot2::fortify(spobj, region="id")
  renamed <- dplyr::rename(fortified, x = long, y = lat)
  join(renamed, spobj@data, by="id")
}

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

  .Deprecated()

  sf1 <- sf::st_as_sf(spobj1)
  sf2 <- sf::st_transform(sf::st_as_sf(spobj2), sf::st_crs(sf1))
  clipped <- sf::st_intersection(sf1, sf2)
  clipped <- sf::st_make_valid(clipped)
  return(clipped)
}
