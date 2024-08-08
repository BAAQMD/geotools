#' reproject
#'
#' Reproject/transform geodata.
#'
#' @param x a `Spatial*` or `sf` object
#' @param new_CRS integer, `CRS`, or `st_crs` object
#' @param new_coordnames character vector
#'
#' @details
#' If `x` is a `Spatial` object (from the `sp` package), `new_CRS` must be a `CRS` object; otherwise, pass an integer (EPSG code) or `st_crs()` result.
#'
#' @return An object of the same class as `x`, but reprojected to the new coordinate system, and with new coordnames.
#'
#' @export
reproject <- function (
    x,
    new_CRS = EPSG_4326,
    new_coordnames = c("lng", "lat")
) {
  .Deprecated("sf::st_transform")
}

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

#' st_repair
#'
#' Attempt to repair problems in geodata.
#'
#' @param geodata `sf` object
#' @param datum passed to `sf::st_transform()`
#' @param view (optional) show result using `mapview()`
#' @param verbose display messages
#'
#' @importFrom sf st_transform st_buffer st_union
#' @importFrom mapview mapview
#'
#' @export
st_repair <- function (
    geodata,
    datum = st_crs(32610),
    view = FALSE,
    verbose = TRUE
) {

  .Deprecated("sf::st_make_valid()")

  msg <- function (...) if(isTRUE(verbose)) message("[st_repair] ", ...)
  msg("repairing n = ", nrow(geodata))

  projected <-
    sf::st_transform(
      geodata,
      crs = datum)

  buffered <-
    sf::st_buffer(
      projected,
      dist = 0)

  unioned <-
    sf::st_union(
      buffered,
      by_feature = TRUE)

  repaired <-
    st_transform(
      st_filter_valid(unioned),
      st_crs(geodata))

  if (isTRUE(view)) mapview::mapview(repaired)

  return(repaired)

}

#' Coerce an `sf` object to a `tibble`
#'
#' @param geodata an `sf` object
#' @return a `tibble`
#' @export
drop_geometry <- function (geodata) {
  .Deprecated("st_drop_geometry")
  return(sf::st_drop_geometry(geodata))
}
