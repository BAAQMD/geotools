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

