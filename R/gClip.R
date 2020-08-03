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
#' @importFrom sf st_as_sf st_transform st_intersection
#'
#' @export
gClip <- function (spobj1, spobj2, ...) {
  sf1 <- sf::st_as_sf(spobj1)
  sf2 <- sf::st_transform(st_as_sf(spobj2), st_crs(sf1))
  clipped <- sf::st_intersection(sf1, sf2)
  return(clipped)
}
#
# gClip_v1 <- function (spobj1, spobj2, min_area = 1e-6, checkValidity = TRUE, verbose = TRUE) {
#
#   msg <- function (...) if (isTRUE(verbose)) message("[gClip] ", ...)
#
#   cls1 <- class(spobj1)
#   if (cls1 %>% str_detect("SpatialPoints")) {
#     err_msg <- str_c("That's a ", cls1, " object --- maybe you want to use filter_spatial() instead?")
#     stop(err_msg)
#   }
#
#   if (!inherits(spobj2, "SpatialPolygons")) {
#     err_msg <- "gClip() can only clip using SpatialPolygons"
#     stop(err_msg)
#   }
#
#   clip_region <-
#     gUnionCascaded(geometry(spobj2)) %>%
#     reproject(CRS(proj4string(spobj1)))
#
#   clipped_geom <-
#     gIntersection(spobj1, clip_region, byid = TRUE)
#
#   clipped_ids <-
#     row.names(clipped_geom) %>%
#     str_replace_all(" [0-9]+$", "")
#
#   row.names(clipped_geom) <-
#     clipped_ids
#
#   if ("data" %in% slots(spobj1)) {
#
#     clipped_data <- spobj1@data[clipped_ids, ]
#     clipped_geodata <-
#        spChFIDs(clipped_geom, clipped_ids),
#     )
#   }
#
#
#
#   return(clipped_geodata)
#
# }
