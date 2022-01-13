#' Calculate the area of each polygon in a SpatialPolygons* object
#'
#' @param spdf `SpatialPolygons*` object
#'
#' @export
polygon_areas <- function (spdf) {
  .Defunct("st_area")
  sapply(spdf@polygons, function(x) x@area / 1e6)
}
