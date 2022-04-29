#' @importFrom dplyr filter
#' @importFrom sf st_as_sf
filter_SpatialData <- function (.data, ...) {
  filtered <- dplyr::filter(sf::st_as_sf(.data), ...)
  if (inherits(.data, "Spatial")) {
    filtered <- as(filtered, "Spatial")
  }
  return(filtered)
}

#' @export
filter.SpatialPointsDataFrame <- filter_SpatialData

#' @export
filter.SpatialLinesDataFrame <- filter_SpatialData

#' @export
filter.SpatialPolygonsDataFrame <- filter_SpatialData
