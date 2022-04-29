#' @importFrom dplyr mutate
#' @importFrom sf st_as_sf
mutate_SpatialData <- function (.data, ...) {
  mutated <- dplyr::mutate(sf::st_as_sf(.data), ...)
  if (inherits(.data, "Spatial")) {
    mutated <- as(mutated, "Spatial")
  }
  return(mutated)
}

#' @export
mutate.SpatialPointsDataFrame <- mutate_SpatialData

#' @export
mutate.SpatialLinesDataFrame <- mutate_SpatialData

#' @export
mutate.SpatialPolygonsDataFrame <- mutate_SpatialData
