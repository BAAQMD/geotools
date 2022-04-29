#' @importFrom dplyr select_vars
#'
select_SpatialData <- function (.data, ...) {
  selected <- dplyr::select(sf::st_as_sf(.data), ...)
  if (inherits(.data, "Spatial")) {
    selected <- as(selected, "Spatial")
  }
  return(selected)
}

#' @export
select.SpatialPointsDataFrame <- select_SpatialData

#' @export
select.SpatialLinesDataFrame <- select_SpatialData

#' @export
select.SpatialPolygonsDataFrame <- select_SpatialData
