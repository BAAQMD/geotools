#' @importFrom dplyr mutate
#'
#' @export
mutate_SpatialData <- function (.data, ...) {
  df <- .data@data
  .data@data <- dplyr::mutate(df, ...)
  return(.data)
}

#' @export
mutate.SpatialPointsDataFrame <- mutate_SpatialData

#' @export
mutate.SpatialLinesDataFrame <- mutate_SpatialData

#' @export
mutate.SpatialPolygonsDataFrame <- mutate_SpatialData
