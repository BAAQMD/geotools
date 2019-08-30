#' @importFrom dplyr select_vars
#'
select_SpatialData <- function (.data, ...) {
  j <- dplyr::select_vars(names(.data), ...)
  selected <- .data[, j]
  names(selected) <- names(j)
  return(selected)
}

#' @export
select.SpatialPointsDataFrame <- select_SpatialData

#' @export
select.SpatialLinesDataFrame <- select_SpatialData

#' @export
select.SpatialPolygonsDataFrame <- select_SpatialData
