#' Filter (subset) a Spatial*DataFrame object
#'
#' @param .data a \code{Spatial*DataFrame}
#' @param ... see \link{subset}
#' @param .dots
#'
#' @importFrom dplyr filter_
#' @importFrom lazyeval all_dots lazy_eval
#'
#' @return a subset of the original \code{Spatial*DataFrame}
#'
#' @seealso subset
#'
#' @export
filter_.SpatialDataFrame <- function (.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  masks <- lazyeval::lazy_eval(dots, data = as.data.frame(.data@data))
  subset(.data, Reduce(`&&`, masks))
}

filter_.SpatialLinesDataFrame <-
  filter_.SpatialDataFrame

filter_.SpatialPointsDataFrame <-
  filter_.SpatialDataFrame

filter_.SpatialPolygonsDataFrame <-
  filter_.SpatialDataFrame
