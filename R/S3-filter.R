#' Filter (subset) a Spatial*DataFrame object
#'
#' @param object a \code{Spatial*DataFrame}
#' @param ... see \link{subset}
#' @param .dots
#'
#' @importFrom dplyr filter_
#' @importFrom lazyeval all_dots lazy_eval
#'
#' @return a subset of the original \code{Spatial*DataFrame}
#'
#' @seealso subset
#' @examples
#' library(dplyr)
#' library(sp)
#' data(meuse, package = "sp")
#' coordinates(meuse) <- ~x+y
#' df <- filter(meuse, copper > median(copper))
#' stopifnot(inherits(df, class(meuse)))
#'
#' @export
filter_.SpatialDataFrame <- function (object, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  masks <- lazyeval::lazy_eval(dots, data = as.data.frame(object@data))
  subset(object, Reduce(`&&`, masks))
}

filter_.SpatialLinesDataFrame <-
  filter_.SpatialDataFrame

filter_.SpatialPointsDataFrame <-
  filter_.SpatialDataFrame

filter_.SpatialPolygonsDataFrame <-
  filter_.SpatialDataFrame
