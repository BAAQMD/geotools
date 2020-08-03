#' gSubset
#'
#' Subset a Spatial* object using a SpatialPolygons* object
#'
#' @param spobj1 `Spatial*` object
#' @param spobj2 `SpatialPolygons*` object
#'
#' @importFrom rgeos gIntersects
#'
#' @export
gSubset <- function (spobj1, spobj2) {
  .Deprecated("Use filter_spatial() instead")
  i <- which(as.logical(rgeos::gIntersects(spobj2, spobj1, byid=TRUE)))
  return(spobj1[i,])
}
