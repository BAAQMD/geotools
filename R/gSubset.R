#' Subset a Spatial* object using a SpatialPolygons* object
#'
#' @export
gSubset <- function (spobj1, spobj2) {
  .Deprecated("Use filter_spatial() instead")
  require(rgeos)
  i <- which(as.logical(gIntersects(spobj2, spobj1, byid=TRUE)))
  return(spobj[i,])
}
