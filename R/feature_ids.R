#' To get the IDs of features in a SpatialPolygons* object
#'
#' @param spobj SpatialPolygons* object
#' @param \dots (unused)
#' @rdname feature_ids
#' @export
feature_ids <- function (spobj, ...) {
  UseMethod("feature_ids")
}

#' @rdname feature_ids
#' @method feature_ids SpatialPolygons
feature_ids.SpatialPolygons <- function (spobj, ...) {
  sapply(spobj@polygons, slot, "ID")
}

#' @rdname feature_ids
#' @method feature_ids SpatialPolygonsDataFrame
feature_ids.SpatialPolygonsDataFrame <- function (spobj, ...) {
  feature_ids(as(spobj, "SpatialPolygons"), ...)
}
