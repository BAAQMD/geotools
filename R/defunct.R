#' sp_centroid
#'
#' Extract the coordinates, as a simple vector, of the centroid of a Spatial* object.
#'
#' @param spobj `Spatial*` object
#' @param ... passed to `gCentroid()`
#'
#' @importFrom rgeos gCentroid
#'
#' @export
sp_centroid <- function (spobj, ...) {
  .Defunct(msg = "Try sf::st_centroid() or sp::gCentroid() instead.")
  spobj %>% rgeos::gCentroid(...) %>% coordinates %>% as.vector
}

#' Merge generic
#'
#' @param y data.frame
#' @param by character
#' @param \dots further arguments, as in \link{merge} generally (e.g. \code{sort})
#'
#' Calculate the area of each polygon in a SpatialPolygons* object
#'
#' @param spdf `SpatialPolygons*` object
#'
#' @export
polygon_areas <- function (spdf) {
  .Defunct("st_area")
  sapply(spdf@polygons, function(x) x@area / 1e6)
}

#' merge_SpatialDataFrame
#'
#' @return an object of the same class as \code{x}
#'
#' @export
merge_SpatialDataFrame <- function (x, y, by, ...) {

  .Defunct("sp::merge")

  if (!identical(row.names(x), row.names(x@data)))
    warning("row.names(x) and row.names(x@data) are not identical")
  if (by == "row.names")
    stop("merge_SpatialDataFrame doesn't support merging by row.names (yet)")

  merged_data <- merge(x@data, y, by = by, ...)
  i <- merged_data[[by]]
  row.names(merged_data) <- i
  merged_geom <- geometry(x)[i, ]

  spobj_constructor <- get(class(x))
  stopifnot(is.function(spobj_constructor))
  new_obj <- spobj_constructor(merged_geom, merged_data)

  stopifnot(identical(
    row.names(new_obj),
    row.names(merged_geom)))

  return(new_obj)

}

#' @describeIn merge_SpatialDataFrame Merge a SpatialPolygonsDataFrame with a data.frame
#' @param x a \code{SpatialPolygonsDataFrame} object
merge_SpatialPolygonsDataFrame <- merge_SpatialDataFrame

setMethod(
  "merge",
  c("SpatialPolygonsDataFrame", "data.frame"),
  merge_SpatialPolygonsDataFrame)

#' @describeIn merge_SpatialDataFrame Merge a SpatialPolygonsDataFrame with a data.frame
#' @param x a \code{SpatialLinesDataFrame} object
merge_SpatialLinesDataFrame <- merge_SpatialDataFrame

setMethod(
  "merge",
  c("SpatialLinesDataFrame", "data.frame"),
  merge_SpatialLinesDataFrame)
