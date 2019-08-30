#' Merge generic
#'
#' @param y data.frame
#' @param by character
#' @param \dots further arguments, as in \link{merge} generally (e.g. \code{sort})
#'
#' @return an object of the same class as \code{x}
#'
#' @export
merge_SpatialDataFrame <- function (x, y, by, ...) {

  .Deprecated("Use sp::merge() instead")

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

#' #' Merge a SpatialPolygonsDataFrame with a data.frame
#' #'
#' #' @docType methods
#' #' @rdname merge-methods
#' #' @export
#' setMethod(
#'   "merge",
#'   signature = c(x = "SpatialPolygonsDataFrame", y = "data.frame"),
#'   definition = function (x, y, by, ...) {
#'     y <- as.data.frame(y)
#'     if (missing(by)) {
#'       if (is.null(row.names(x@data)) || is.null(row.names(y))) {
#'         warning("[merge.SpatialPolygonsDataFrame] merging by position")
#'         i <- 1:nrow(x@data)
#'       } else {
#'         warning("[merge.SpatialPolygonsDataFrame] merging by row names")
#'         i <- row.names(x@data)
#'       }
#'       new_data <- data.frame(x@data, y[i, ])
#'     } else {
#'       message("[merge.SpatialPolygonsDataFrame] merging by ", by)
#'       j <- which(names(y) == by)
#'       i <- match(x@data[, by], y[, j])
#'       new_data <- data.frame(x@data, y[i, -j])
#'     }
#'     row.names(new_data) <- row.names(x@data)
#'     SpatialPolygonsDataFrame(geometry(x), new_data)
#'   })
#'
#' #' Merge a SpatialPolygons with a data.frame
#' #'
#' #' @docType methods
#' #' @rdname merge-methods
#' #' @export
#' setMethod(
#'   "merge",
#'   signature = c(x = "SpatialPolygons", y = "data.frame"),
#'   definition = function (x, y, by, ...) {
#'     y <- as.data.frame(y)
#'     j <- which(names(y) == by)
#'     i <- match(row.names(x), y[, j])
#'     SpatialPolygonsDataFrame(geometry(x), y[i, -j])
#'   })

