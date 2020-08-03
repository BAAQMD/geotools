#' Return a copy of the Spatial* object with coordnames set to
#' x and y, and a copy of the coordinates stored in its data.frame
#'
#' @param spobj `Spatial*` object
#'
#' @rdname with_xy
#' @export
with_xy.default <- function (spobj) {
  coordnames(spobj) <- c("x", "y")
  spobj$x <- coord_data(spobj)$x
  spobj$y <- coord_data(spobj)$y
  return(spobj)
}

#' S4 methods for with_xy
#'
#' @param spobj `Spatial*` object
#'
#' @docType methods
#' @rdname with_xy-methods
#' @export
setGeneric("with_xy", with_xy.default)

#' @rdname with_xy-methods
#'
#' @param spobj `Spatial*` object
#'
#' @aliases with_xy,SpatialPoints,ANY-method
setMethod("with_xy", "SpatialPoints", function (spobj) {
  coordnames(spobj) <- c("x", "y")
  SpatialPointsDataFrame(spobj, coord_data(spobj))
})

#' @rdname with_xy-methods
#' @aliases with_xy,SpatialPolygons,ANY-method
setMethod("with_xy", "SpatialPolygons", function (spobj) {
  coordnames(spobj) <- c("x", "y")
  SpatialPolygonsDataFrame(spobj, coord_data(spobj))
})

#' @rdname with_xy-methods
#' @aliases with_xy,SpatialPixels,ANY-method
setMethod("with_xy", "SpatialPixels", function (spobj) {
  coordnames(spobj) <- c("x", "y")
  SpatialPixelsDataFrame(spobj, coord_data(spobj))
})

#' @rdname with_xy-methods
#' @aliases with_xy,SpatialPixels,ANY-method
setMethod("with_xy", "SpatialPointsDataFrame", with_xy.default)

#' @rdname with_xy-methods
#' @aliases with_xy,SpatialPolygonsDataFrame,ANY-method
setMethod("with_xy", "SpatialPolygonsDataFrame", with_xy.default)

#' @rdname with_xy-methods
#' @aliases with_xy,SpatialPixelsDataFrame,ANY-method
setMethod("with_xy", "SpatialPixelsDataFrame", with_xy.default)
