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
  .Defunct(msg = "upgrade code to use `sf` instead of `sp`")
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

#' reproject
#'
#' Reproject/transform geodata.
#'
#' @param x a `Spatial*` or `sf` object
#' @param new_CRS integer, `CRS`, or `st_crs` object
#' @param new_coordnames character vector
#'
#' @details
#' If `x` is a `Spatial` object (from the `sp` package), `new_CRS` must be a `CRS` object; otherwise, pass an integer (EPSG code) or `st_crs()` result.
#'
#' @return An object of the same class as `x`, but reprojected to the new coordinate system, and with new coordnames.
#'
#' @export
reproject <- function (
    x,
    new_CRS = EPSG_4326,
    new_coordnames = c("lng", "lat")
) {

  .Defunct("sf::st_transform")

  require(rgdal)

  if (is.character(new_CRS)) {
    new_CRS <- CRS(new_CRS)
  }

  if (inherits(x, "sf")) {

    reprojected <- sf::st_transform(x, new_CRS)
    if (!missing(new_coordnames)) {
      warning("Not handling new_coordnames since your data is `sf` rather than `Spatial`")
    }

  } else if (inherits(x, "Spatial")) {

    reprojected <- sp::spTransform(x, new_CRS)
    if (!missing(new_coordnames)) {
      sp::coordnames(reprojected) <- new_coordnames
    }

  } else {
    stop("Don't know how to handle that class of object")
  }

  return(reprojected)

}

clip_and_trim <- function (rst, lower = .Machine$double.eps, upper = Inf) {
  .Defunct()
  clamped <- raster::clamp(rst, -Inf, upper, useValues = TRUE)
  clipped <- raster::clamp(clamped, lower, Inf, useValues = FALSE)
  raster::trim(clipped)
}

#' Force replacement of CRS while leaving coordinate values unchanged
#'
#' @param geodata `sf` or `sfc` object
#' @param crs passed to `sf::st_as_sf()``
#' @param ... further arguments to [sf::st_as_sf()]
#' @param verbose display messages
#'
#' @importFrom sf st_as_sf
#'
#' @export
force_crs <- function (
    geodata,
    crs,
    ...,
    verbose = getOption("verbose")
) {

  .Defunct("sf::st_set_crs()")

  msg <- function (...) if(isTRUE(verbose)) message("[force_crs] ", ...)

  # Only used in this function, temporarily.
  coord_vars <- c(".x_coord", ".y_coord")

  #
  # These are all helper function from `geotools`:
  #
  # - is_geodata()
  # - coordinates_to_columns()
  # - drop_geometry()
  #
  stopifnot(is_geodata(geodata))
  fortified <- coordinates_to_columns(geodata, coord_vars)
  naive <- sf::st_drop_geometry(fortified)

  # Coerce it back into an `sf` object
  #
  # FIXME: not designed to handle the case where `geodata` inherits from `sfc`
  #
  if (inherits(geodata, "sfc")) {
    warning("[force_crs] casting back to `sf`, not `sfc`")
  }

  coerced <-
    sf::st_as_sf(
      naive,
      coords = coord_vars,
      crs = crs,
      ...)

  return(coerced)

}

#' gContainsOrOverlaps
#'
#' This is the default predicate used by `gFilter()`.
#'
#' @param geom2 `Spatial*` object
#' @param geom1 `Spatial*` object
#' @param byid passed to `rgeos::gContains()` and `rgeos::gOverlaps()`
#'
#' @note Unlike most `rgeos` functions, this returns row.names !!
#'
#' @importFrom rlang is_empty
#' @importFrom rgeos gContains gOverlaps
#'
#' @export
gContainsOrOverlaps <- function (geom2, geom1, byid = TRUE) {

  .Defunct()

  # First pass: test for *containment*
  ij1 <- rgeos::gContains(geom2, geom1, byid = byid)
  is_contained <- as.logical(rowSums(ij1))

  # Preliminary result: may be modified by second pass (below)
  result <- row.names(geom1)[is_contained]

  # Second pass: test for *overlap* --- but only consider features
  # not already passing the first test (i.e., containment)
  remainder <- geom1[which(!is_contained), ]
  if (rlang::is_empty(remainder)) {
    # do nothing
  } else {
    ij2 <- rgeos::gOverlaps(geom2, remainder, byid = byid)
    is_overlapping <- as.logical(rowSums(ij2))
    result <- union(result, row.names(remainder[is_overlapping]))
  }

  # Final result
  return(result)

}

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
  .Defunct("filter_spatial() or sf::st_filter()")
  i <- which(as.logical(rgeos::gIntersects(spobj2, spobj1, byid=TRUE)))
  return(spobj1[i,])
}

