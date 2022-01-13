#' @importFrom dplyr mutate
#'
#' @export
mutate_SpatialData <- function (.data, ...) {
  .Deprecated(msg = "upgrade code to use `sf` instead of `sp`")
  df <- .data@data
  .data@data <- dplyr::mutate(df, ...)
  return(.data)
}

#' @export
mutate.SpatialPointsDataFrame <- mutate_SpatialData

#' @export
mutate.SpatialLinesDataFrame <- mutate_SpatialData

#' @export
mutate.SpatialPolygonsDataFrame <- mutate_SpatialData

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
  .Deprecated(msg = "upgrade code to use `sf` instead of `sp`")
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

#' fortify_xy
#'
#' For use with ggplot2 (plotting polygons)
#'
#' @note Consider using `ggplot2::geom_sf()` instead.
#'
#' @param spobj `Spatial*` object
#'
#' @importFrom ggplot2 fortify
#' @importFrom plyr rename
#'
#' @export
fortify_xy <- function (spobj) {
  .Deprecated("geom_sf", msg = "use geom_sf(data = ...) instead")
  spobj@data$id <- rownames(spobj@data)
  fortified <- ggplot2::fortify(spobj, region="id")
  renamed <- plyr::rename(fortified, list(long="x", lat="y"))
  join(renamed, spobj@data, by="id")
}

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

  .Deprecated("st_transform")

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
