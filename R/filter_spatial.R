#' filter_spatial
#'
#' A slightly more convenient version of [sf::st_filter()]. It does the
#' job of reprojecting the second object to the same CRS as the first,
#' if that's needed.
#'
#' @param x object to be filtered
#' @param y object to filter with
#' @param FUN (function) a spatial predicate, for example: [sf::st_intersects()]
#' @param ... further arguments to [sf::st_filter()]
#' @param verbose (logical)
#'
#' @importFrom sf st_as_sf st_transform st_filter
#'
#' @return (typically) a subset of \code{x}
#'
#' @export
filter_spatial <- function (
  x,
  y,
  FUN = st_intersects,
  ...,
  verbose = getOption("verbose")
) {

  msg <- function (...) if (isTRUE(verbose)) message("[filter_spatial] ", ...)

  # if needed, try converting `x` to a spatial object
  if (!any(class(x) %in% c("sfc"))) {
    x <- sf::st_as_sf(x)
  }

  # if needed, try converting `y` to a spatial object
  if (!any(class(y) %in% c("sfc"))) {
    y <- sf::st_as_sf(y)
  }

  # if needed, reproject y to have the same CRS as x
  if (st_crs(y) != st_crs(x)) {
    y <- st_transform(y, st_crs(x))
  }

  filtered <-
    sf::st_filter(
      x,
      y,
      .predicate = FUN,
      ...)

  return(filtered)

}
