#' Apply a spatial filter
#'
#' @param x ([Spatial][sp::Spatial] object) to be filtered
#' @param y ([Spatial][sp::Spatial] object) to filter with
#' @param FUN (function) a spatial predicate, for example: [gIntersects][rgeos::gIntersects]
#' @param ... further arguments to `FUN`
#' @param verbose (logical)
#'
#' @importFrom sf st_as_sf st_transform st_union st_crs st_within
#'
#' @return (typically) a subset of \code{spobj1}
#'
#' @export
filter_spatial <- function (
  x,
  y,
  FUN = NULL,
  crs = NULL,
  ...,
  verbose = getOption("verbose")
) {

  msg <- function (...) if (isTRUE(verbose)) message("[filter_spatial] ", ...)

  if (is.null(FUN)) {
    FUN <- sf::st_within
  }

  if (is.null(crs)) {
    crs <- sf::st_crs(x)
  }

  filtered <-
    sf::st_filter(
      sf::st_as_sf(x),
      sf::st_transform(sf::st_as_sf(y), crs),
      .predicate = FUN,
      ...)

  if (inherits(x, "Spatial")) {
    filtered <- as(filtered, "Spatial")
  }

  return(filtered)

}
