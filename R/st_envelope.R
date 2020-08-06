#' st_envelope
#'
#' Return bounding box of `obj` as a polygon
#'
#' @param obj simple feature or simple feature set, passed to [sf::st_bbox()]
#' @param ... passed tp [sf::st_bbox]
#'
#' @export
st_envelope <- function (obj, ...) {
  bb <- sf::st_bbox(obj, ...)
  envelope <- st_as_sfc(bb)
  return(envelope)
}
