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
