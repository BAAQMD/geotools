#' @export
sp_centroid <- function (spobj, ...) {
  .Defunct(msg = "Try sf::st_centroid() or sp::gCentroid() instead.")
  spobj %>% gCentroid(...) %>% coordinates %>% as.vector
}
