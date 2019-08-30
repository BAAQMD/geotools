#' @export
centroid <- function (spobj, ...) {
  spobj %>% gCentroid(...) %>% coordinates %>% as.vector
}
