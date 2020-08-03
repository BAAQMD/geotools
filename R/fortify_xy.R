#' fortify_xy
#'
#' For use with ggplot2 (plotting polygons)
#'
#' @note Consider using `ggplot2::geom_sf()` instead.
#'
#' @param spobj `Spatial*` object
#'
#' @importFrom ggplot2 fortify
#'
#' @export
fortify_xy <- function (spobj) {
  spobj@data$id <- rownames(spobj@data)
  fortified <- ggplot2::fortify(spobj, region="id")
  renamed <- plyr::rename(fortified, list(long="x", lat="y"))
  join(renamed, spobj@data, by="id")
}
