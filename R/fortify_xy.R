#' For use with ggplot2 (plotting polygons)
#'
#' @export
fortify_xy <- function (spobj) {
  spobj@data$id <- rownames(spobj@data)
  fortified <- ggplot2::fortify(spobj, region="id")
  renamed <- plyr::rename(fortified, list(long="x", lat="y"))
  join(renamed, spobj@data, by="id")
}
