#' coord_web_mercator
#'
#' @param envelope
#' @param xlim
#' @param ylim
#'
#' @return
#' @export
#'
coord_web_mercator <- function (envelope = NULL, xlim = NULL, ylim = NULL) {
  if (isFALSE(is.null(envelope))) {
    bbox <- st_bbox(st_transform(st_envelope(envelope), WEB_MERCATOR))
    xlim <- bbox[c("xmin", "xmax")]
    ylim <- bbox[c("ymin", "ymax")]
  }
  return(coord_sf(crs = WEB_MERCATOR, datum = WEB_MERCATOR, xlim = xlim, ylim = ylim))
}
