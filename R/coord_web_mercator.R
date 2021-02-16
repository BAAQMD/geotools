#' coord_web_mercator
#'
#' @param envelope sf or sfc
#' @param xlim numeric (two values), if `envelope` isn't provided
#' @param ylim numeric (two values), if `envelope` isn't provided
#'
#' @return output of `coord_sf(...)`
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
