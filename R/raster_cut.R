#' raster_cut
#'
#' @param rst [RasterLayer](raster::Raster-class) object
#' @param breaks passed to [cut()]
#' @param include.lowest passed to [cut()]
#' @param ... passed to [cut()]
#' @param na.rm logical; passed to [cut()]
#' @param verbose logical
#'
#' @return a new [RasterLayer](raster::Raster-class) object, of the same dimensions, etc., with values cut accordingly
#' @export
#'
raster_cut <- function (
  rst,
  breaks,
  include.lowest = TRUE,
  ...,
  na.rm = TRUE,
  verbose = getOption("verbose", default = FALSE)
) {

  cut_values <-
    cut(
      raster::values(rst),
      breaks = breaks,
      na.rm = na.rm,
      ...)

  attr_data <-
    data.frame(
      ID = seq_along(levels(cut_values)),
      lower = head(breaks, -1),
      upper = tail(breaks, -1))

  if (isFALSE(is.null(names(breaks)))) {
    if (isTRUE(include.lowest)) {
      break_names <- head(names(breaks), -1)
    } else {
      break_names <- tail(names(breaks), -1)
    }
    attr_data$label <- break_names
  }

  cut_layer <- raster::raster(rst)
  raster::values(cut_layer) <- as.integer(cut_values)
  cut_layer <- raster::as.factor(cut_layer)
  levels(cut_layer) <- attr_data # see ?raster::factorValues

  return(cut_layer)

}
