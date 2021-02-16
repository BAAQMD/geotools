#' cut_raster
#'
#' @param raster_layer 
#' @param breaks 
#' @param include.lowest 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
cut_raster <- function (
  raster_layer,
  breaks,
  include.lowest = TRUE,
  ...,
  na.rm = TRUE,
  verbose = getOption("verbose", default = FALSE)
) {
  
  cut_values <-
    cut(
      raster::values(raster_layer),
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
  
  cut_layer <- raster::raster(raster_layer)
  raster::values(cut_layer) <- as.integer(cut_values)
  cut_layer <- raster::as.factor(cut_layer)
  levels(cut_layer) <- attr_data # see ?raster::factorValues
  
  return(cut_layer)
  
}
