#' raster_bind
#'
#' @param rst RasterLayer
#' @param ... zero or more additional RasterLayers
#' @param fill ignored
#' @param outfile character
#' @param verbose logical
#'
#' @aliases bind_rasters
#'
#' @return
#' @export
raster_bind <- function (
  rst,
  ...,
  fill = NA,
  outfile = NULL,
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) message("[bind_rasters] ", ...)

  raster_list <-
    append(rst, list(...))

  names(raster_list) <-
    map(raster_list, names)

  extent_list <-
    map(raster_list, raster::extent)

  merged_extent <-
    raster::extent(reduce(raster_list, raster::merge))

  expanded_list <-
    map(raster_list, raster::extend, merged_extent)

  stacked <-
    reduce(expanded_list, raster::addLayer)

  if (isFALSE(is.null(outfile))) {
    msg("writing to: ", outfile)
    raster::writeRaster(stacked, filename = outfile, overwrite = TRUE)
  }

  return(stacked)

}
