#' read_ncdf_raster
#'
#' @param path character
#' @param ... reserved for future use
#' @param verbose logical
#'
#' @importFrom ncmeta nc_atts
#' @importFrom xfun file_ext
#'
#' @return `RasterLayer` object
#' @export
read_ncdf_raster <- function (
  path,
  ...,
  layer_name = NULL,
  units = "m",
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) {
    message("[read_ncdf_raster] ", ...)
  }

  stopifnot(xfun::file_ext(path) == "nc")

  ncdf_globals <-
    ncmeta::nc_atts(path) %>%
    pull(value)

  # if (isTRUE(verbose)) {
  #   print(ncdf_globals)
  # }

  ncdf_proj4string <-
    with(ncdf_globals, {
      proj4string_LCC(
        lon_0 = XCENT,
        lat_0 = YCENT,
        lat_1 = P_ALP,
        lat_2 = P_BET,
        x_0   = 0, #-XORIG, # + (XCELL / 2),
        y_0   = 0, #-YORIG, # + (YCELL / 2),
        a     = 6.37e6, # NWS84 spheroid: radius 6,370 km
        b     = 6.37e6, # NWS84 spheroid: radius 6,370 km
        units = "m",
        verbose = verbose)
    })

  ncdf_bb <-
    with(
      ncdf_globals,
      c(xmin = XORIG,
        xmax = XORIG + XCELL * NCOLS,
        ymin = YORIG,
        ymax = YORIG + YCELL * NROWS))


  quieted <- purrr::quietly(raster::raster)(path, ...)
  msg(quieted$output)
  raster_layer <- quieted$result

  raster::crs(raster_layer) <- ncdf_proj4string
  raster::extent(raster_layer) <- st_extent(ncdf_bb)

  # mapview::mapview(st_cast(st_envelope(ncdf_bb, crs = ncdf_proj4string), "MULTILINESTRING")) + mapview::mapview(raster_layer)

  if (isFALSE(is.null(layer_name))) {
    msg("setting names to: ", layer_name)
    names(raster_layer) <- layer_name
  }

  attr(raster_layer, "ncdf_globals") <-
    ncdf_globals

  return(raster_layer)

}
