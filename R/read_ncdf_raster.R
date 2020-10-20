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
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) {
    message("[read_ncdf_raster] ", ...)
  }

  stopifnot(xfun::file_ext(path) == "nc")

  ncdf_globals <-
    ncdf_path %>%
    ncmeta::nc_atts() %>%
    pull(value)

  ncdf_proj4string <-
    with(ncdf_globals, {
      proj4string_LCC(
        lon_0 = XCENT,
        lat_0 = YCENT,
        lat_1 = P_ALP,
        lat_2 = P_BET,
        x_0   = -XORIG + (XCELL / 2),
        y_0   = -YORIG + (YCELL / 2),
        a     = 6.37e6, # NWS84 spheroid
        b     = 6.37e6, # NWS84 spheroid
        units = "km",
        verbose = verbose)
    })

  raster_layer <- raster::raster(ncdf_path)
  raster::crs(raster_layer) <- ncdf_proj4string

  return(raster_layer)

}
