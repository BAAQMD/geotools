#' read_ncdf_raster
#'
#' @param path character
#' @param ... reserved for future use
#' @param verbose logical
#'
#' @importFrom ncmeta nc_atts
#' @importFrom xfun file_ext
#'
#' @return `SpatRaster` object
#' @export
read_ncdf_raster <- function (
  path,
  ...,
  layer_name = NULL,
  crs = NULL,
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) {
    message("[read_ncdf_raster] ", ...)
  }

  stopifnot(xfun::file_ext(path) == "nc")

  ncdf_globals <-
    ncmeta::nc_atts(path) %>%
    pull(value)

  quieted <- purrr::quietly(terra::rast)(path, ...)
  msg(quieted$output)
  rst_obj <- quieted$result

  if (is.null(crs)) {

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
          #units = "m",
          verbose = verbose)
      })

    terra::crs(rst_obj) <-
      ncdf_proj4string

  } else {

    terra::crs(rst_obj) <- crs

  }

  terra::ext(rst_obj) <- local({

    ncdf_bb <-
      with(
        ncdf_globals,
        c(xmin = XORIG,
          xmax = XORIG + XCELL * NCOLS,
          ymin = YORIG,
          ymax = YORIG + YCELL * NROWS))

    terra::ext(ncdf_bb)

  })

  # mapview::mapview(st_cast(st_envelope(ncdf_bb, crs = ncdf_proj4string), "MULTILINESTRING")) + mapview::mapview(rst_obj)

  if (isFALSE(is.null(layer_name))) {
    msg("setting names to: ", layer_name)
    names(rst_obj) <- layer_name
  }

  attr(rst_obj, "ncdf_globals") <-
    ncdf_globals

  return(rst_obj)

}
