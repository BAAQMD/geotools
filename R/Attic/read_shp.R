#' Read a layer from a shapefile
#'
#' @param dsn usually the name of a folder/directory
#' @param layer layer name
#' @param verbose,stringsAsFactors arguments passed to rgdal::readOGR
#' @param \dots further arguments to readOGR
#' @return a \code{Spatial*} object
#' @importFrom rgdal readOGR
#'
#' @examples
#' dsn <- "~/GitHub/BAAQMD/ARB/data-raw/CoAbDis.zip"
#' read_shp(dsn, layer = "CoAbDis")
#'
#' @export
read_shp <- function (dsn, layer, stringsAsFactors = FALSE, ..., verbose = getOption("verbose")) {

  msg <- function (...) if(isTRUE(verbose)) message("[read_shp] ", ...)

  try(dsn <- normalizePath(dsn)) # because `readOGR()` doesn't like "~" (home directory)
  msg("reading layer ", layer, " from ", dsn)

  is_zipfile <- function (file) str_detect(file, fixed("\\.zip$", ignore_case = TRUE))

  #
  # Allow for `dsn` to name a zipped shapefile.
  #
  # In this case, extract the contents to a (temporary) directory
  # and then proceed as normal.
  #
  if (is_zipfile(dsn)) {
    tmp_dn <- unzipped(dsn, pattern = layer, junkpaths = TRUE)
  }

  sp_obj <- rgdal::readOGR(dsn, layer, stringsAsFactors = stringsAsFactors, ..., verbose = verbose)
  sf_obj <- sf::st_as_sf(sp_obj)
  return(sf_obj)

}
