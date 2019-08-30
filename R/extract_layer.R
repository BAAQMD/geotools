#' To extract a layer from an (unzipped) shapefile
#'
#' @param dsn data source name
#' @param layer layer name
#' @param datum (optional) passed to \code{\link[sp]{spTransform}}
#' @param quiet (optional)
#' @param \dots further arguments passed to \code{\link[rgdal]{readOGR}}
#'
#' @examples
#' \dontrun{
#'
#' basins <- extract_layer(
#'   unzip_url("http://www.arb.ca.gov/ei/gislib/boundaries/ca_air_basins.zip",
#'             exdir = file.path("cache")),
#'   layer = "CaAirBasin")
#'
#' districts <- extract_layer(
#'   unzip_url("http://www.arb.ca.gov/ei/gislib/boundaries/ca_air_district.zip",
#'             exdir = file.path("cache")),
#'   layer = "CaAirDistrict")
#'
#' counties <- extract_layer(
#'   unzip_url("http://www.arb.ca.gov/ei/gislib/boundaries/ca_county.zip",
#'             exdir = file.path("cache")),
#'   layer = "CaCounty")
#' }
#'
extract_layer <- function (dsn, layer, datum, quiet = FALSE, ...) {

  .Defunct("shptools::read_shp()")

  if (missing(layer)) layer <- basename(dsn)
  msg <- capture.output(spobj <- readOGR(normalizePath(dsn), layer, ...))
  if (!quiet) {
    message(msg)
    message(str(spobj@data))
  }
  if (!missing(datum)) spobj <- spTransform(spobj, datum)
  return(spobj)
}
