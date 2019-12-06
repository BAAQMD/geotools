#' @export
sp_centroid <- function (spobj, ...) {
  .Defunct(msg = "Try sf::st_centroid() or sp::gCentroid() instead.")
  spobj %>% gCentroid(...) %>% coordinates %>% as.vector
}

#' @export
write_shapefile <- function (
  spobj,
  dsn = getwd(),
  layer = deparse(substitute(spobj)),
  ...,
  overwrite_layer = TRUE,
  verbose = TRUE
) {

  .Defunct("shptools::write_shp()")

  if (!dir.exists(dsn)) {
    if (verbose) warning("Creating directory ", dsn)
    dir.create(dsn, recursive = TRUE)
  }

  outfile <- file.path(dsn, str_c(layer, ".shp"))
  if (verbose) message("Writing to ", outfile)

  success <- writeOGR(
    spobj, dsn, layer, ...,
    driver = "ESRI Shapefile",
    overwrite_layer = overwrite_layer)

  return(invisible(spobj)) # for chaining

}

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
