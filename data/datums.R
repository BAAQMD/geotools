#'----------------------------------------------------------------------
#'
#' See also:
#'
#' - `geotools-package.R` for associated documentation.
#' - `data/CMAQ_CRS.rda` (and `proj4string_LCC()`)
#'
#'----------------------------------------------------------------------

# WGS 84 / UTM zone 10N, https://epsg.io/32610
UTM10_CRS   <- sf::st_crs(32610)

# NAD83 / UTM zone 10N, https://epsg.io/26910
UTM10_NAD83 <- sf::st_crs(26910)

UTM10_NAD27 <- sf::st_crs("+proj=utm +zone=10 +datum=NAD27 +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.da")

# Most typical for lon/lat
WGS84_CRS   <- sf::st_crs(4326)
WGS84_GPS   <- sf::st_crs(4326)

# Web mapping
WEB_MERCATOR <- sf::st_crs(3857)

CA_ALBERS_NAD83 <- sf::st_crs(3310)
