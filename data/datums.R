#
# Note: see `geotools-package.R` for associated documentation.
#

NAD83_UTM10 <- 26910

UTM10_NAD83 <-
  sp::CRS("+init=epsg:26910")

UTM10_NAD27 <-
  sp::CRS("+proj=utm +zone=10 +datum=NAD27 +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.da")

WGS84_GPS <- 4326

WGS84 <-
  sp::CRS("+init=epsg:4326")

EPSG_4326 <-
  sp::CRS("+init=epsg:4326")

EPSG_3310 <-
  sp::CRS("+init=epsg:3310")

EPSG_32610 <-
  sp::CRS("+init=epsg:32610")

UTM10 <-
  sp::CRS("+init=epsg:32610")

CA_ALBERS_NAD83 <-
  sp::CRS("+init=epsg:3310")

WEB_MERCATOR <-
  sf::st_crs(3857)
