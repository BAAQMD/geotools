#' @docType package
#'
#' @importClassesFrom sp SpatialPointsDataFrame SpatialLinesDataFrame SpatialPolygonsDataFrame
#'
#' @import utils
#' @import methods
#'
#' @import readr
#' @import dplyr
#' @import httr
#' @import purrr
#' @import tibble
#' @import stringr
#' @import rlang
#'
#' @import rgdal
#' @import sp
#' @import sf
#'
#' @import funtools
#' @import shptools
#'
#' @import units
#'
#' @import ARB
#'
#' @importFrom methods as slot slotNames
#' @importFrom utils download.file unzip
#'
#' @importFrom funtools rbind_list
#'
NULL

#' @name datums
#' @rdname datums
#' @export
NULL

#' EPSG 4326
#'
#' @docType data
#' @rdname EPSG_4326
#' @name EPSG_4326
#' @description WGS84 (EPSG 4326)
#' @format an `sp::CRS` object
'EPSG_4326'

#' WGS84 GPS (EPSG:4326)
#'
#' @docType data
#' @rdname EPSG_4326
#' @name WGS84_GPS
#' @description WGS84 (EPSG 4326)
#' @format integer (EPSG code)
'WGS84_GPS'

#' UTM10 NAD83 (EPSG:26910)
#'
#' @docType data
#' @rdname EPSG_26910
#' @name NAD83_UTM10
#' @description Universal Transverse Mercator (UTM), Zone 10, North American Datum (NAD) 1983
#' @format integer (EPSG code)
'NAD83_UTM10'

#' UTM10 NAD83 (EPSG:26910)
#'
#' @docType data
#' @rdname EPSG_26910
#' @name UTM10_NAD83
#' @description Universal Transverse Mercator (UTM), Zone 10, North American Datum (NAD) 1983
#' @format an `sp::CRS` object
'UTM10_NAD83'

#' UTM10 NAD27 (EPSG:26910)
#'
#' @docType data
#' @rdname EPSG_26910
#' @name UTM10_NAD27
#' @description Universal Transverse Mercator (UTM), Zone 10, North American Datum (NAD) 1927
#' @format an `sp::CRS` object
'UTM10_NAD27'

#' California Albers (EPSG:3310)
#'
#' @docType data
#' @name CA_ALBERS_NAD83
#' @description California Albers (EPSG 3310)
#' @format an `sp::CRS` object
'CA_ALBERS_NAD83'
