#' Reproject/transform a Spatial* object
#'
#' @export
reproject <- function (x, new_CRS = EPSG_4326, new_coordnames = c("lng", "lat")) {

  require(rgdal)

  if (is.character(new_CRS)) {
    new_CRS <- CRS(new_CRS)
  }

  if (inherits(x, "sf")) {

    reprojected <- st_transform(x, new_CRS)
    if (!missing(new_coordnames)) {
      warning("Not handling new_coordnames since your data is `sf` rather than `Spatial`")
    }

  } else if (inherits(x, "Spatial")) {

    reprojected <- spTransform(x, new_CRS)
    if (!missing(new_coordnames)) {
      coordnames(reprojected) <- new_coordnames
    }

  } else {
    stop("Don't know how to handle that class of object")
  }

  return(reprojected)

}
