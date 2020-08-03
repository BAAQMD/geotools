#' as_gridded
#'
#' @param x `sf` or `Spatial*` object
#' @param layer name of layer to grid
#' @param coord_vars names of coordinate variables
#' @param crs suitable for use with `proj4string(.) <- crs`
#'
#' @importFrom sf st_crs st_drop_geometry st_centroid st_coordinates
#' @importFrom sp gridded proj4string
#' @importFrom dplyr mutate_at
#'
#' @export
as_gridded <- function (
  x,
  layer,
  coord_vars = NULL,
  crs = NULL
) {

  if (inherits(x, "sf")) {

    crs <- sf::st_crs(x)[["proj4string"]]

    if (is.null(coord_vars)) {
      centroid_geodata <- sf::st_centroid(x)
      coord_vars <- colnames(sf::st_coordinates(centroid_geodata))[1:2]
      x <- sf::st_drop_geometry(coordinates_to_columns(centroid_geodata, coords = coord_vars))

      # HACK
      x <- dplyr::mutate_at(x, coord_vars, round)

    }

  } else if (inherits(x, "data.frame")) {
    # pass
  } else {
    stop("Don't know how to handle an object of class ", class(x))
  }

  spobj <- as_spatial(x, coord_vars = coord_vars, crs = crs)
  proj4string(spobj) <- crs
  gridded(spobj) <- TRUE
  return(spobj)

}
