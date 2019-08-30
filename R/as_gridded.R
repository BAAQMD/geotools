#' @export
as_gridded <- function (x, layer, coord_vars = NULL, crs = NULL) {

  if (inherits(x, "sf")) {

    crs <- st_crs(x)[["proj4string"]]

    if (is.null(coord_vars)) {
      centroid_geodata <- st_centroid(x)
      coord_vars <- colnames(st_coordinates(centroid_geodata))[1:2]
      x <- drop_geometry(coordinates_to_columns(centroid_geodata, coords = coord_vars))

      # HACK
      x <- mutate_at(x, coord_vars, round)

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
