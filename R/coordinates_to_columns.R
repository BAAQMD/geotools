#' Copy coordinates into (two) columns
#'
#' @param geodata (sf or sfc)
#' @param coords (character) names of columns
#' @param ... further arguments
#' @param verbose (logical)
#'
#' @export
coordinates_to_columns <- function (geodata, coords, ..., verbose = getOption("verbose")) {

  d <- length(coords)
  xy_coords <- matrix(st_coordinates(geodata)[,1:d], ncol = d)
  coord_data <- set_names(as.data.frame(xy_coords), coords)
  bind_cols(geodata, coord_data)

}
