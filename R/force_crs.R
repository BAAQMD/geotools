#' Force replacement of CRS while leaving coordinate values unchanged
#'
#' @param geodata (sf or sfc)
#' @param coords (character) names of the (two) columns containing coordinates
#' @param ... further arguments to [sf::st_as_sf()]
#' @param verbose (logical)
#'
#' @export
force_crs <- function (geodata, crs, ..., verbose = getOption("verbose")) {

  msg <- function (...) if(isTRUE(verbose)) message("[force_crs] ", ...)

  # Only used in this function, temporarily.
  coord_vars <- c(".x_coord", ".y_coord")

  #
  # These are all helper function from `geotools`:
  #
  # - is_geodata()
  # - coordinates_to_columns()
  # - drop_geometry()
  #
  stopifnot(is_geodata(geodata))
  fortified <- coordinates_to_columns(geodata, coord_vars)
  naive <- drop_geometry(fortified)

  # Coerce it back into an `sf` object
  #
  # FIXME: not designed to handle the case where `geodata` inherits from `sfc`
  #
  if (inherits(geodata, "sfc")) warning("[force_crs] casting back to `sf`, not `sfc`")
  coerced <- st_as_sf(naive, coords = coord_vars, crs = crs, ...)
  return(coerced)

}
