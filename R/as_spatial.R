#' as_spatial
#'
#' Create a SpatialPointsDataFrame from tabular data
#'
#' @param input_data tabular data
#' @param coord_vars names of variables in `input_data` that contain coordinate values
#' @param crs as would be returned by `CRS()`
#' @param na.rm drop any rows with non-finite coordinates
#' @param ... passed to `sp::SpatialPointsDataFrame()`
#' @param verbose display messages
#'
#' @importFrom dplyr select select_vars filter_at
#' @importFrom sp SpatialPointsDataFrame
#'
#' @export
as_spatial <- function (
  input_data,
  coord_vars,
  crs = CRS(NA_character_),
  na.rm = TRUE,
  ...,
  verbose = getOption("verbose")
) {

  msg <- function (...) if (isTRUE(verbose)) message("[as_spatial] ", ...)

  if (isTRUE(na.rm)) {
    input_data <- filter(input_data, if_all(coord_vars, is.finite))
  }

  geodata <-
    sf::st_as_sf(
      input_data,
      coords = coord_vars,
      crs = crs,
      ...)

  return(as(geodata, "Spatial"))

}
