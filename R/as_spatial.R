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

  coord_vars <-
    dplyr::select_vars(
      names(input_data),
      coord_vars)

  coord_data <-
    dplyr::select(
      input_data,
      coord_vars)

  if (isTRUE(na.rm)) {

    coord_data <-
      dplyr::filter_at(
        coord_data,
        vars(coord_vars),
        all_vars(is.finite(.)))

    attr_data <-
      dplyr::filter_at(
        input_data,
        vars(coord_vars),
        all_vars(is.finite(.)))

  } else {

    coord_data <- coord_data # no change
    attr_data <- input_data # no change, just rename

  }

  geodata <-
    sp::SpatialPointsDataFrame(
      coord_data,
      attr_data,
      ...) %>%
    set_CRS(crs)

  coordnames(geodata) <-
    coord_vars

  return(geodata)

}
