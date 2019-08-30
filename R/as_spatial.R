#' Create a SpatialPointsDataFrame from tabular data
#'
#' @importFrom dplyr select select_vars
#' @importFrom sp SpatialPointsDataFrame
#' @export
as_spatial <- function (input_data, coord_vars, crs = CRS(NA_character_), na.rm = TRUE, ...) {

  msg <- function (...) if (isTRUE(verbose)) message("[as_spatial] ", ...)

  coord_vars <- dplyr::select_vars(names(input_data), coord_vars)
  coord_data <- dplyr::select(input_data, coord_vars)

  if (isTRUE(na.rm)) {

    coord_data <- filter_at(coord_data, vars(coord_vars), all_vars(is.finite(.)))
    attr_data <- filter_at(input_data, vars(coord_vars), all_vars(is.finite(.)))

  } else {

    coord_data <- coord_data # no change
    attr_data <- input_data # no change, just rename

  }

  geodata <-
    sp::SpatialPointsDataFrame(coord_data, attr_data, ...) %>%
    set_CRS(crs)

  coordnames(geodata) <-
    coord_vars

  return(geodata)

}
