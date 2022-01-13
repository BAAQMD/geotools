#' Promote a data frame to an `sf` object
#'
#' @param input_data data frame or tibble
#' @param coords names of columns; see [sf::st_as_sf()]
#' @param crs can use integer or [sf::st_crs()]
#' @param ... further arguments to [sf::st_as_sf()]
#' @param verbose logical
#'
#' @importFrom sf st_crs st_as_sf
#'
#' @export
as_geodata <- function (input_data, coords, crs, ..., verbose = TRUE) {

  msg <- function (...) if(isTRUE(verbose)) message("[as_geodata] ", ...)

  # TODO: handle missing coord vars (autodetect using `tbltools::find_vars()`)

  if (is.numeric(crs)) {
    crs <- sf::st_crs(crs)
  }

  geodata <- sf::st_as_sf(input_data, coords = coords, crs = crs, ...)

  return(geodata)

}
