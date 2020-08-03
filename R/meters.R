#' meters
#'
#' Create a numeric vector in units of "meters" (distance)
#'
#' @param x numeric vector
#'
#' @examples
#' meters(5)
#' meters(100) * meters(100)
#'
#' @export
meters <- function (x) {
  m_unit <- units::make_unit("m")
  units::as_units(x, m_unit)
}

#' as_square_meters
#'
#' Convert a plain numeric vector to a `units` object with units of `m^2`
#'
#' @param x numeric vector
#'
#' @export
as_square_meters <- function (x) {
  readr::parse_double(x) * meters(1.0) * meters(1.0)
}
