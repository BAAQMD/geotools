#' Create a numeric vector in units of "meters" (distance)
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

#' @export
as_square_meters <- function (x) {
  parse_double(x) * meters(1.0) * meters(1.0)
}
