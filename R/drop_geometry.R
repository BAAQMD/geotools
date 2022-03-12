#' Coerce an `sf` object to a `tibble`
#'
#' @param geodata an `sf` object
#' @return a `tibble`
#' @export
drop_geometry <- function (geodata) {
  .Deprecated("st_drop_geometry")
  return(sf::st_drop_geometry(geodata))
}
