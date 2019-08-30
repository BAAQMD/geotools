#' Coerce an `sf` object to a `tibble`
#'
#' @param geodata an `sf` object
#' @return a `tibble`
#' @export
drop_geometry <- function (geodata) {

  st_geometry(geodata) <- NULL

  return(as_tibble(geodata))

}
