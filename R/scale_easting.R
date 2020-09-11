#' scale_easting
#'
#' @param name
#' @param ...
#' @param unit
#' @param expand
#' @param labels
#'
#' @return ggplot2 scale object
#' @export
#'
scale_easting <- function (
  name = "Easting",
  ...,
  unit = "km",
  expand = ggplot2::expansion(mult = 0, add = 0),
  labels = purrr::partial(qtytools::convert_qty, from = "m", to = unit)
) {
  scale_x_continuous(name = glue::glue("{name} ({unit})"), ..., labels = labels, expand = expand)
}
