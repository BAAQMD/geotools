#' scale_northing
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
scale_northing <- function (
  name = "Northing",
  ...,
  unit = "km",
  expand = ggplot2::expansion(mult = 0, add = 0),
  labels = purrr::partial(qtytools::convert_qty, from = "m", to = unit)
) {
  scale_y_continuous(name = glue::glue("{name} ({unit})"), ..., labels = labels, expand = expand)
}
