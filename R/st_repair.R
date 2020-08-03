#' st_repair
#'
#' Attempt to repair problems in geodata.
#'
#' @param geodata `sf` object
#' @param datum passed to `sf::st_transform()`
#' @param view (optional) show result using `mapview()`
#' @param verbose display messages
#'
#' @importFrom sf st_transform st_buffer st_union
#' @importFrom mapview mapview
#'
#' @export
st_repair <- function (
  geodata,
  datum = st_crs(32610),
  view = FALSE,
  verbose = TRUE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[st_repair] ", ...)
  msg("repairing n = ", nrow(geodata))

  projected <-
    sf::st_transform(
      geodata,
      crs = datum)

  buffered <-
    sf::st_buffer(
      projected,
      dist = 0)

  unioned <-
    sf::st_union(
      buffered,
      by_feature = TRUE)

  repaired <-
    st_transform(
      st_filter_valid(unioned),
      st_crs(geodata))

  if (isTRUE(view)) mapview::mapview(repaired)

  return(repaired)

}
