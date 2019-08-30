#' Discard invalid geometries
#'
#' @param geodata object of type `sf`
#' @param min_dimension drop geometries with dimension lower than this
#' @param view logical; show results in Rstudio viewer?
#' @param verbose logical
#'
#' @note for 2D polygons, set `min_dimension` equal to 1
#'
#' @export
st_filter_valid <- function (geodata, min_dimension = 1, view = FALSE, verbose = TRUE) {

  msg <- function (...) if (isTRUE(verbose)) message("[st_filter_valid] ", ...)
  msg("dropping empty and invalid geometries")

  # Helper function
  is_true <- function (x) (!is.na(x) & as.logical(x))

  nonempty_geodata <-
    geodata %>%
    filter(st_dimension(geodata) >= min_dimension)

  valid_geodata <-
    nonempty_geodata %>%
    filter(is_true(st_is_valid(.)))

  if (isTRUE(view)) mapview::mapview(valid_geodata)

  return(valid_geodata)

}
