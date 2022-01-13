#' proj4string_LCC
#'
#' Construct a `PROJ4` string for a Lambert Conformal Conic projection.
#'
#' @param lon_0 Longitude of projection center
#' @param lat_0 Latitude of projection center
#' @param lat_1 First standard parallel
#' @param lat_2 Second standard parallel
#' @param x_0 False easting
#' @param y_0 False northing
#' @param ... reserved for future use
#' @param verbose logical
#'
#' @references
#' - https://proj.org/operations/projections/lcc.html
#'
#' @examples
#' proj4string_LCC(lat_1=30, lat_2=60, lat_0=37, lon_0=-120.5, y_0=16000, x_0=220000, a=6.37e6, b=6.37e6, units="km", verbose=TRUE)
proj4string_LCC <- function (
  lon_0 = 0.0,
  lat_0 = 0.0,
  lat_1 = 0.0,
  lat_2 = 0.0,
  x_0 = 0.0,
  y_0 = 0.0,
  units = "m",
  datum = "NAD83",
  ellps = "GRS80",
  ...,
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) {
    message("[proj4string_LCC] ", ...)
  }

  dot_args <-
    rlang::dots_list(..., .named = TRUE)

  dot_assignments <-
    unname(imap_chr(dot_args, ~ str_c(.y, .x, sep = "=")))

  template <-
    c("proj=lcc",
      "lat_1={lat_1}",
      "lat_2={lat_2}",
      "lat_0={lat_0}",
      "lon_0={lon_0}",
      "x_0={x_0}",
      "y_0={y_0}") %>%
    c(dot_assignments) %>%
    c("units={units}",
      "datum={datum}",
      "ellps={ellps}",
      "no_defs") %>%
    str_c("+", .) %>%
    str_c(., collapse = " ")

  glued <-
    as.character(
      glue::glue(
        template))

  msg(glued)

  return(glued)

}
