#' st_sample_points
#'
#' Sample and overlay a set of points in polygons (or along polylines).
#'
#' @param geodata (sf) most likely polygons or polylines (inside or along which to sample)
#' @param size (int) number of points to sample (in or along each feature)
#' @param verbose display messages
#'
#' @return `sfc` of POINTS, joined back to the original `geodata` (i.e., an overlay)
#'
#' @export
st_sample_points <- function (
  geodata,
  size,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[st_sample_points] ", ...)

  size <-
    rlang::enquo(size)

  sample_sizes <-
    round(
      rlang::eval_tidy(size, geodata))

  stopifnot(
    funtools::all_true(
      is.finite(sample_sizes)),
    funtools::all_true(
      sample_sizes >= 0))

  msg("projecting to planar CRS (NAD83_UTM10)")
  planar_geodata <-
    sf::st_transform(
      geodata,
      crs = NAD83_UTM10)

  msg("sampling n = ", sum(sample_sizes), " points over ", nrow(geodata), " features")
  planar_points <-
    planar_geodata %>%
    sf::st_sample(size = sample_sizes) %>%
    st_cast("POINT") %>%
    st_sf()

  msg("joining points back to features")
  joined_geodata <-
    sf::st_join(
      planar_points,
      planar_geodata,
      join = st_intersects)

  msg("reprojecting back to original CRS (", st_crs(geodata), ")")
  reprojected_geodata <-
    sf::st_transform(
      joined_geodata,
      sf::st_crs(geodata))

  return(reprojected_geodata)

}
