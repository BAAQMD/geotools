#' st_stencil
#'
#' @param cutout
#' @param dist
#' @param projected_crs
#'
#' @return
#' @export
#'
st_stencil <- function (cutout, dist = 100e3, projected_crs = NAD83_UTM10) {

  # Do the operations in a projected CRS
  projected_cutout <-
    st_cast(
      st_transform(cutout, projected_crs),
      "MULTIPOLYGON")

  buffered_envelope <-
    st_buffer(
      st_envelope(
        projected_cutout),
      dist = dist)

  projected_stencil <-
    st_difference(
      buffered_envelope,
      projected_cutout)

  # Restore the original CRS
  stencil <-
    st_transform(
      projected_stencil,
      st_crs(cutout))

  return(stencil)

}
