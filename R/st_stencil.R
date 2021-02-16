#' st_stencil
#'
#' Returns a "stencil" (a polygon larger than `cutout`) with `cutout` subtracted from it.
#'
#' @param cutout polygonal `sf` object
#' @param dist buffer size for stencil
#' @param projected_crs something to work in, like `NAD83_UTM10`, that makes sense for your particular use case
#'
#' @return polygon with cutout cut out
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
