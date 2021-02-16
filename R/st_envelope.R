#' st_envelope
#'
#' Construct a rectangular polygon enveloping `x`
#'
#' @param x either a spatial object, or a vector of four coordinates `c(xmin, xmax, ymin, ymax)`
#' @param ... passed to [sf::st_bbox] or [sf::st_as_sf()]
#'
#' @export
st_envelope <- function (x, ..., crs = NULL) {

  if (is.numeric(x)) {

    coords <- c(x, ...)
    stopifnot(length(coords) == 4)
    xmin <- coords["xmin"]; xmax <- coords["xmax"]
    ymin <- coords["ymin"]; ymax <- coords["ymax"]
    wkt_data <- tibble(
      wkt = glue::glue(
        "POLYGON (({xmin} {ymin}, {xmax} {ymin}, {xmax} {ymax}, {xmin} {ymax}, {xmin} {ymin}))"))
    envelope <- st_as_sf(wkt_data, wkt = "wkt", ...)
    if (is.null(crs)) {
      stop("must supply crs = ...")
    } else {
      st_crs(envelope) <- crs
    }

  } else {

    bb <- sf::st_bbox(x, ...)
    envelope <- st_as_sfc(bb)

    if (isFALSE(is.null(crs))) {
      envelope <- st_transform(envelope, crs)
    }

  }

  return(envelope)

}
