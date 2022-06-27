#' ggplot_tileset
#'
#' @importFrom stringr str_detect str_c
#' @importFrom digest digest
#' @importFrom httr parse_url GET status_code
#' @importFrom png writePNG
#' @importFrom glue glue
#' @importFrom here here
#' @importFrom cacher cached `%or%`
#' @importFrom sf st_bbox st_transform
#' @importFrom slippymath bbox_to_tile_grid compose_tile_grid
#' @importFrom raster as.array
#' @importFrom ggplot2 ggplot annotation_raster
#'
#' @export
#'
ggplot_tileset <- function (
  url,
  envelope,
  zoom = NULL,
  expand = c(0, 0),
  max_tiles = NULL,
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) message("[ggplot_tileset] ", ...)
  msg("hello")

  envelope <-
    st_envelope(envelope)

  msg("buffering envelope")
  buffered_envelope <-
    st_buffer(st_transform(envelope, NAD83_UTM10), dist = 10e3)

  tile_grid <-
    slippymath::bbox_to_tile_grid(
      sf::st_bbox(sf::st_transform(buffered_envelope, WGS84_GPS)),
      zoom = zoom,
      max_tiles = max_tiles)

  if (stringr::str_detect(url, "mapbox")) {
    url <- stringr::str_c(
      url,
      "?access_token=",
      Sys.getenv("MAPBOX_TOKEN"))
  }

  download_tile <- function (url, x, y, z, default_size = c(512, 512), default_rgb = c(1, 1, 1, 1)) {

    tile_url <- glue::glue(url)

    outfile <- glue::glue(
      here::here(
        "Cache",
        "download_tile",
        httr::parse_url(tile_url)[["hostname"]],
        digest::digest(tile_url, algo = "md5"),
        z,
        "{x}_{y}.jpg"))

    if (!file.exists(outfile)) {
      dn <- dirname(outfile)
      if (!dir.exists(dn)) dir.create(dn, recursive = TRUE)
      msg("fetching: ", tile_url)
      response <- httr::GET(tile_url, httr::write_disk(outfile, overwrite = TRUE))
      if (httr::status_code(response) == 404) {
        warning("HTTP 404 Not Found. Creating default tile for ", tile_url)
        # FIXME: rgb values not working correctly, but we only want white, so it's OK for now
        pix <- array(rep(default_rgb, prod(default_size)), dim = c(default_size, 4))
        png::writePNG(pix, outfile)
      }
    }

    return(outfile)

  }

  tile_images <-
    pmap(
      tile_grid$tiles,
      download_tile,
      url = url,
      z = tile_grid$zoom)

  tile_raster <-
    cacher::cached(
      "ggplot_tileset",
      "tile_raster",
      httr::parse_url(url)[["hostname"]],
      digest::digest(url, algo = "md5"),
      digest::digest(tile_images, algo = "md5"),
      verbose = FALSE) %or%
    {
      msg("composing tile_raster")
      composed <- slippymath::compose_tile_grid(tile_grid, tile_images)
      raster::readAll(composed)
    }

  #png_path <- "tile_raster.png"
  #msg("writing raster to ", png_path)
  #slippymath::raster_to_png(tile_raster, png_path)

  names(tile_raster) <-
    c("r", "g", "b", "a")[seq_along(names(tile_raster))]

  raster_envelope <- sf::st_transform(st_envelope(tile_raster), WEB_MERCATOR)
  raster_bbox <- sf::st_bbox(raster_envelope)

  map_object <-
    ggplot2::ggplot() +
    ggplot2::annotation_raster(
      raster::as.array(tile_raster) / 255,
      xmin = raster_bbox["xmin"],
      xmax = raster_bbox["xmax"],
      ymin = raster_bbox["ymin"],
      ymax = raster_bbox["ymax"]) +
    scale_easting(
      unit = "km",
      limits = sf::st_bbox(sf::st_transform(envelope, WEB_MERCATOR))[c("xmin", "xmax")],
      expand = expand) +
    scale_northing(
      unit = "km",
      limits = sf::st_bbox(sf::st_transform(envelope, WEB_MERCATOR))[c("ymin", "ymax")],
      expand = expand) +
    coord_web_mercator(
      envelope = sf::st_transform(envelope, WEB_MERCATOR))

  return(map_object)

}
