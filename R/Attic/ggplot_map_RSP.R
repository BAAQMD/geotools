#' ggplot_map_RSP
#'
#' @param data
#' @param tileset
#' @param tile_opacity
#' @param coastline
#' @param zoom
#' @param ...
#' @param envelope
#' @param crs
#' @param expand
#' @param verbose
#'
#' @note Cloned from PM25-major-facility-modeling on 2020-09-11
#'
#' @importFrom here here
#' @importFrom ggmap ggmap get_googlemap get_stamenmap
#' @importFrom ggspatial annotation_scale
#' @importFrom ggplot2 expansion coord_sf ggplot geom_sf
#' @importFrom sf st_buffer st_transform st_as_sfc st_set_crs st_crs st_coordinates st_centroid st_bbox st_intersection
#' @importFrom strtools str_csv
#' @importFrom librarian is_valid_url
#'
#' @export
#'
ggplot_map_RSP <- function (
  data = NULL,
  tileset = NULL,
  tile_opacity = NULL,
  coastline = NULL,
  zoom = NULL,
  ...,
  envelope = NULL,
  crs = WEB_MERCATOR,
  expand = ggplot2::expansion(mult = 0.025),
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[ggplot_map_RSP] ", ...)

  msg("buffering envelope")
  buffered_envelope <-
    st_buffer(st_transform(envelope, NAD83_UTM10), dist = 10e3)

  if (isTRUE(is.null(envelope))) {
    xmin <- -180550; xmax <- -131950; ymin <- 83562; ymax <- 140950
    lcc_proj4string <- "+proj=lcc +lat_1=30 +lat_2=60 +lat_0=37 +lon_0=-120.5 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs "
    envelope <-
      glue::glue("POLYGON (({xmin} {ymin}, {xmax} {ymin}, {xmax} {ymax}, {xmin} {ymax}, {xmin} {ymin}))") %>%
      st_as_sfc() %>%
      st_set_crs(lcc_proj4string) %>%
      st_transform(crs)
  }

  if (isTRUE(crs == WEB_MERCATOR)) {
    map_coord <-
      coord_web_mercator(
        envelope = st_transform(envelope, WEB_MERCATOR))
  } else {
    #stop("custom crs not yet supported")
    map_coord <- coord_sf(crs = crs, datum = crs)
  }

  if (isFALSE(is.null(tileset))) {

    if (tileset == "raceeth") {
      tileset <- "http://demographics.virginia.edu/DotMap/tiles4/{z}/{x}/{y}.png"
      tile_opacity <- (tile_opacity %||% 1.0) # default to  1.0
      coastline <- (coastline %||% TRUE)
      zoom <- (zoom %||% 10)
    }

    if (tileset == "satellite") {
      tileset <- "https://api.mapbox.com/v4/mapbox.satellite/{z}/{x}/{y}@2x.png"
      tile_opacity <- (tile_opacity %||% 0.5) # default to 0.5
      coastline <- (coastline %||% FALSE)
      zoom <- (zoom %||% 10)
    }

    if (librarian:::is_valid_url(tileset)) {

      msg("tileset is valid url; invoking ggplot_tileset()")
      map_object <-
        ggplot_tileset(
          url = tileset,
          zoom = zoom,
          expand = expand,
          envelope = envelope,
          verbose = verbose)

    } else if (isFALSE(is.null(tileset))) {

      msg("tileset is: ", tileset)

      #crs <- WEB_MERCATOR
      crs <- st_crs(WGS84_GPS)
      datum <- crs
      map_coord <- coord_sf(crs = crs, datum = datum)

      basemap_lonlat_bounds <-
        set_names(
          st_bbox(st_transform(buffered_envelope, WGS84_GPS)),
          c("left", "bottom", "right", "top"))

      basemap_lonlat_center <-
        set_names(as.numeric(
          st_coordinates(st_transform(
            st_centroid(buffered_envelope), WGS84_GPS))), c("lon", "lat"))

      msg("basemap_lonlat_bounds is: ", str_csv(basemap_lonlat_bounds))
      msg("basemap_lonlat_center is: ", str_csv(basemap_lonlat_center))

      require(ggmap)

      if (str_detect(tileset, "toner")) {

        basemap_tiles <-
          ggmap::get_stamenmap(
            bbox = basemap_lonlat_bounds,
            zoom = zoom,
            scale = 2,
            maptype = tileset,
            where = here::here("Cache", "get_map", tileset))

      } else if (str_detect(tileset, "terrain|satellite|roadmap|hybrid")) {

        basemap_tiles <-
          ggmap::get_googlemap(
            center = basemap_lonlat_center,
            zoom = zoom,
            scale = 2,
            maptype = tileset,
            where = here::here("Cache", "get_map", tileset))

      }

      map_object <-
        ggmap::ggmap(basemap_tiles)

    }

  } else {

    map_object <-
      ggplot()

  }

  if (isFALSE(is.null(tileset))) {

    msg("tile_opacity is: ", tile_opacity)
    tile_opacity_layer <-
      geom_sf(
        fill = "white", alpha = I(1 - tile_opacity),
        inherit.aes = FALSE,
        data = buffered_envelope) #st_buffer(st_transform(envelope, WGS84_GPS), 0.1))

    map_object <-
      map_object +
      tile_opacity_layer

  }

  msg("st_bbox(envelope)['ymin'] is: ", st_bbox(envelope)['ymin'])
  msg("crs is: ", crs)

  map_annotations <-
    list(
      # ggspatial::annotation_north_arrow(
      #   location = "br",
      #   which_north = "true",
      #   height = unit(1.25, "cm"),
      #   pad_x = unit(0.25, "in"), pad_y = unit(0.4, "in"),
      #   style = ggspatial::north_arrow_fancy_orienteering()),
      ggspatial::annotation_scale(
        text_cex = 0.9,
        location = "br"))

  map_theme <-
    theme_void() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_line(color = gray(0.8), size = 0.1),
      panel.background = element_blank())

  map_object <-
    map_object +
    map_coord +
    map_theme +
    map_annotations +
    scale_easting(
      limits = st_bbox(st_envelope(st_transform(envelope, crs)))[c("xmin", "xmax")],
      expand = expand) +
    scale_northing(
      limits = st_bbox(st_envelope(st_transform(envelope, crs)))[c("ymin", "ymax")],
      expand = expand) +
    labs(
      caption = glue::glue(
        "DRAFT {strtools::str_date()}"))

  coastline <- (coastline %||% TRUE)
  if (isTRUE(coastline)) {

    msg("making coastline_stencil")
    coastline_stencil <-
      SFBA::SFBA_OSM_coast %>%
      st_transform(st_crs(envelope)) %>%
      st_stencil(dist = 10e3) %>%
      st_intersection(st_buffer(envelope, dist = 10e3)) %>%
      st_transform(crs)

    map_coastline_layer <-
      geom_sf(
        color = NA, fill = gray(0.8), alpha = 0.5,
        inherit.aes = FALSE,
        data = coastline_stencil)

    map_object <-
      map_object +
      map_coastline_layer

  }

  attr(map_object, "map_coord") <-
    map_coord

  return(map_object)

}
