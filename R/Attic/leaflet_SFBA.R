#' Leaflet map of the SF Bay Area
#'
#' @param ... arguments to [leaflet::leaflet()]
#' @param lng (numeric) longitude, in decimal degrees, center of map
#' @param lat (numeric) latitude, in decimal degrees, for center of map
#'
#'
#' @examples
#' leaflet_map_SFBA(zoom = 9)
#' leaflet_map_SFBA(zoom = 9, layer_controls = FALSE)
#'
#' @seealso [leaflet_map_facilities()]
#'
#' @export
leaflet_map_SFBA <- function (
  data = NULL,
  ...,
  lng = NULL,
  lat = NULL,
  zoom = NULL,
  max_zoom = NULL,
  options = NULL,
  layer_controls = TRUE,
  tileset_providers = NULL,
  tile_options = tileOptions(),
  verbose = getOption("verbose")
) {

  LIGHT_PURPLE <- "#7f7fff"

  if (is.null(options)) {
    options <- leafletOptions(minZoom = 8, maxZoom = max_zoom)
  }

  if (is.null(tileset_providers)) {
    tileset_providers <- list(
      "Default" = "CartoDB.Positron",
      "Unlabeled" = "CartoDB.PositronNoLabels",
      "Satellite" = "Esri.WorldImagery",
      #"GrayCanvas" = "Esri.WorldGrayCanvas",
      "Toner" = "Stamen.TonerLite",
      "Terrain" = "Stamen.TerrainBackground")
  }

  # HACK to make sure the first layer named in `tileset_providers` is the one shown by default
  if (!isTRUE(layer_controls)) {
    tileset_providers <- rev(tileset_providers)
  }

  CARE_color <- LIGHT_PURPLE
  CARE_plus_boundaries <- st_union(CARE::CARE_region_geodata) # FIXME: cache this

  leaflet_map <-
    leaflet(data = data, ..., options = options) %>%
    addProviderLayers(tileset_providers, tile_options = tile_options, verbose = verbose) %>%
    addPolygons(data = SFAB_WGS84_boundary, group = "AirBasin", fill = FALSE, weight = 1.5, color = gray(0.7)) %>%
    addPolygons(data = CARE_plus_boundaries, group = "CARE+", fillColor = CARE_color, color = scales::muted(CARE_color), opacity = 0.25, weight = 2)

  if (is.null(data)) {
    # If no data, use this default view. (Otherwise, it's assumed that the data
    # will determine the zoom and the bounds.)
    msg("setting view to SF Bay Area")
    leaflet_map <- setViewBayArea(leaflet_map, lng = lng, lat = lat, zoom = zoom, verbose = verbose)
  } else if (is.null(lng) && is.null(lat)) {
    bb <- unname(st_bbox(data))
    leaflet_map <- fitBounds(leaflet_map, lng1 = bb[1], lat1 = bb[2], lng2 = bb[3], lat2 = bb[4])
  }

  if (isTRUE(layer_controls)) {
    leaflet_map <-
      leaflet_map %>%
      addLayersControl(baseGroups = names(tileset_providers),
                       overlayGroups = c("AirBasin", "CARE+"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroups("CARE+")
  }

  return(leaflet_map)

}
