#' Map facility-specific locations (as pins), or quantities (as circles), with leaflet
#'
#' @param facilities (geodata) for example: `point_source_facilities(RY(2015))`
#' @param size (expression) if supplied, controls size of circles (area, not radius, will be proportional to this)
#' @param color (expression) if supplied, controls colors of circles
#' @param max_size (numeric) helpful for tweaking the relative sizes of circles
#' @param stroke (logical) see [leaflet::addCircleMarkers()]
#' @param ... further arguments to [leaflet_SFBA()]
#' @param verbose (logical)
#'
#' @seealso [leaflet_map_SFBA()]
#'
#' @export
leaflet_map_facilities <- function (
  facilities,
  size = NULL,
  color = "orange",
  opacity = 0.333,
  popup = NULL,
  max_size = 100,
  stroke = FALSE,
  popupOptions = popupOptions(),
  ...,
  verbose = getOption("verbose")
) {

  require(lazyeval)

  msg <- function (...) if(isTRUE(verbose)) message("[leaflet_map_facilities] ", ...)

  leaflet_map <- leaflet_map_SFBA(facilities, ...)

  #
  # Try a reasonable "default" formula for popup HTML
  #
  if (is.null(popup)) {

    popup_vars <- c("fac_id", "fac_name", "fac_address")

    if (all(popup_vars %in% names(facilities))) {
      popup <- ~ str_c(
        str_c("<b>P#", fac_id, "</b>"),
        fac_name,
        fac_address,
        sep = "</br>")
    }

  }

  msg("popup formula is: ", popup)

  #
  # Use non-standard evaluation to compute `sizes`
  #
  #f_size <- f_capture(size)
  #sizes <- f_eval(f_size, data = facilities)

  if (is.null(size)) {

    #
    # If no `size` provided: just add Markers
    #
    msg("adding Markers")
    addMarkers(leaflet_map, popup = popup)

  } else {

    sizes <- pull(facilities, size)

    #
    # Add CircleMarkers, with *area* (not radius!) proportional to `size`
    #
    msg("top sizes are: ", str_csv(head(rev(sort(round(sizes))))))

    radii <- scales::rescale_max(sqrt(sizes)) * max_size
    msg("top radii are: ", str_csv(head(rev(sort(round(radii))))))

    msg("adding CircleMarkers")
    addCircleMarkers(
      leaflet_map,
      radius = radii,
      fillColor = color,
      fillOpacity = opacity,
      stroke = stroke,
      color = color,
      opacity = opacity,
      popup = popup,
      popupOptions = popupOptions)

  }

}
