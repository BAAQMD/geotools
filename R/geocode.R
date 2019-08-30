#' Geocode street address(es)
#'
#' @param address character
#' @param key character (your valid API key)
#' @param verbose TRUE or FALSE
#' @param \dots further arguments to be passed on to the Google Maps API
#'
#' @description Geocode street address(es) using the Google Maps API.
#'
#' @references https://developers.google.com/maps/documentation/geocoding/
#'
#' @note \code{GOOGLE_API_KEY} should be set in \code{~/.Renviron} or \code{/etc/R/Renviron.site}
#'
#' @importFrom httr GET stop_for_status content accept_json
#' @export
geocode <- function (address, ..., key, verbose = FALSE) {
  if (is.null(address) || is.na(address)) {
    result <- list(NULL)
  } else {
    google_api_url <- "https://maps.google.com/maps/api/geocode/json"
    if (missing(key)) key <- Sys.getenv("GOOGLE_API_KEY")
    query <- list(sensor = "false", key = key, address = address, ...)
    response <- httr::GET(google_api_url, httr::accept_json(), query = query)
    httr::stop_for_status(response) # if OK, nothing will happen
    result <- httr::content(response, "parsed")
  }
  class(result) <- "GoogleGeocoderResult"
  return(result)
}

#' @export
as.data.frame.GoogleGeocoderResult <- function (x, row.names = NULL, optional = FALSE, ...) {
  f <- function (json_list) {
    with(json_list, data_frame(
      geo_addr = as.character(formatted_address),
      geo_lng = geometry$location$lng,
      geo_lat = geometry$location$lat))
  }
  if (!is.null(x$status) && x$status == "OK") {
    x$results %>% lapply(f) %>% bind_rows()
  } else {
    data_frame(geo_addr = NA_character_, geo_lng = NA_real_, geo_lat = NA_real_)
  }
}
