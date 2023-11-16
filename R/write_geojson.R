#' write_geojson
#'
#' Write a layer to a GeoJSON file.
#'
#' @param spobj geodata
#' @param dsn output path; defaults to the current working directory
#' @param layer name of layer to write
#' @param ... passed to [sf::st_write()]
#' @param overwrite (logical)
#' @param verbose display messages
#'
#' @importFrom here here
#' @importFrom sf st_write st_transform st_as_sf
#' @importFrom fs path_ext file_exists file_delete path_rel
#' @importFrom jsonlite fromJSON toJSON
#'
#' @export
write_geojson <- function (
  object,
  dsn = NULL,
  layer = NULL,
  ...,
  crs = 4326,
  overwrite = TRUE,
  pretty = TRUE,
  digits = 6,
  append = FALSE,
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) message("[write_geojson] ", ...)

  if (is.null(dsn)) {
    dsn <- getwd()
    if (is.null(layer)) {
      layer <- deparse(substitute(object))
    }
    path <- fs::path(dsn, layer)
  } else {
    path <- dsn
  }

  fs::path_ext(path) <- ".geojson"

  if (file.exists(path)) {
    if (isTRUE(overwrite)) {
      file.remove(path) # FIXME: move out of the way, don't delete (in case of failure)
    } else {
      stop("[write_geojson] overwrite is FALSE and file exists: ", path)
    }
  }

  msg("writing ", nrow(object), " features to ",
      fs::path_rel(path, here::here()))

  sf::st_write(
    sf::st_transform(
      sf::st_as_sf(object),
      crs = crs),
    path,
    ...,
    append = append)

  if (isTRUE(pretty) || isTRUE(is.finite(digits))) {
    # Read the JSON back in; tidy it; and write it to a final location
    reread <- jsonlite::fromJSON(path)
    tidied <- jsonlite::toJSON(reread, pretty = pretty, digits = digits)
    cat(tidied, file = path)
  }

  return(path)

}
