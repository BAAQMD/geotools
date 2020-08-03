#' write_geojson
#'
#' Write a layer to a GeoJSON file.
#'
#' @param spobj geodata
#' @param dsn output path; defaults to the current working directory
#' @param layer name of layer to write
#' @param pretty passed to `jsonlite::toJSON()`
#' @param digits passed to `jsonlite::toJSON()`
#' @param ... passed to `rgdal::writeOGR()`
#' @param overwrite_layer passed to `rgdal::writeOGR()`
#' @param verbose display messages
#'
#' @importFrom rgdal writeOGR
#' @importFrom jsonlite fromJSON toJSON
#'
#' @export
write_geojson <- function (
  spobj,
  dsn = getwd(),
  layer = deparse(substitute(spobj)),
  pretty = TRUE,
  digits = 6,
  ...,
  overwrite_layer = TRUE,
  verbose = TRUE
) {

  outfile <- file.path(dsn, str_c(layer, ".geojson"))
  if (verbose) message("Writing to ", outfile)

  # Use rgdal's GeoJSON driver to write to a tempfile
  tmp_file <- tempfile()
  success <- rgdal::writeOGR(
    spobj,
    dsn = tmp_file,
    layer = "",
    ...,
    driver = "GeoJSON",
    overwrite_layer = overwrite_layer)

  # Read the JSON back in; tidy it; and write it to a final location
  reread <- jsonlite::fromJSON(tmp_file)
  tidied <- jsonlite::toJSON(reread, pretty = pretty, digits = digits)
  cat(tidied, file = outfile)

  return(invisible(spobj)) # for chaining

}
