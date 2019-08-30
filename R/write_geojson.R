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

  require(rgdal)
  require(jsonlite)

  # Use rgdal's GeoJSON driver to write to a tempfile
  tmp_file <- tempfile()
  success <- writeOGR(
    spobj, dsn = tmp_file, layer = "", ...,
    driver = "GeoJSON",
    overwrite_layer = overwrite_layer)

  # Read the JSON back in; tidy it; and write it to a final location
  reread <- jsonlite::fromJSON(tmp_file)
  tidied <- jsonlite::toJSON(reread, pretty = pretty, digits = digits)
  cat(tidied, file = outfile)

  return(invisible(spobj)) # for chaining

}
