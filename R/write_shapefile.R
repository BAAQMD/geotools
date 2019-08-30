#' @export
write_shapefile <- function (
  spobj,
  dsn = getwd(),
  layer = deparse(substitute(spobj)),
  ...,
  overwrite_layer = TRUE,
  verbose = TRUE
) {

  .Defunct("shptools::write_shp()")

  if (!dir.exists(dsn)) {
    if (verbose) warning("Creating directory ", dsn)
    dir.create(dsn, recursive = TRUE)
  }

  outfile <- file.path(dsn, str_c(layer, ".shp"))
  if (verbose) message("Writing to ", outfile)

  success <- writeOGR(
    spobj, dsn, layer, ...,
    driver = "ESRI Shapefile",
    overwrite_layer = overwrite_layer)

  return(invisible(spobj)) # for chaining

}
