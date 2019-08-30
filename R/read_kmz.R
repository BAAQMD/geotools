#' @export
read_kmz <- function (kmz_file, kml_file = NULL, crs = EPSG_4326, ..., verbose = FALSE) {

  tmp_dir <- tempdir()
  unzipped_files <- unzip(kmz_file, exdir = tmp_dir)

  if (is.null(kml_file)) {
    unzipped_kml_files <- tools::list_files_with_exts(tmp_dir, "kml")
    stopifnot(length(unzipped_kml_files) == 1)
    kml_file <- first(unzipped_kml_files)
  }

  stopifnot(file.exists(kml_file))

  dsn <- dirname(kml_file)
  stopifnot(dir.exists(dsn))

  layer_names <- ogrListLayers(kml_file)
  stopifnot(length(layer_names) > 0)

  import_layer <- function (layer_name) {
    readOGR(dsn = kml_file, layer = layer_name, stringsAsFactors = FALSE, ...)
  }

  combined_layers <-
    set_names(layer_names) %>%
    map(import_layer) %>%
    reduce(rbind)

  reprojected <-
    combined_layers %>%
    reproject(crs)

  return(reprojected)

}

