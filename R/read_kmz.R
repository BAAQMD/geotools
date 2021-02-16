#' read_kmz
#'
#' Read the first KML file contained in a KMZ file.
#'
#' @param kmz_file path
#' @param kml_file for backwards compatibility
#' @param crs passed to `reproject()`
#' @param ... passed to `reproject()`; may include `coordnames`
#' @param verbose display messages
#'
#' @export
read_kmz <- function (
  kmz_file,
  kml_file = NULL,
  crs = EPSG_4326,
  ...,
  verbose = FALSE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[read_kmz] ", ...)

  if (isFALSE(file.exists(kmz_file))) {
    if (isTRUE(RCurl::url.exists(kmz_file))) {
      msg("url exists; downloading")
      url <- kmz_file
      url_md5 <- digest::digest(url, "md5")
      destfile <- tempfile(pattern = url_md5, fileext = ".kmz")
      downloader::download(url, destfile)
      kmz_file <- destfile
    }
  }

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

  layer_names <- rgdal::ogrListLayers(kml_file)
  stopifnot(length(layer_names) > 0)

  import_layer <- function (layer_name) {
    msg("importing: ", layer_name)

    #
    # FIXME: add multi-layer logic using `st_layers(kml_file)`
    # (See https://mitchellgritts.com/posts/load-kml-and-kmz-files-into-r/)
    #

    sf::read_sf(kml_file)
  }

  combined_layers <-
    set_names(layer_names) %>%
    map(import_layer) %>%
    reduce(rbind)

  reprojected <-
    combined_layers %>%
    reproject(
      new_CRS = crs,
      ...)

  return(reprojected)

}

