#' read_kmz
#'
#' Read the first KML file contained in a KMZ file.
#'
#' @param kmz_path path
#' @param kml_file for backwards compatibility
#' @param crs passed to `reproject()`
#' @param ... passed to `reproject()`; may include `coordnames`
#' @param verbose display messages
#'
#' @export
read_kmz <- function (
    kmz_path,
    kml_file = NULL,
    crs = WGS84_CRS,
    ...,
    verbose = FALSE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[read_kmz] ", ...)

  if (isFALSE(file.exists(kmz_path))) {
    if (isTRUE(RCurl::url.exists(kmz_path))) {
      msg("url exists; downloading")
      url <- kmz_path
      url_md5 <- digest::digest(url, "md5")
      destfile <- tempfile(pattern = url_md5, fileext = ".kmz")
      downloader::download(url, destfile)
      kmz_path <- destfile
    }
  }

  tmp_dir <- file.path(tempdir(), basename(kmz_path))
  unzipped_files <- unzip(kmz_path, exdir = tmp_dir)

  if (is.null(kml_file)) {

    unzipped_kml_files <-
      dir(tmp_dir, pattern = glob2rx("*.kml"),
          recursive = TRUE)

    stopifnot(length(unzipped_kml_files) == 1)
    kml_file <- file.path(
      tmp_dir, unzipped_kml_files[[1]])

  }

  stopifnot(file.exists(kml_file))

  dsn <- dirname(kml_file)
  stopifnot(dir.exists(dsn))

  layer_names <-
    sf::st_layers(kml_file)$name

  stopifnot(length(layer_names) > 0)

  import_layer <- function (layer_name) {
    #
    # FIXME: add multi-layer logic using `st_layers(kml_file)`
    # (See https://mitchellgritts.com/posts/load-kml-and-kmz-files-into-r/)
    #
    msg("importing: ", layer_name)
    sf::read_sf(kml_file)
  }

  result <-
    set_names(layer_names) %>%
    map(import_layer) %>%
    bind_rows()

  if (!is.null(crs)) {
    result <- st_transform(result, crs)
  }

  return(result)

}

