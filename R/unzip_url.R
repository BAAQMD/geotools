#' To download and extract a zipped file (that may not have .zip at the end of the URL)
#'
#' @param url character
#' @param destfile name for local copy of (downloaded) file
#' @param exdir directory to hold expanded contents
#' @param quiet (optional)
#'
unzip_url <- function (url, destfile, exdir, quiet = FALSE) {
  if (missing(destfile)) destfile <- basename(url)
  if (missing(exdir)) exdir <- file.path(getwd(), sub("\\.zip$", "", destfile))
  zipfile <- file.path(exdir, destfile)
  if (!file.exists(zipfile)) {
    dir.create(exdir, recursive = TRUE)
    download.file(url, zipfile)
  }
  if (!quiet) message("Unzipping into: ", exdir)
  contents <- unzip(zipfile, exdir = exdir)
  return(exdir)
}
