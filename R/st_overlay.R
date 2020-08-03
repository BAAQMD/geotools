#' @importFrom raster extract
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @rdname st_overlay-methods
st_overlay.geodata.raster <- function (
  geodata,
  object,
  ...
) {

  col_name <- deparse(substitute(object))
  col_name <- str_c(col_name, "_data")

  weighted_values_list <-
    raster::extract(
      object,
      geodata,
      cellnumbers = TRUE,
      weights = TRUE,
      normalizeWeights = TRUE,
      na.rm = FALSE)

  data_list <-
    purrr::map(
      weighted_values_list,
      tibble::as_tibble)

  annotated_geodata <-
    dplyr::mutate(
      geodata,
      !!col_name := data_list)

  return(annotated_geodata)

}

setClass(
  "RasterStack",
  package = "raster")

#' Overlay spatial data onto some kind of (other) spatial data.
#'
#' WARNING: Experimental work in progress! Developed to overlay a RasterStack onto polygons. YMMV.
#'
#' @param geodata for now, should be polygons in the form of an `sf` object
#' @param object for now, should be a `RasterLayer` or `RasterStack`
#' @param ... further arguments (reserved for future use)
#'
#' @export
#' @docType methods
#' @rdname st_overlay-methods
setGeneric(
  "st_overlay",
  function (geodata, object, ...) {
    standardGeneric("st_overlay")
  })

#' @rdname st_overlay-methods
#' @importClassesFrom raster RasterLayer
#' @aliases st_overlay,sf,RasterLayer-method
setMethod(
  "st_overlay",
  signature(geodata = "sf", object = "RasterLayer"),
  st_overlay.geodata.raster)

#' @rdname st_overlay-methods
#' @importClassesFrom raster RasterStack
#' @aliases st_overlay,sf,RasterStack-method
setMethod(
  "st_overlay",
  signature(geodata = "sf", object = "RasterStack"),
  st_overlay.geodata.raster)
