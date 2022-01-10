#' raster_overlay
#'
#' @param input_geodata `sf` or `sfc` object
#' @param raster_brick [raster::Raster-class]
#' @param fun name of summarizing function, like "mean"
#' @param progress logical
#' @param unit character (optional), like "ug/m^3"
#' @param ... passed to [exactextractr::exact_extract()]
#' @param verbose logical
#'
#' @importFrom exactextractr exact_extract
#'
#' @return
#' @export
raster_overlay <- function(
  input_geodata,
  raster_brick,
  fun = "mean",
  progress = TRUE,
  unit = NULL,
  ...,
  verbose = getOption("verbose", default = TRUE)
) {

  msg <- function (...) if(isTRUE(verbose)) {
    message("[raster_overlay] ", ...)
  }

  extracted_data <-
    as_tibble(
      exactextractr::exact_extract(
        raster_brick,
        input_geodata,
        fun = fun,
        progress = progress,
        ...)) %>%
    set_names(
      names(raster_brick))

  try(detach("package:raster"), silent = TRUE)

  overlaid_geodata <-
    input_geodata %>%
    bind_cols(extracted_data) %>%
    as_tibble() %>%
    st_as_sf()

  if (isFALSE(is.null(unit))) {
    overlaid_geodata <-
      mutate_at(
        overlaid_geodata,
        vars(one_of(names(extracted_data))),
        ~ set_units(., unit, mode = "character"))
  }

  return(overlaid_geodata)

}
