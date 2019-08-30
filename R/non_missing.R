#' Return all rows of a Spatial*DataFrame object except those with NA in the given column
#'
#' @export
non_missing <- function (spdf, variable) {
  stopifnot(inherits(spdf, "Spatial"))
  stopifnot("data" %in% slotNames(spdf))
  spdf[!is.na(spdf@data[,variable]),]
}
