#' Return the coordinates of a Spatial* object as a data.frame
#'
#' @export
coord_data <- function (spobj) {
  data.frame(coordinates(spobj), row.names=row.names(spobj))
}
