#' Return the coordinates of a Spatial* object as a tibble
#'
#' @param spobj anything for which `coordinates(.)` works
#'
#' @export
coord_data <- function (spobj) {

  coord_df <-
    data.frame(
      coordinates(spobj),
      row.names = row.names(spobj))

  coord_tbl <-
    tibble::as_tibble(
      coord_df)

  return(coord_tbl)

}
