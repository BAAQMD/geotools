#' @export
st_repair <- function (geodata, datum = st_crs(32610), view = FALSE, verbose = TRUE) {

  msg <- function (...) if(isTRUE(verbose)) message("[st_repair] ", ...)
  msg("repairing n = ", nrow(geodata))

  projected <- st_transform(geodata, datum)
  buffered <- st_buffer(projected, 0)
  unioned <- st_union(buffered, by_feature = TRUE)

  repaired <- st_filter_valid(unioned) %>% st_transform(st_crs(geodata))

  if (isTRUE(view)) mapview::mapview(repaired)

  return(repaired)

}
