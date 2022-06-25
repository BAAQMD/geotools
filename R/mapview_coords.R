mapview_coords <- function (
  x,
  crs = "epsg:4326",
  color = "red",
  ...
) {
  
  coords <- matrix(as.numeric(x), ncol = 2)
  
  if (is.null(names(coords))) {
    names(coords) <- c("X", "Y") 
  }
  
  mpt_obj <- sf::st_multipoint(coords)
  sfc_obj <- sf::st_sfc(mpt_obj, crs = crs)
  
  mapview::mapview(sfc_obj, color = color, ...)
  
}
