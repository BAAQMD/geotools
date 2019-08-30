clip_and_trim <- function (rst, lower = .Machine$double.eps, upper = Inf) {
  clamped <- raster::clamp(rst, -Inf, upper, useValues = TRUE)
  clipped <- raster::clamp(clamped, lower, Inf, useValues = FALSE)
  raster::trim(clipped)
}
