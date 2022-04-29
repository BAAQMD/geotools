test_that("write_geojson() works", {

  data(meuse)
  meuse_sf <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, agr = "constant")

  tmp_path <- tempfile(fileext = ".geojson")

  write_geojson(
    meuse_sf,
    tmp_path)

})
