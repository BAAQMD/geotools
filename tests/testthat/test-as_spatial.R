context("as_spatial")

input_data <-
  tibble::tibble(
    fac_id = c(10, 11, 12),
    fac_lng = c(-122, -121, NA),
    fac_lat = c(NA, 38, 39))

test_that("na.rm = default", {

  test_geodata <-
    as_spatial(
      input_data,
      coord_vars = c("fac_lng", "fac_lat"),
      crs = WGS84_CRS)

  expect_equal(1, nrow(test_geodata))

})

test_that("na.rm = TRUE", {

  test_geodata <-
    as_spatial(
      input_data,
      coord_vars = c("fac_lng", "fac_lat"),
      crs = WGS84_CRS,
      na.rm = TRUE)

  expect_equal(1, nrow(test_geodata))

})

test_that("na.rm = FALSE", {

  expect_error(

    test_geodata <-
      as_spatial(
        input_data,
        coord_vars = c("fac_lng", "fac_lat"),
        crs = WGS84_CRS,
        na.rm = FALSE),

    "missing values in coordinates not allowed")

})
