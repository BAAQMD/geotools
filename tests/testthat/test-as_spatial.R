context("as_spatial")

input_data <-
  tibble::tibble(
    fac_id = c(10, 11, 12),
    fac_lng = c(-122, -121, NA),
    fac_lat = c(NA, 38, 39))

test_that("na.rm = default", {

  test_geodata <-
    input_data %>%
    as_spatial(coord_vars = c("fac_lng", "fac_lat"), crs = WGS84)

  expect_equal(1, nrow(test_geodata))

})

test_that("na.rm = TRUE", {

  test_geodata <-
    input_data %>%
    as_spatial(coord_vars = c("fac_lng", "fac_lat"), crs = WGS84, na.rm = TRUE)

  expect_equal(1, nrow(test_geodata))

})

test_that("na.rm = FALSE", {

  expect_error(

    test_geodata <-
      input_data %>%
      as_spatial(coord_vars = c("fac_lng", "fac_lat"), crs = WGS84, na.rm = FALSE),

    "NA values in coordinates")

})
