context("gClip")

test_that("land area preserved", {

  county <- TIGER2015::TIGER2015_CA_counties["06001", ]
  coast <- SFBA::SFBA_OSM_coast

  expect_warning(
    clipped <- gClip(county, coast, verbose = TRUE),
    "attribute variables are assumed to be spatially constant throughout all geometries")

  expect_equal(
    readr::parse_double(county[["ALAND"]]), 1.91e9, tol = 0.01)

  expect_equal(
    sf::st_area(clipped), # in square meters
    units::set_units(1.91e9, "m^2"),
    tol = 0.01)

})
