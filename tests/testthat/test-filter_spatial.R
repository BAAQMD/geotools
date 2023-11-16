test_that("filter_spatial", {

  # Simple test: only containment (shouldn't be any "overlapping")

  filter_region <-
    SFBA::SFBA_county_TIGER_2010_geometry |>
    filter(county_id == "06075") |> # San Francisco
    st_transform(UTM10_CRS) |>
    st_buffer(-100)

  test_tracts <-
    filter_spatial(
      SFBA::SFBA_tract_TIGER_2010_geometry,
      filter_region)

  expect_equal(
    nrow(test_tracts), 197)

})
