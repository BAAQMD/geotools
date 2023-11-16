context("filter_spatial")

# See https://cran.r-project.org/web/packages/sp/vignettes/CRS_warnings.html
options("rgdal_show_exportToProj4_warnings" = "none")

expect_warning(
  SFBA_tracts_WGS84 <-
    TIGER2015::TIGER2015_SFBA_tracts %>%
    reproject(
      new_CRS = WGS84_CRS),
  "is deprecated")

#
# Tests with `ALA_county_WGS84`
#
expect_warning(
  ALA_county_WGS84 <-
    TIGER2015::TIGER2015_CA_counties %>%
    subset(
      str_detect(.$GEOID, "^06001")) %>%
    reproject(
      new_CRS = WGS84_CRS),
  "is deprecated")

test_that("Alameda County (sp)", {

  # Simple test: only containment (shouldn't be any "overlapping")

  test_tracts <-
    filter_spatial(
      SFBA_tracts_WGS84,
      ALA_county_WGS84)

  expect_equal(
    nrow(test_tracts), 361)

})

test_that("Alameda County (sf)", {

  # Simple test: only containment (shouldn't be any "overlapping")

  sf1 <- st_as_sf(SFBA_tracts_WGS84)
  sf2 <- st_as_sf(ALA_county_WGS84)
  test_tracts <- filter_spatial(sf1, sf2)
  expect_equal(nrow(test_tracts), 361)

})
