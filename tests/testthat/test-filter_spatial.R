context("filter_spatial")

SFBA_tracts_WGS84 <-
  TIGER2015::TIGER2015_SFBA_tracts %>%
  reproject(
    new_CRS = WGS84)

#
# Tests with `ALA_county_WGS84`
#

ALA_county_WGS84 <-
  TIGER2015::TIGER2015_CA_counties %>%
  subset(
    str_detect(.$GEOID, "^06001")) %>%
  reproject(
    new_CRS = WGS84)

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

#
# Tests with `CARE_Richmond_WGS84`
#

test_that("Richmond CARE Impact Region", {

  # More complicated: some tracts are "contained"; some tracts "overlap"

  CARE_Richmond_WGS84 <-
    CARE::CARE_region_geodata %>%
    filter(
      CARE_name == "Richmond") %>%
    filter(
      CARE_designation == "impact") %>%
    reproject(
      new_CRS = WGS84)

  spobj1 <- SFBA_tracts_WGS84
  spobj2 <- CARE_Richmond_WGS84

  expected_nrow <- 36

  expect_message(
    test_spobj <- filter_spatial(spobj1, spobj2),
    "st_as_sf")

  expect_equal(nrow(test_spobj), expected_nrow)

  sf1 <- st_as_sf(spobj1)
  sf2 <- st_as_sf(spobj2)

  test_sf <- filter_spatial(sf1, sf2)
  expect_equal(nrow(test_sf), expected_nrow)

})

#
# Tests with `CARE_Vallejo_WGS84`
#

test_that("Vallejo CARE Impact Region", {

  CARE_Vallejo_WGS84 <-
    CARE::CARE_region_geodata %>%
    filter(
      CARE_name == "Vallejo") %>%
    filter(
      CARE_designation == "impact") %>%
    reproject(
      new_CRS = WGS84)

  spobj1 <- SFBA_tracts_WGS84
  spobj2 <- CARE_Vallejo_WGS84

  expected_nrow <- 22

  expect_message(
    test_spobj <- filter_spatial(spobj1, spobj2),
    "st_as_sf")

  expect_equal(nrow(test_spobj), expected_nrow)

  sf1 <- st_as_sf(spobj1)
  sf2 <- st_as_sf(spobj2)

  test_sf <- filter_spatial(sf1, sf2)
  expect_equal(nrow(test_sf), expected_nrow)

})
