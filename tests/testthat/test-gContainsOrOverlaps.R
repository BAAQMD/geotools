context("gContainsOrOverlaps")

test_that("land area preserved", {

  spobj1 <- filter(CARE::CARE_region_geodata, CARE_name == "Richmond")
  spobj2 <- filter(TIGER2015::TIGER2015_SFBA_tracts, str_detect(GEOID, "^06013"))

})
