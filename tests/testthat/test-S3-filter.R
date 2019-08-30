context("S3 filter")

data(meuse, package = "sp")
coordinates(meuse) <- ~ x + y

test_cutoff <-
  median(meuse$copper)

filtered <-
  filter(meuse, copper > median(copper))

test_that("filter worked", {
  expect_true(all(filtered[["copper"]] > test_cutoff))
})

test_that("class unchanged", {
  expect_is(meuse, "SpatialPointsDataFrame")
  expect_is(filtered, "SpatialPointsDataFrame")
})

