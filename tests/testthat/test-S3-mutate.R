context("S3 mutate")

data(meuse, package = "sp")
coordinates(meuse) <- ~ x + y

mutated <-
  mutate(meuse, mdn_Cu = median(copper), max_Cu = max(copper))

test_that("mutate worked", {
  expect_true(all(mutated[["mdn_Cu"]] == median(meuse$copper)))
  expect_true(all(mutated[["max_Cu"]] == max(meuse$copper)))
})

test_that("class unchanged", {
  expect_is(meuse, "SpatialPointsDataFrame")
  expect_is(mutated, "SpatialPointsDataFrame")
})

