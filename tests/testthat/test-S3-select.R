context("S3 select")

data(meuse, package = "sp")
coordinates(meuse) <- ~ x + y

selected <-
  select(meuse, Pb = lead, Cu = copper)

test_that("select worked", {
  expect_equal(names(selected), c("Pb", "Cu"))
})

test_that("values unchanged", {
  expect_identical(selected[["Pb"]], meuse$lead)
  expect_identical(selected[["Cu"]], meuse$copper)
})

test_that("class unchanged", {
  expect_is(meuse, "SpatialPointsDataFrame")
  expect_is(selected, "SpatialPointsDataFrame")
})

