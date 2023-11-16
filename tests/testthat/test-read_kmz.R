test_that("read_kmz", {

  kmz_path <- "RSP-CERP-geo-boundary.kmz"
  expect_true(file.exists(kmz_path))

  result <- read_kmz(kmz_path)
  expect_is(result, "sf")
  expect_equal(nrow(result), 1)
  expect_true(all(c("Name", "Description") %in% names(result)))

})
