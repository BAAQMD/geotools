context("merge S4 generic (SpatialLinesDataFrame)")

rand_xy <- function (...) cbind(x = rnorm(...), y = rnorm(...))
rand_int <- function (n, upper = 1, lower = 0) round(runif(n, lower, upper))

rand_lines <- function (n) lapply(
  rand_int(n, upper = 10, lower = 3),
  function (n) Line(rand_xy(n)))

test_ll <- list(
  Lines(rand_lines(3), ID = "L0"),
  Lines(rand_lines(4), ID = "L1"),
  Lines(rand_lines(2), ID = "L2"))

ln_ids <- sapply(test_ll, slot, name = "ID")

foo_df <- data.frame(
  ln_id = ln_ids,
  foo = rnorm(length(ln_ids)),
  row.names = ln_ids)

bar_df <- data.frame(
  ln_id = ln_ids,
  bar = rnorm(length(ln_ids)),
  row.names = ln_ids)

test_sldf <- sp::SpatialLinesDataFrame(
  sl = sp::SpatialLines(test_ll),
  data = foo_df, match.ID = TRUE)

test_that("values match before merging", {
  expect_identical(test_sldf$foo, foo_df$foo)
})

test_that("merge doesn't trip over itself", {
  expect_equal(
    sp::merge(test_sldf, bar_df, by = "ln_id"),
    merge_SpatialLinesDataFrame(test_sldf, bar_df, by = "ln_id"))
})

test_that("values match after merging", {
  merged <- merge(test_sldf, bar_df, by = "ln_id")
  expect_identical(merged$foo, foo_df$foo)
  expect_identical(merged$bar, bar_df$bar)
})

test_that("class is correct", {
  merged <- merge_SpatialLinesDataFrame(test_sldf, bar_df, by = "ln_id")
  expect_is(merged, "SpatialLinesDataFrame")
})
