#' st_sample_geotools
#'
#' @examples
#' county_geodata <- TIGER2015::TIGER2015_SFBA_counties %>% with_county_populations()
#' county_sample <- county_geodata %>% st_sample(n = 1000, weight_var = "county_pop_total")
#'
#' tract_geodata <- TIGER2015::TIGER2015_SFBA_tracts %>% filter(str_detect(GEOID, "^06095")) %>% with_tract_populations()
#' tract_sample <- tract_geodata %>% st_sample(n = 1000, weight_var = "tract_pop_total")
#'
#' ALA_block_geodata <- TIGER2015::TIGER2015_SFBA_blocks %>% filter(str_detect(GEOID10, "^06001")) %>% with_block_populations()
#' ALA_block_sample <- ALA_block_geodata %>% dplyr::select(block_id, block_pop_total) %>% st_sample(frac = 0.1, weight_var = "block_pop_total")
#'
#' mapview::mapview(ALA_block_sample, cex = 1, color = NULL)
#'
#' @export
st_sample_geotools <- function (
  geodata,
  n,
  fraction = NULL,
  weight_var,
  type = "random",
  iter = 9,
  ...
) {

  msg <- function (...) if(isTRUE(verbose)) message("[st_sample_geotools] ", ...)

  .Defunct(
    new = "st_sample_points",
    package = "sf")

  if (missing(n)) {
    if (is.null(fraction)) stop("must supply either `n` or `fraction`")
    n <- fraction * sum(pull(geodata, weight_var))
  }

  n <- as.integer(round(n))

  weights <- pull(geodata, weight_var)
  counts <- round(n * weights / sum(weights, na.rm = TRUE))
  i <- (counts > 0) & (is.finite(counts))

  msg("n = ", n, " (covering ", sum(i), " of ", length(i), " areal units)")

  poly_objs <- as(geodata[i, ], "Spatial")
  poly_geoms <- geometry(poly_objs)
  poly_ids <- row.names(poly_geoms)
  poly_objs@data[, "poly_id"] <- poly_ids
  poly_counts <- set_names(counts[i], poly_ids)

  sampler <- function (poly_id) {
    feature <- poly_geoms[poly_id, ]
    n_pts <- poly_counts[poly_id]
    pt_coords <- spsample(feature, n_pts, type = "random", iter = iter)
    pt_attrs <- data_frame(poly_id = rep(poly_id, n_pts))
    return(SpatialPointsDataFrame(pt_coords, pt_attrs))
  }

  rbind_list <- purrr::lift_dl(rbind)
  result_list <- pbapply::pblapply(poly_ids, sampler)

  joined <-
    compact(result_list) %>%
    rbind_list() %>%
    st_as_sf() %>%
    left_join(poly_objs@data, by = "poly_id") %>%
    dplyr::select(-poly_id)

  return(joined)

}
