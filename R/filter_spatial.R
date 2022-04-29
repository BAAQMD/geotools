# See https://cran.r-project.org/web/packages/sp/vignettes/CRS_warnings.html
options("rgdal_show_exportToProj4_warnings" = "none")

#' Apply a spatial filter
#'
#' @param x ([Spatial][sp::Spatial] object) to be filtered
#' @param y ([Spatial][sp::Spatial] object) to filter with
#' @param FUN (function) a spatial predicate, for example: [gIntersects][rgeos::gIntersects]
#' @param ... further arguments to `FUN`
#' @param verbose (logical)
#'
#' @importFrom sf st_as_sf st_transform st_union st_crs
#'
#' @return (typically) a subset of \code{spobj1}
#'
#' @export
filter_spatial <- function (
  x,
  y,
  FUN = NULL,
  ...,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[filter_spatial] ", ...)

  is_sf <- function (obj) inherits(x, c("sf", "sfc"))

  if (is_sf(x)) {
    if (!is_sf(y)) {
      msg("first converting via st_as_sf()")
      y <- sf::st_as_sf(y)
    }
    filter_spatial_sf(x, y, FUN = FUN, ..., verbose = verbose)
  } else {
    msg("try using st_as_sf() first; see ?filter_spatial")
    filter_spatial_sp(x, y, FUN = FUN, ..., verbose = verbose)
  }

}

filter_spatial_sf <- function (sf1, sf2, FUN = NULL, ..., verbose = FALSE) {

  if (!is.null(FUN)) {
    stop("`FUN` is not yet supported in `filter_spatial_sf`; complain to <dholstius@baaqmd.gov>")
  }

  msg <- function (...) if (isTRUE(verbose)) message("[filter_spatial] ", ...)
  if (is.null(FUN)) {
    msg("using (st_contains | st_overlaps)")
  }

  region <- sf::st_transform(sf::st_union(sf2), sf::st_crs(sf1))
  suppressMessages(contained <- reduce(st_contains(region, sf1), union))
  suppressMessages(overlapped <- reduce(st_overlaps(region, sf1), union))
  i <- union(contained, overlapped)
  return(sf1[i, ])

}

filter_spatial_sp <- function (spobj1, spobj2, FUN = NULL, ..., verbose = FALSE) {

  msg <- function (...) if (isTRUE(verbose)) message("[filter_spatial] ", ...)

  #
  # Define `gFilter`, a function that yields the (row) indices of
  # features in geom1 (based on relationship to geom2)
  #
  if (is.null(FUN)) {

    if (inherits(spobj1, "SpatialPoints")) {

      msg("defaulting to gContains")
      gFilter <- function (geom2, geom1, byid = TRUE) {
        gContains(geom2, geom1, byid = byid)
      }

    } else {

      msg("defaulting to gContainsOrOverlaps")
      gFilter <- function (geom2, geom1, byid = TRUE) {
        #first_pass <- gContainsOrOverlaps(gEnvelope(geom2), geom1, byid = byid)
        #second_pass <- gContainsOrOverlaps(geom2, geom1[first_pass, ], byid = byid)
        #return(second_pass)
        gContainsOrOverlaps(geom2, geom1, byid = byid)
      }

    }

  } else {

    msg("using ", deparse(substitute(FUN)))

    gFilter <- function (geom2, geom1, byid = TRUE) {
      ij <- FUN(geom2, geom1, byid = byid, ...)
      i <- which(as.logical(rowSums(ij, na.rm = TRUE)))
      return(i)
    }

  }

  proj1 <- suppress_warning(proj4string(spobj1), "CRS object has comment")
  proj2 <- suppress_warning(proj4string(spobj2), "CRS object has comment")

  if (is.na(proj1) && is.na(proj2)) {
    warning("both proj4strings are NA")
  } else if (is.na(proj1)) {
    stop("first spobj has NA proj4string")
  } else if (is.na(proj2)) {
    stop("second spobj has NA proj4string")
  } else if (proj1 != proj2) {
    warning("proj4strings don't match")
    spobj2 <- reproject(spobj2, CRS(proj1))
  } else {
    msg("proj4strings match (good)")
  }

  geom1 <- geometry(spobj1)
  geom2 <- geometry(spobj2)

  msg("applying gFilter")
  i <- gFilter(geom2, geom1, byid = TRUE) # TODO: add `...`

  filtered_geom <- geom1[i, ]
  filtered_FIDs <- row.names(filtered_geom)

  SPDF_CLASS_NAMES <- str_c(
    "Spatial",
    c("Points", "Polygons", "Polylines", "Grid"),
    "DataFrame")

  if (class(spobj1) %in% SPDF_CLASS_NAMES) {

    # This thing is a Spatial*DataFrame
    df <- spobj1@data

    filtered_df <- as.data.frame(df[i, , drop = FALSE])
    row.names(filtered_df) <- filtered_FIDs

    cls <- class(spobj1)
    construct_spobj <- get(cls)

    msg("re-merging filtered geometry and filtered data")
    result <- construct_spobj(filtered_geom, filtered_df)

  } else {

    # This thing is just a Spatial* object
    result <- filtered_geom

  }

  stopifnot(setequal(class(result), class(spobj1)))
  return(result)

}
