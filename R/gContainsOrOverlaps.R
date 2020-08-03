#' gContainsOrOverlaps
#'
#' This is the default predicate used by `gFilter()`.
#'
#' @param geom2 `Spatial*` object
#' @param geom1 `Spatial*` object
#' @param byid passed to `rgeos::gContains()` and `rgeos::gOverlaps()`
#'
#' @note Unlike most `rgeos` functions, this returns row.names !!
#'
#' @importFrom rlang is_empty
#' @importFrom rgeos gContains gOverlaps
#'
#' @export
gContainsOrOverlaps <- function (geom2, geom1, byid = TRUE) {

  # First pass: test for *containment*
  ij1 <- rgeos::gContains(geom2, geom1, byid = byid)
  is_contained <- as.logical(rowSums(ij1))

  # Preliminary result: may be modified by second pass (below)
  result <- row.names(geom1)[is_contained]

  # Second pass: test for *overlap* --- but only consider features
  # not already passing the first test (i.e., containment)
  remainder <- geom1[which(!is_contained), ]
  if (rlang::is_empty(remainder)) {
    # do nothing
  } else {
    ij2 <- rgeos::gOverlaps(geom2, remainder, byid = byid)
    is_overlapping <- as.logical(rowSums(ij2))
    result <- union(result, row.names(remainder[is_overlapping]))
  }

  # Final result
  return(result)

}
