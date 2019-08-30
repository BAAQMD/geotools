#' gContainsOrOverlaps
#'
#' @description Default basis for `gFilter` is `gContainsOrOverlaps`
#' @note Unlike most `rgeos` functions, this returns row.names !!
#'
#' @importFrom purrr is_empty
#'
#' @export
gContainsOrOverlaps <- function (geom2, geom1, byid = TRUE) {

  # First pass: test for *containment*
  ij1 <- gContains(geom2, geom1, byid = byid)
  is_contained <- as.logical(rowSums(ij1))

  # Preliminary result: may be modified by second pass (below)
  result <- row.names(geom1)[is_contained]

  # Second pass: test for *overlap* --- but only consider features
  # not already passing the first test (i.e., containment)
  remainder <- geom1[which(!is_contained), ]
  if (!purrr::is_empty(remainder)) {
    ij2 <- gOverlaps(geom2, remainder, byid = byid)
    is_overlapping <- as.logical(rowSums(ij2))
    result <- union(result, row.names(remainder[is_overlapping]))
  }

  # Final result
  return(result)

}
