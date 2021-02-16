#' Experimental --- use at your own risk
#'
#' @export
adjust_bbox <- function (
  bb,
  aspect,
  hjust = 0,
  vjust = 0
) {
  bb_width <- with(as.list(bb), (xmax - xmin))
  bb_height <- with(as.list(bb), (ymax - ymin))
  bb_aspect <- bb_width / bb_height
  if (bb_aspect == aspect) {
    return(bb)
  } else if (bb_aspect > aspect) {
    height_desired <- bb_width / aspect
    height_gap <- height_desired - bb_height
    bb <- bb + height_gap * c(xmin = 0, ymin = vjust - 0.5, xmax = 0, ymax = vjust + 0.5)
  } else if (bb_aspect < aspect) {
    width_desired <- bb_height * aspect
    width_gap <- width_desired - bb_width
    bb <- bb + width_gap * c(xmin = hjust - 0.5, ymin = 0, xmax = hjust + 0.5, ymax = 0)
  } else {
    stop("should never get here")
  }
  return(bb)
}
