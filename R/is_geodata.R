is_geodata <- function (x, verbose = getOption("verbose")) {

  msg <- function (...) if(isTRUE(verbose)) message("[is_geodata] ", ...)

  if (inherits(x, c("sf", "sfc"))) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}
