#' pull_fips
#'
#' Extract state, county, and tract components from rownames.
#'
#' @param geodata geodata with rownames that are FIPS codes
#'
#' @export
pull_fips <- function (geodata) {
  FIDs <- row.names(geodata)
  FIPS_pattern <- "([0-9]{2})([0-9]{3})([0-9]{6})"
  FIPS_parts <- str_match(FIDs, FIPS_pattern)[, -1, drop = FALSE]
  colnames(FIPS_parts) <- c("state", "county", "tract")
  return(FIPS_parts)
}
