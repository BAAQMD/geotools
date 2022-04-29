#' raster_digest
#'
#' @param rst_layer (raster) `terra` object
#' @param algo (character) passed to [digest::digest()]
#'
#' @importFrom terra values sources
#' @importFrom purrr map_chr
#' @importFrom digest digest
#'
#' @return hash, as character, according to `algo`
#'
#' @export
raster_digest <- function (rst_layer, algo = "sha1") {
  part_objs <- list(names(rst_layer), terra::values(rst_layer), terra::sources(rst_layer))
  part_hashes <- purrr::map_chr(part_objs, digest::digest, algo = algo)
  hash <- digest::digest(part_hashes, algo = algo)
  return(hash)
}
