#' raster_sum
#'
#' @param layer_list list of something that can be passed to [terra::rast()]
#' @param weights (optional) numeric, of same length as `layer_list`
#' @param na.rm (logical)
#' @param ... options for writing files, as in [terra::writeRaster()]
#'
#' @importFrom progress progress_bar
#' @importFrom stringr str_glue
#'
#' @export
raster_sum <- function (
  layer_list,
  weights = NULL,
  na.rm = TRUE,
  ...,
  progress = TRUE,
  cache = TRUE,
  chunk_size = 100,
  verbose = FALSE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[raster_sum] ", ...)

  n_layers <- length(layer_list)
  info_msg <- stringr::str_glue("summing n = {format_count(n_layers)} layers")
  msg(info_msg)

  # Create progress bar to be shared by all invocations of raster_sum__(...),
  # even recursive invocations. See below (in raster_sum.R) for definition
  # of raster_sum__().
  if (isTRUE(progress)) {
    progress <- progress::progress_bar$new(
      format = "[:bar] :percent (:current/:total) elapsed: :elapsedfull eta: :eta",
      total = n_layers,
      width = 80,
      show_after = 0)
  }

  summed <-
    raster_sum__(
      layer_list,
      weights = weights,
      na.rm = na.rm,
      ...,
      progress = progress,
      cache = cache,
      chunk_size = chunk_size,
      verbose = verbose)

  return(summed)

}

#' raster_sum__
#'
#' @describeIn raster_sum uncached variant
#'
#' @importFrom here here
#' @importFrom purrr map map2
#' @importFrom terra rast weighted.mean writeRaster
#' @importFrom fs file_exists path_ext_set file_delete
#'
raster_sum__ <- function (
  layer_list,
  weights = NULL,
  progress = NULL,
  chunk_size = 30,
  na.rm = TRUE,
  ...,
  cache = TRUE,
  cache_dir = here::here("Cache", "raster_sum__"),
  verbose = FALSE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[raster_sum__] ", stringr::str_glue(...))

  n_layers <- length(layer_list)
  if (is.null(weights)) weights <- rep(1, n_layers)
  stopifnot(length(weights) == n_layers)

  # Shortcut if n = 1
  if (n_layers == 1) {

    summed <- layer_list[[1]] * weights

    # Update the progress bar
    if (isFALSE(is.null(progress))) {
      progress$tick(len = n_layers)
    }

  } else if (n_layers <= chunk_size) {

    # This layer's digest will be a digest of digests
    layer_digests <- purrr::map(layer_list, attr, "digest")
    no_digest_yet <- which(lengths(layer_digests) == 0)
    layer_digests[no_digest_yet] <- map_chr(layer_list[no_digest_yet], raster_digest)

    layer_digests <- unlist(layer_digests)
    stopifnot(length(layer_digests) == n_layers)
    i <- order(layer_digests)
    content <- str_c("na.rm-", na.rm, "-", str_c(layer_digests[i], weights[i], sep = "-", collapse = "-"))
    key <- digest::digest(content, algo = "sha1")

    msg("content is: ", content)

    # Compute (possible) path to cached result
    path <- file.path(cache_dir, fs::path_ext_set(key, ".grd"))

    if (cache == "rm") {
      if (fs::file_exists(path)) {
        msg("deleting: ", path)
        fs::file_delete(path)
      }
    }

    if (isTRUE(cache) && file.exists(path)) {
      summed <- terra::rast(path)
    } else {
      # Compute the actual weighted sum
      rst_obj <- terra::rast(layer_list)
      summed <- sum(weights) * terra::weighted.mean(rst_obj, w = weights, na.rm = na.rm, ...)
      if (isTRUE(cache)) {
        # Write the weighted sum to disk
        summed <- terra::writeRaster(summed, filename = path, overwrite = FALSE)
      }
    }

    # If every layer had the same name, then use it to name the result
    nm <- unique(map_chr(layer_list, names))
    if (length(nm) == 1) {
      names(summed) <- nm
    }

    # Update the progress bar
    if (isFALSE(is.null(progress))) {
      progress$tick(len = n_layers)
    }

  } else {

    # Don't update the progress bar

    stopifnot(all(chunk_size > 0))
    stopifnot(all(chunk_size == round(chunk_size)))

    # Assign layers to "chunks" of size no more than n=chunk_size
    n_chunks <- ceiling(n_layers / chunk_size)
    i <- rep(1:n_chunks, length.out = n_layers)
    sub_layers <- split(layer_list, i)
    sub_weights <- split(weights, i)

    # Invoke raster_sum__() recursively to yield a sub-total for
    # each chunk.
    sub_totals <-
      purrr::map2(
        sub_layers,
        sub_weights,
        raster_sum__,
        progress = progress,
        chunk_size = chunk_size,
        ...,
        na.rm = na.rm,
        cache = cache,
        verbose = verbose)

    # Invoke raster_sum__() once more, this time to yield a total
    # for those sub-totals. (This might itself be cached.)
    # Don't count this action toward the progress bar.
    # summed <-
    #   raster_sum__(
    #     sub_totals,
    #     weights = rep(1, length(sub_totals)),
    #     progress = NULL,
    #     ...,
    #     na.rm = na.rm,
    #     cache = FALSE, # WAS: cacce
    #     verbose = verbose)
    summed <- sum(terra::rast(sub_totals), na.rm = na.rm)

  }

  # Return the result
  attr(summed, "digest") <- raster_digest(summed)
  return(summed)

}
