# NEWS

## Changes in version 2022-04-29

- `mapview::mapviewOptions(fgb = FALSE)` now set on load
    - See https://stackoverflow.com/questions/65485747/mapview-points-not-showing-in-r
- Greatly simplified some older methods that accept `Spatial*` objects
   - Use `sf` methods under hood, converting via `st_as_sf()` first
   - Result still inherits from `Spatial` if first argument does
   - Retaining these for backwards-compatibility till `rdgal` expires in 2023
- Changes to S3 methods for `Spatial*DataFrame`
   - `mutate()` is un-deprecated
   - `filter()` and `select()` now rely on `sf` methods under the hood
- All tests now succeed
    - Some empty tests were removed
- Added quasi-experimental raster methods
    - These rely on `terra`
    - `raster_sum()`, a fast and cached way of summing rasters, optionally with weights
    - `raster_digest()`, which hashes result of `terra::values()`

## Changes in version 2022-01-13

Deleted:
- `meters()`
- `as_square_meters()`

Defunct:
- S3 `merge` methods for `Spatial*DataFrame`
- `polygon_areas()` made obsolete by `st_area()`

Deprecated:
- S3 `filter` methods for `Spatial*DataFrame`
- S3 `mutate` methods for `Spatial*DataFrame`
- `fortify()` made obsolete by `sf::geom_sf()`
- `reproject()` superseded by `sf::st_transform()`

Other:
- `drop_geometry()` now leverages `sf::st_drop_geometry()`
- `read_ncdf_raster()` now returns `terra::SpatRaster` instead of `raster::RasterLayer`
- `raster_overlay()` coerces its input to `raster::RasterBrick`
