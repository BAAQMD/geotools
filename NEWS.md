# NEWS

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