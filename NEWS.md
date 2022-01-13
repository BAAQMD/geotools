# NEWS

## Changes in version 2022-01-13

Deleted:
- `meters()`
- `as_square_meters()`

Defunct:
- `merge` methods for `Spatial*DataFrame`
- `polygon_areas()` made obsolete by `st_area()`

Deprecated:
- `fortify()` made obsolete by `sf::geom_sf()`
- `reproject()` superseded by `sf::st_transform()`

Other:
- `drop_geometry()` now leverages `sf::st_drop_geometry()`
- `read_ncdf_raster()` now returns `terra::SpatRaster` instead of `raster::RasterLayer`
- `raster_overlay()` coerces its input to `raster::RasterBrick`
