test_p4s <- "+proj=lcc +lat_1=30 +lat_2=60 +lat_0=37 +lon_0=-120.5 +x_0=233000 +y_0=28000 +a=6370000 +b=6370000 +units=km +no_defs"
test_crs <- st_crs(test_p4s)
n_row <- 239
n_col <- 189
test_bb <- c(xmin = 0, xmax = n_col, ymin = 0, ymax = n_row)
test_envelope <- st_envelope(test_bb, crs = test_crs)
test_dsn <- file.path("tests", "testthat")
write_shp(test_envelope, dsn = test_dsn, layer = "CALPUFF_envelope.shp")
