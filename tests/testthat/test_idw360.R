context("Wind maps")

test_that("idw360", {
  library(sp)
  df <- structure(list(date = structure(c(17472, 17472, 17472, 17472, 17472),
                                        class = "Date"),
                       hour = c(15, 15, 15, 15, 15),
                       station_code = c("ACO", "AJM", "AJU", "BJU", "CHO"),
                       value = c(36, 6, 7, 319, 214),
                       lat = c(19.635501, 19.272161, 19.154286,
                               19.370464, 19.266948),
                       lon = c(-98.912003, -99.207744, -99.162611,
                               -99.159596, -98.886088)),
                  .Names = c("date", "hour", "station_code",
                             "value", "lat", "lon"),
                  row.names = c(NA, -5L),
                  class = c("data.frame"))
  station_loc <- df[, c("lat", "lon", "value")]
  coordinates(station_loc) <- ~lon + lat
  crs_string <- "+proj=longlat +ellps=WGS84 +no_defs +towgs84=0,0,0"
  proj4string(station_loc) <- sp::CRS(crs_string)

  # create a 10x10 grid based on the stations
  pixels <- 10
  mxc_grid <- expand.grid(x = seq( (min(coordinates(station_loc)[, 1]) - .1),
                                (max(coordinates(station_loc)[, 1]) + .1),
                                length.out = pixels),
                          y = seq( (min(coordinates(station_loc)[, 2]) - .1),
                                (max(coordinates(station_loc)[, 2]) + .1),
                                length.out = pixels))

  mxc_grid_pts <- SpatialPixels(SpatialPoints( (mxc_grid)))
  mxc_grid_pts <- as(mxc_grid_pts, "SpatialGrid")
  proj4string(mxc_grid_pts) <- CRS(crs_string)

  # Inverse distance weighting
  idw <- idw360(station_loc$value, station_loc, mxc_grid_pts)
  expect_type(idw$pred, "double")
  expect_equal(length(idw$pred), 10 * 10)
  expect_true(all(idw$pred <= 360))
  expect_true(all(idw$pred >= 0))

  # First column x/longitud, second y/latitude
  locations <- data.frame(lon = c(1, 2), lat = c(1, 2))
  coordinates(locations) <- ~lon + lat
  # Wind direction values in degrees
  values <- c(55, 355)
  # The grid for which to extrapolate the values
  grid <- data.frame(lon = c(1, 2, 1, 2), lat = c(1, 2, 2, 1))
  coordinates(grid) <- ~lon + lat

  idw <- idw360(values, locations, grid)
  expect_equal(idw, data.frame(pred = c(55, 355, 25, 25)))
})
