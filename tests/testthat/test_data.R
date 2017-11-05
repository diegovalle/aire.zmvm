test_that(".convert_time correctly parses string", {
  expect_equal(.convert_time("11:00 h, miércoles 06 de abril de 2016"),
               "2016-04-06 11:00:00")
  expect_equal(.convert_time("11:00 h,
				miércoles 06 de abril de 2016
			"),  "2016-04-06 11:00:00")
  expect_equal(suppressWarnings(.convert_time("24:00 h,
				lunes 23 de mayo de 2016
			")),  "2016-05-24 00:00:00")

  expect_equal(.convert_time("23:00 h,
				domingo 22 de mayo de 2016"),  "2016-05-22 23:00:00")
  expect_equal(.convert_time("20:00 h,
				domingo 22 de mayo de 2016
			"),  "2016-05-22 20:00:00")
  expect_equal(.convert_time("01:00 h,
				lunes 23 de mayo de 2016
			"),  "2016-05-23 01:00:00")

  expect_equal(.convert_time("23:00 h,
				S&aacute;bado 21 de mayo de 2016
			"),  "2016-05-21 23:00:00")
  expect_equal(.convert_time("01:00 h,
				domingo 22 de mayo de 2016
			"),  "2016-05-22 01:00:00")
  expect_equal(suppressWarnings(.convert_time("24:00 h,
				S&aacute;bado 21 de mayo de 2016
			")),  "2016-05-22 00:00:00")

  expect_equal(suppressWarnings(.convert_time("24:00 h,
				lunes 30 de mayo de 2016")), "2016-05-31 00:00:00")

  expect_equal(suppressWarnings(.convert_time("24:00 h,
  mi&eacute;rcoles 01 de junio de 2016")), "2016-06-02 00:00:00")

  expect_warning(.convert_time("24:00 h,
  mi&eacute;rcoles 01 de junio de 2016"))

})

test_that(("convert units"), {
  expect_equal(convert_to_imeca(-1, "NO2", showWarnings = FALSE), NA)
  expect_equal(convert_to_imeca(NA, "NO2", showWarnings = FALSE), NA)
  expect_equal(convert_to_imeca(c(450, 350, 250), "NO2", showWarnings = FALSE), c(215,167,119))
  expect_equal(convert_to_imeca(c(450, 350, 48), c("NO2", "NO2", "O3"), showWarnings = FALSE), c(215,167,34))

  expect_equal(convert_to_imeca(90, "NO2", showWarnings = FALSE), 43)
  expect_equal(convert_to_imeca(75, "NO2", showWarnings = FALSE), 36)
  #expect_equal(convert_to_imeca(150, "NO2", showWarnings = FALSE), 71)
  expect_equal(convert_to_imeca(250, "NO2", showWarnings = FALSE), 119)
  expect_equal(convert_to_imeca(350, "NO2", showWarnings = FALSE), 167)
  expect_equal(convert_to_imeca(450, "NO2", showWarnings = FALSE), 215)

  expect_equal(convert_to_imeca(48, "O3", showWarnings = FALSE), 34)
  expect_equal(convert_to_imeca(67, "O3", showWarnings = FALSE), 48)
  expect_equal(convert_to_imeca(77, "O3", showWarnings = FALSE), 63)
  expect_equal(convert_to_imeca(205, "O3", showWarnings = FALSE), 201)
  expect_equal(convert_to_imeca(72, "O3", showWarnings = FALSE), 53)
  expect_equal(convert_to_imeca(98, "O3", showWarnings = FALSE), 103)
  expect_equal(convert_to_imeca(170, "O3", showWarnings = FALSE), 166)

  expect_equal(convert_to_imeca(1.5, "CO", showWarnings = FALSE), 14)
  expect_equal(convert_to_imeca(6, "CO", showWarnings = FALSE), 55)
  # expect_equal(convert_to_imeca(12, "CO", showWarnings = FALSE), 109)
  expect_equal(convert_to_imeca(18, "CO", showWarnings = FALSE), 164)
  # expect_equal(convert_to_imeca(24, "CO", showWarnings = FALSE), 218)

  expect_equal(convert_to_imeca(80, "PM10", showWarnings = FALSE), 102)

  expect_warning(convert_to_imeca(80, "PM10"))
  expect_silent(convert_to_imeca(80, "PM10", showWarnings = FALSE))
})

test_that("station pollution data matches api", {
  skip_on_cran()

  df_min_2016 <- get_station_data("MINIMOS", "PM10", 2016, progress = NULL)
  df_max_2016 <- get_station_data("MAXIMOS", "PM10", 2016)
  df_min_2015 <- get_station_data("MINIMOS", "PM10", 2015)
  df_max_2015 <- get_station_data("MAXIMOS", "O3", 2015)
  df_max_2005 <- get_station_data("MAXIMOS", "SO2", 2005)
  df_wdr_2005 <- get_station_data("MAXIMOS", "WDR", 2005)
  df_horarios_2010 <- get_station_data("HORARIOS", "PM10", 2010)
  df_horarios_2016 <- get_station_data("HORARIOS", "O3", 2016)

  # Check that PM25 is correctly coded without a '.'
  expect_true(unique(get_station_data("MAXIMOS", "PM25", 2004:2005)$pollutant) == "PM25")

  #expect_false(all(stringr::str_detect(unique(df_horarios_2016$station_code), "[:space:]")))
  #expect_false(all(stringr::str_detect(unique(df_horarios_2016$station_code), "\ufffd")))

  expect_false("CHA" %in% unique(df_max_2015$station_code))
  expect_false("CHA" %in% unique(df_max_2005$station_code))
  expect_true("MON" %in% unique(df_max_2015$station_code))

  expect_equal(unname(unlist(subset(df_min_2016, date == as.Date("2016-01-03"))$value)),
               c(21, 26, 0, 4, 26, 18, 0, NA, 0, 17, 0, 6, 6, 3, 0, 29, NA,
                 24, 0, 0, NA, 21, 27, NA, 0, NA, NA, 50, NA, 0, 0, 11, NA, 0,
                 25, 3, 0, 0, NA, 21, 47))
  expect_equal(unname(unlist(subset(df_max_2016, date == as.Date("2016-01-05"))$value)),
               c(52, 30, 0, 82, 76, 242, 0, NA, 0, 84, 0, 42, 112, 86, 0, 88,
                 32, 64, 0, 0, NA, 95, 71, NA, 0, NA, NA, 116, NA, 0, 0, 63, NA,
                 0, 111, 110, 0, 0, 75, 108, 151))

  expect_equal(unname(unlist(subset(df_min_2015, date == as.Date("2015-01-01"))$value)),
               c(17, NA, NA, 11, NA, 11, NA, NA, NA, 9, NA, NA, 38, 16, NA,
                 12, NA, 13, NA, NA, NA, 18, 12, NA, NA, 9, NA, 9, 106, NA, 10,
                 103, NA, NA, NA, 7, NA, NA, 15, 4, NA))
  expect_equal(unname(unlist(subset(df_max_2015, date == as.Date("2015-02-15"))$value)),
               c(79, 72, 52, 60, NA, NA, NA, 70, NA, NA, NA, NA, 71, 38, 68,
                 NA, 61, NA, 72, NA, 83, NA, 79, 85, NA, 82, 73, NA, 76, 69, 94,
                 67, NA, 61, NA, NA, 86, 43, 59, 72, 73, 69, 56))
  expect_equal(unname(unlist(subset(df_max_2005, date == as.Date("2005-03-03"))$value)),
               c(NA, NA, 14, 139, 55, NA, NA, 11, NA, NA, NA, NA, NA, 202, NA,
                 NA, 7, NA, 8, 30, 12, NA, 32, 49, 12, NA, NA, 7, 101, 7, 15,
                 NA, 25, 20, NA, NA, 11, 10, 19, 25))

  expect_equal(unname(unlist(subset(df_horarios_2010, date == as.Date("2010-01-01") &
                                     hour == 1)$value)),
              c(115, 98, 195, 104, 83, 62, 182, 275, 73, 225, 81, 129, 71,
                107))
  expect_equal(unname(unlist(subset(df_horarios_2016, date == as.Date("2016-02-29") &
                                      hour == 1)$value)),
               c(NA, 28, 1, NA, NA, 5, 2, 6, NA, 30, 10, 9, 27, 14, 25, 15,
                 5, 30, 7, NA, 13, NA, 7, 15, 37, 17, 10, NA, 11, NA, NA, 0, NA,
                 NA, NA, 0, 10, NA, 0, 2, 3, NA, 18))
})


test_that("zone pollution data matches api", {
  skip_on_cran()

  df_max_o3 <- suppressWarnings(get_zone_data("MAXIMOS", "O3", c("NO", "NE", "CE"),
                          "2015-12-25", "2016-01-01"))
  df_max_tz <- suppressWarnings(get_zone_data("MAXIMOS", c("O3", "PM10"), c("TZ"),
                          "2015-12-31", "2016-01-06"))
  df_horarios <- suppressWarnings(get_zone_data("HORARIOS", c("O3", "PM10"),
                                                c("NO", "NE", "CE"),
                               "2015-12-25", "2016-01-01"))

  expect_warning(get_zone_data("MAXIMOS", "O3", c("NO", "NE", "CE"),
                               "2015-12-25", "2016-01-01"))
  expect_warning(get_zone_data("MAXIMOS", "SO2", c("NO", "NE", "CE"),
                               "2017-02-25", "2017-05-01"))
  expect_silent(get_zone_data("MAXIMOS", "O3", c("NO", "NE", "CE"),
                              "2015-12-25", "2016-01-01",
                              showWarnings = FALSE))
  expect_equal(subset(df_max_o3, zone == "NO" &
                        pollutant == "O3")$value,
               c(109, 51, 29, 49, 36, 104, 92, 119))
  expect_equal(subset(df_max_o3, zone == "NE" &
                        pollutant == "O3")$value,
               c(122, 48, 32, 59, 38, 106, 115, 125))
  expect_equal(unique(df_max_o3$zone), c("NO", "NE", "CE"))

  expect_equal(subset(df_max_tz, zone == "NO" &
                        pollutant == "PM10")$value,
               c(107, 133, 129, 80, 104, 103, 78))
  expect_equal(subset(df_max_tz, zone == "SO" &
                        pollutant == "O3")$value,
               c(124,132,69,57,29,44,33))
  expect_equal(unique(df_max_tz$zone), c("NO", "NE", "CE", "SO", "SE"))

  expect_equal(subset(df_horarios, zone == "CE" &
                        pollutant == "PM10" &
                        date == "2015-12-25")$value,
               c(107, 107, 108, 108, 110, 112, 113, 113, 117, 119, 126, 127,
                 126, 126, 127, 127, 127, 126, 126, 126, 126, 125, 124, 124))
  expect_equal(subset(df_horarios, zone == "NO" &
                        pollutant == "O3" &
                        hour == 1 &
                        date == "2016-01-01")$value,
               c(5))
  expect_equal(unique(df_horarios$zone), c("NO", "NE", "CE"))
  expect_equal(unique(df_horarios$pollutant), c("O3", "PM10"))
})

test_that("latest data", {
  skip_on_cran()

  df <- get_latest_data()
  expect_gt(nrow(df), 0)
  expect_type(df$value, "integer")
  expect_type(df$datetime, "character")
  expect_false(all(is.na(df$datetime)))
})

test_that("idw360", {
  library(sp)
  df <- structure(list(date = structure(c(17472, 17472, 17472, 17472, 17472), class = "Date"),
                       hour = c(15, 15, 15, 15, 15),
                       station_code = c("ACO", "AJM", "AJU", "BJU", "CHO"),
                       value = c(36, 6, 7, 319, 214),
                       lat = c(19.635501, 19.272161, 19.154286, 19.370464, 19.266948),
                       lon = c(-98.912003, -99.207744, -99.162611, -99.159596, -98.886088)),
                  .Names = c("date", "hour", "station_code", "value", "lat", "lon"),
                  row.names = c(NA, -5L),
                  class = c("data.frame"))
  station_loc <- df[,c("lat", "lon", "value")]
  coordinates(station_loc) <- ~lon+lat
  proj4string(station_loc) <- sp::CRS("+proj=longlat +ellps=WGS84 +no_defs +towgs84=0,0,0")

  # create a 10x10 grid based on the stations
  pixels = 10
  mxc_grid <- expand.grid(x=seq((min(coordinates(station_loc)[ ,1]) - .1),
                                (max(coordinates(station_loc)[ ,1]) + .1),
                                length.out = pixels),
                          y=seq((min(coordinates(station_loc)[ ,2]) - .1),
                                (max(coordinates(station_loc)[ ,2]) + .1),
                                length.out = pixels))

  mxc_grid_pts <- SpatialPixels(SpatialPoints((mxc_grid)))
  mxc_grid_pts <- as(mxc_grid_pts, "SpatialGrid")
  proj4string(mxc_grid_pts) <- CRS("+proj=longlat +ellps=WGS84 +no_defs +towgs84=0,0,0")

  # Inverse distance weighting
  idw <- idw360(station_loc$value, station_loc, mxc_grid_pts)
  expect_type(idw$pred, "double")
  expect_equal(length(idw$pred), 10*10)
  expect_true(all(idw$pred <= 360))
  expect_true(all(idw$pred >= 0))

  # First column x/longitud, second y/latitude
  locations <- data.frame(lon = c(1, 2), lat = c(1, 2))
  coordinates(locations) <- ~lon+lat
  # Wind direction values in degrees
  values <- c(55, 355)
  # The grid for which to extrapolate the values
  grid <- data.frame(lon = c(1, 2, 1, 2), lat = c(1, 2, 2, 1))
  coordinates(grid) <- ~lon+lat

  idw <- idw360(values, locations, grid)
  expect_equal(idw, data.frame(pred = c(55, 355, 25 ,25)))
})

