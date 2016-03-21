test_that("station pollution data matches api", {
  df_min <- get_station_data("MINIMOS", "pm10", 2010)
  df_max <- get_station_data("MAXIMOS", "pm10", 2010)
  df_horarios <- get_station_data("HORARIOS", "pm10", 2010)

  expect_equal(unname(unlist(subset(df_min, date == as.Date("2010-01-01"))$value)),
               c(NA, NA, NA, NA, 11, NA, NA, NA, NA, NA,
                 14, NA, 9, NA, NA,
                 12, 13, NA, 8, NA, 15, NA, NA, 6, 10, 11,
                 NA, 9, 7, NA, NA, NA, 7, 19, NA))
  expect_equal(unname(unlist(subset(df_max, date == as.Date("2010-01-01"))$value)),
               c(NA, NA, NA, NA, 102, NA, NA, NA, NA,
                 NA, 87, NA,
                 160, NA, NA, 122, 122, NA, 83, NA, 218, NA, NA,
                 273, 115, 100,
                 NA, 167, 283, NA, NA, NA, 275, 248, NA))
  expect_equal(unname(unlist(subset(df_horarios, date == as.Date("2010-01-01") &
                                      hour == 1)$value)),
               c(NA, NA, NA, NA, 62, NA, NA, NA, NA, NA, 81,
                 NA, 107, NA, NA, 115, 104, NA, 83, NA, 129, NA, NA, 225, 73,
                 71, NA, 98, 182, NA, NA, NA, 275, 195, NA))
})


test_that("zone pollution data matches api", {
  df_max_o3 <- get_zone_data("MAXIMOS", "O3", c("NO", "NE", "CE"),
                          "2015-12-25", "2016-01-01")
  df_max_tz <- get_zone_data("MAXIMOS", c("O3", "PM10"), c("TZ"),
                          "2015-12-31", "2016-01-06")
  df_horarios <- get_zone_data("HORARIOS", c("O3", "PM10"), c("NO", "NE", "CE"),
                               "2015-12-25", "2016-01-01")

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

