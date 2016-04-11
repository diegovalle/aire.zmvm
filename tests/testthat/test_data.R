test_that("convert time correctly parses string", {
  expect_equal(convert_time("11:00 h, miércoles 06 de abril de 2016"),
               "2016-04-06 11:00:00")
  expect_equal(convert_time("11:00 h,
				miércoles 06 de abril de 2016
			"),  "2016-04-06 11:00:00")
})

test_that(("convert units"), {
  # df_max_zone <- get_zone_data("MAXIMOS", c("NO2"), c("TZ"),
  #               "2015-01-01", "2015-12-31")
  # df_max_station <- get_station_data("MAXIMOS", "NO2", 2015)
  #
  # library(dplyr)
  # zone_max <- df_max_zone %>%
  #   group_by(date) %>%
  #   summarise(max = max(value, na.rm = TRUE))
  # station_max <- df_max_station %>%
  #   group_by(date) %>%
  #   summarise(max = max(value, na.rm = TRUE)) %>%
  #   mutate(max_imeca = convert_to_imeca(max, "NO2"))
  #
  # expect_equal(zone_max$max, station_max$max_imeca)
  expect_equal(convert_to_imeca(-1, "NO2"), NA)
  expect_equal(convert_to_imeca(NA, "NO2"), NA)
  expect_equal(convert_to_imeca(c(450, 350, 250), "NO2"), c(215,167,119))
  expect_equal(convert_to_imeca(c(450, 350, 48), c("NO2", "NO2", "O3")), c(215,167,34))

  expect_equal(convert_to_imeca(90, "NO2"), 43)
  expect_equal(convert_to_imeca(75, "NO2"), 36)
  #expect_equal(convert_to_imeca(150, "NO2"), 71)
  expect_equal(convert_to_imeca(250, "NO2"), 119)
  expect_equal(convert_to_imeca(350, "NO2"), 167)
  expect_equal(convert_to_imeca(450, "NO2"), 215)

  expect_equal(convert_to_imeca(48, "O3"), 34)
  expect_equal(convert_to_imeca(67, "O3"), 48)
  expect_equal(convert_to_imeca(77, "O3"), 63)
  expect_equal(convert_to_imeca(205, "O3"), 201)
  expect_equal(convert_to_imeca(72, "O3"), 53)
  expect_equal(convert_to_imeca(98, "O3"), 103)
  expect_equal(convert_to_imeca(170, "O3"), 166)

  # expect_equal(convert_to_imeca(1.5, "CO"), 14)
  # expect_equal(convert_to_imeca(6, "CO"), 55)
  # expect_equal(convert_to_imeca(12, "CO"), 109)
  # expect_equal(convert_to_imeca(18, "CO"), 164)
  # expect_equal(convert_to_imeca(24, "CO"), 218)
})

test_that("station pollution data matches api", {
  df_min_2016 <- get_station_data("MINIMOS", "PM10", 2016)
  df_max_2016 <- get_station_data("MAXIMOS", "PM10", 2016)
  df_min_2015 <- get_station_data("MINIMOS", "PM10", 2015)
  df_max_2015 <- get_station_data("MAXIMOS", "O3", 2015)
  df_horarios_2010 <- get_station_data("HORARIOS", "PM10", 2010)
  df_horarios_2016 <- get_station_data("HORARIOS", "O3", 2016)

  expect_false("CHA" %in% unique(df_max_2015$station_code))
  expect_true("MON" %in% unique(df_max_2015$station_code))

  expect_equal(unname(unlist(subset(df_min_2016, date == as.Date("2016-01-03"))$value)),
               c(21, 26, NA, 4, 26, 22, NA, NA, NA, 17, NA, 6, 6, 3, NA, 29,
                 NA, 24, NA, NA, NA, 21, 27, NA, NA, NA, NA, 50, NA, NA, NA, 11,
                 NA, NA, 25, 3, NA, NA, NA, 21, 47))
  expect_equal(unname(unlist(subset(df_max_2016, date == as.Date("2016-01-05"))$value)),
               c(52, 30, NA, 82, 76, 242, NA, NA, NA, 84, NA, 42, 112, 86, NA,
                 88, 32, 64, NA, NA, NA, 95, 71, NA, NA, NA, NA, 116, NA, NA,
                 NA, 63, NA, NA, 111, 110, NA, NA, 75, 108, 151))

  expect_equal(unname(unlist(subset(df_min_2015, date == as.Date("2015-01-01"))$value)),
               c(17, 11, 11, 9, NA, 38, 16, 12, 13, 18, 12, 9, 9, 106, 10, 103,
                 NA, 7, 15, 4, NA))
  expect_equal(unname(unlist(subset(df_max_2015, date == as.Date("2015-02-15"))$value)),
               c(79, 72, 52, 60, NA, 70, NA, NA, 71, 38, 68, 61, 72, 83, NA,
                 79, 85, NA, 82, 73, 76, 69, 94, 67, 61, 86, 43, 59, 72, 73, 69,
                 56))

  expect_equal(unname(unlist(subset(df_horarios_2010, date == as.Date("2010-01-01") &
                                      hour == 1)$value)),
               c(115, 98, 195, 104, 83, 62, 182, 275, 73, 225, 81, 129, 71,
                 107))
  expect_equal(unname(unlist(subset(df_horarios_2016, date == as.Date("2016-02-29") &
                                      hour == 1)$value)),
               c(NA, 28, 1, NA, NA, 5, 2, 6, NA, 30, 10, 9, 27, 14, 25, 15,
                 5, 30, 7, NA, 13, NA, 7, 15, 37, 17, 10, NA, 11, NA, NA, NA,
                 NA, NA, NA, NA, 10, NA, NA, 2, 3, NA, 18))
})


test_that("zone pollution data matches api", {
  df_max_o3 <- suppressWarnings(get_zone_data("MAXIMOS", "O3", c("NO", "NE", "CE"),
                          "2015-12-25", "2016-01-01"))
  df_max_tz <- suppressWarnings(get_zone_data("MAXIMOS", c("O3", "PM10"), c("TZ"),
                          "2015-12-31", "2016-01-06"))
  df_horarios <- suppressWarnings(get_zone_data("HORARIOS", c("O3", "PM10"),
                                                c("NO", "NE", "CE"),
                               "2015-12-25", "2016-01-01"))

  expect_warning(get_zone_data("MAXIMOS", "O3", c("NO", "NE", "CE"),
                               "2015-12-25", "2016-01-01"))
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


