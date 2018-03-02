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

test_that("is.integer2 works", {
  expect_true(is.integer2(1986))
  expect_false(is.integer2(99.2))
  expect_false(is.integer2(99.0000000001))
  expect_false(is.integer2(character(0)))
  expect_false(is.integer2(NULL))
  expect_false(is.integer2(1.0000000000001))
  expect_true(is.integer2(2018))
  expect_true(is.integer2(c(1999:2010)[3]))

})

test_that("get_station_data matches website", {
  skip_on_cran()

  # Invalid function arguments
  expect_error(get_station_data("INVALID", "PM10", 2016))
  expect_error(get_station_data("MAXIMOS", "INVALID", 2016))
  expect_error(get_station_data("MAXIMOS", "PM10", 2016.6))
  expect_error(get_station_data("MAXIMOS", "PM10", 1980))
  expect_error(get_station_data("MAXIMOS", "PM10", -9:2015))
  expect_error(get_station_data("MAXIMOS", "PM10", 2016.999))
  expect_error(get_station_data("MAXIMOS", "PM10", -2016))

  df_min_2016 <- get_station_data("MINIMOS", "PM10", 2016, progress = NULL)
  df_max_2016 <- get_station_data("MAXIMOS", "PM10", 2016)
  df_min_2015 <- get_station_data("MINIMOS", "PM10", 2015)
  df_max_2015 <- get_station_data("MAXIMOS", "O3", 2015)
  df_max_2005 <- get_station_data("MAXIMOS", "SO2", 2005)
  df_wdr_2005 <- get_station_data("MAXIMOS", "WDR", 2005)
  df_horarios_2010 <- get_station_data("HORARIOS", "PM10", 2010)
  df_horarios_2016 <- get_station_data("HORARIOS", "O3", 2016)

  # Wait before downloading
  Sys.sleep(2)
  # No measuring stations for PM25 in 1986, should show message
  expect_message(get_station_data("HORARIOS", "PM25", 1986))
  Sys.sleep(2)
  expect_equal(dplyr::filter(get_station_data("HORARIOS", "RH", 2000),
                date == "2000-01-01" & hour == 3 &
                  station_code == "XAL")$value, 56)

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