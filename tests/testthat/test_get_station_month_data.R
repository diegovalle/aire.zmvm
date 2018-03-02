test_that(("test get_station_month_data"), {
  skip_on_cran()

  # Invalid function arguments
  expect_error(get_station_month_data("INVALID", "PM10", 2016, 1))
  expect_error(get_station_month_data("MAXIMOS", "INVALID", 2016, 2))
  expect_error(get_station_month_data("MAXIMOS", "PM10", 2016.6, 1))
  expect_error(get_station_month_data("MAXIMOS", "PM10", 2004, 1))
  expect_error(get_station_month_data("MAXIMOS", "PM10", -9:2015, 1))
  expect_error(get_station_month_data("MAXIMOS", "PM10", 2016.999, 12))
  expect_error(get_station_month_data("MAXIMOS", "PM10", -2016, 1))
  expect_error(get_station_month_data("MAXIMOS", "PM10", -2016, 13))
  expect_error(get_station_month_data("MAXIMOS", "PM10", -2016, 1.5))

  df_hor_2005_jan <- get_station_month_data("HORARIOS", "RH", 2005, 1)
  df_min_2016_april <- get_station_month_data("MINIMOS", "PM10", 2016, 4)
  df_max_2016_march <- get_station_month_data("MAXIMOS", "O3", 2016, 3)

  # test that the data only include one month
  expect_true(all(month(df_hor_2005_jan$date) == 1))
  expect_true(all(month(df_min_2016_april$date) == 4))
  expect_true(all(month(df_max_2016_march$date) == 3))

  expect_equal(
    unname(unlist(subset(df_hor_2005_jan, date == as.Date("2005-01-03") &
                           station_code == "MON")$value)),
    c(56, 60, 64, 68, 67, 68, 70, 72, 66, 59, 53, 45, 34, 24, NA,
      21, 30, 34, 38, 40, 40, 42, 41))
  expect_equal(
    unname(unlist(subset(df_min_2016_april,
                         date == as.Date("2016-04-15"))$value)),
    c(NA, 16, 0, 7, 23, 27, 0, NA, 0, 17, 0, 1, 41, NA, 0, 25, NA,
      24, 0, 0, NA, 20, 11, 16, 0, NA, NA, 33, NA, 0, 0, 28, NA, 0,
      36, 25, 0, 0, 27, NA, 37))
  expect_equal(
    unname(unlist(subset(df_max_2016_march,
                         date == as.Date("2016-03-23"))$value)),
    c(60, NA, 50, 36, NA, 59, 59, 61, NA, 34, 51, 66, 43, 43, 55,
      75, 57, NA, 61, NA, NA, NA, 57, 53, 51, 59, NA, NA, 73, NA, NA,
      0, NA, 50, NA, 0, 46, 54, 0, 63, 54, 64, 48))

  # Expect warning from deprecated function
  expect_warning(get_station_single_month("RH", 2005, 1))
})
