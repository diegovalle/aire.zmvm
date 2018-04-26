test_that("test get_station_month_data", {
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
  expect_error(get_station_month_data("MAXIMOS", c("PM10", "O3"), 2016, 1))
  # There's an error in the 2016 WSP data
  expect_error(get_station_month_data("MAXIMOS", "WSP", 2016, 1))
  expect_error(get_station_month_data("HORARIOS", "WSP", 2016, 1))
  expect_error(get_station_month_data("MINIMOS", "WSP", 2016, 1))
  # Error in the ACO station
  expect_error(get_station_month_data("MINIMOS", "WSP", 2012, 1))
  expect_error(get_station_month_data("MAXIMOS", "WSP", 2012, 1))
  for (year in 2005:2017)
    expect_error(get_station_month_data("MAXIMOS", "WSP", year, 1))

  skip_on_cran()

  df_hor_2018_jan <- get_station_month_data("HORARIOS",
                                            "WSP", 2018, 1)
  expect_warning(df_tmp_2017_jan <- get_station_month_data("HORARIOS",
                                                         "TMP", 2005, 1),
                 "Temperature \\(TMP\\) was rounded to the nearest integer")
  df_min_2016_april <- get_station_month_data("MINIMOS", "PM10", 2016, 4)
  df_max_2016_march <- get_station_month_data("MAXIMOS", "O3", 2016, 3)

  # test that the data only include one month
  expect_true(all(month(df_hor_2018_jan$date) == 1))
  expect_true(all(month(df_min_2016_april$date) == 4))
  expect_true(all(month(df_max_2016_march$date) == 3))

  expect_equal(
    unname(unlist(subset(df_hor_2018_jan, date == "2018-01-01" &
                           station_code == "CHO")$value)),
    c(0.5, 0.4, 0.5, 0.4, 0.3, 0.7, 0.5, 0.9, 0.5, 0.6, 1.4, 1.6,
      1.7, 1.6, 1.6, 1.3, 1.9, 1.7, 2.3, 1, 2.7, 2.4, 1.9, 1.1))
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
  expect_warning(get_station_single_month("RH", 2005, 1),
                 "'get_station_single_month' is deprecated.")
})
