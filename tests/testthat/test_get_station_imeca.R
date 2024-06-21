context("Get station data")

test_that("get_station_imeca pollution data matches the website", {
  # Invalid function arguments
  expect_error(get_station_imeca("INVALID", "2009-01-01"))
  expect_error(get_station_imeca("SO2", "INVALID"))
  expect_error(get_station_imeca("O3", "2009-02-31"))
  expect_error(get_station_imeca(c("O3", "SO2"), "2009-02-31"))

  skip_on_cran()

  Sys.sleep(1)
  df <- get_station_imeca("O3", "2017-05-15")
  expect_equal(max(df$value, na.rm = TRUE), 151)

  Sys.sleep(1)
  df <- get_station_imeca("O3", "2018-01-01")
  expect_equal(df$value[which(df$station_code == "AJM")],
               c(22, 20, 21, 24, 24, 22, 12, 8, 13,
                 20, 29, 36, 40, 46, 55,
                 69, 63, 49, 34, 16, 12, 11, 11, 9))

  df <- get_station_imeca("O3", "2009-01-01")
  expect_equal(df$value[which(df$station_code == "XAL")],
               c(8, 3, 2, 3, 4, 5, 4, 5, 6, 17, 36,
                 58, 77, 60, 42, 43, 38,
                 31, 15, 6, 4, 4, 2, 2))
  df <- get_station_imeca("PM10", "2020-01-02")
  expect_equal(df$value[which(df$station_code == "XAL")],
               c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                 NA, NA, NA, 76, 76, 74, 73, 70))
})
