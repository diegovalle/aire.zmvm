context("Get IMECA values by zone")

test_that("zone pollution data matches website", {
  # Invalid function arguments
  expect_error(get_zone_imeca("INVALIDCRITERION", "O3",
                              "NO", "2015-12-25", "2016-01-01"))
  expect_error(get_zone_imeca("MAXIMOS", "INVALIDO3",
                              "NO", "2015-12-25", "2016-01-01"))
  expect_error(get_zone_imeca("MAXIMOS", "O3",
                              "INVALIDNO", "2015-12-25", "2016-01-01"))
  expect_error(get_zone_imeca("MAXIMOS", "O3",
                              "NO", "2015-13-25", "2016-01-01"))
  expect_error(get_zone_imeca("MAXIMOS", "O3",
                              "NO", "2015-12-25", "2016-14-01"))

  skip_on_cran()

  Sys.sleep(1)
  df_max_o3 <- suppressMessages(get_zone_imeca("MAXIMOS",
                                               "O3", c("NO", "NE", "CE"),
                                               "2015-12-25", "2016-01-01"))

  Sys.sleep(1)
  df_max_tz <- suppressMessages(get_zone_imeca("MAXIMOS",
                                               c("O3", "PM10"), c("TZ"),
                                               "2015-12-31", "2016-01-06"))

  Sys.sleep(1)
  df_horarios <- suppressMessages(get_zone_imeca("HORARIOS", c("O3", "PM10"),
                                                 c("NO", "NE", "CE"),
                                                 "2015-12-25", "2016-01-01"))


  Sys.sleep(1)
  expect_message(get_zone_imeca("MAXIMOS", "PM10", c("NO", "NE", "CE"),
                                "2008-01-01", "2008-01-01"))

  Sys.sleep(1)
  expect_message(get_zone_imeca("MAXIMOS", "SO2", c("NO", "NE", "CE"),
                                "2017-02-25", "2017-05-01"))
  expect_silent(get_zone_imeca("MAXIMOS", "O3", c("NO", "NE", "CE"),
                               "2015-12-25", "2016-01-01",
                               show_messages = FALSE))
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
               c(124, 132, 69, 57, 29, 44, 33))
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
  # detect date errors
  expect_error(get_zone_imeca("MAXIMOS", "O3", "TZ",
                              "2008-01-32", "2007-13-44"))
  # test that deprecated function shows warning
  expect_warning(get_zone_data("MAXIMOS", "O3", "NO",
                               "2015-12-31", "2015-12-31"))
})
