
test_that("is.Date", {

  expect_false(is.Date("test"))
  expect_true(is.Date("2018-01-21"))
  expect_true(is.Date("2005-01-01"))
})

test_that("get_station_imeca pollution data matches the website", {
  skip_on_cran()

  df <- get_station_imeca("O3", "2017-05-15")
  expect_equal(max(df$value, na.rm = TRUE), 151)

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
})
