context("Get latest IMECA values")

test_that("latest data", {
  skip_on_cran()

  df <- get_latest_imeca()
  Sys.sleep(1)
  expect_gt(nrow(df), 0)
  expect_type(df$value, "integer")
  expect_type(df$datetime, "character")
  expect_false(all(is.na(df$datetime)))

  expect_warning(get_latest_data())
})


test_that("All stations in the `stations`` data.frame are up-to-date", {
  skip_on_cran()
  df <- get_latest_imeca()
  expect_equal(sort(intersect(df$station_code, stations$station_code)),
               sort(df$station_code))
})
