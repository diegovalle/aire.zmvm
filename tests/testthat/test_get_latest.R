

test_that("latest data", {
  skip_on_cran()

  df <- get_latest_imeca()
  expect_gt(nrow(df), 0)
  expect_type(df$value, "integer")
  expect_type(df$datetime, "character")
  expect_false(all(is.na(df$datetime)))

  expect_warning(get_latest_data())
})
