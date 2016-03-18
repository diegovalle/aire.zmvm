test_that("data downloaded matches api", {
  df_min <- get_pollution_data("MINIMOS", "pm10", 2010)
  df_max <- get_pollution_data("MAXIMOS", "pm10", 2010)
  df_horarios <- get_pollution_data("HORARIOS", "pm10", 2010)

  expect_equal(unname(unlist(df_min[1,])),
               c(1262304000, NA, NA, NA, NA, 11, NA, NA, NA, NA, NA,
                 14, NA, 9, NA, NA,
                 12, 13, NA, 8, NA, 15, NA, NA, 6, 10, 11,
                 NA, 9, 7, NA, NA, NA, 7, 19, NA, 19, 6, 11, 11))
  expect_equal(unname(unlist(df_max[1,])),
               c(1262304000, NA, NA, NA, NA, 102, NA, NA, NA, NA,
                 NA, 87, NA,
                 160, NA, NA, 122, 122, NA, 83, NA, 218, NA, NA,
                 273, 115, 100,
                 NA, 167, 283, NA, NA, NA, 275, 248, NA, 283, 83,
                 170.0625, 160))
  expect_equal(unname(unlist(df_horarios[1,])),
               c(1262304000, 1, NA, NA, NA, NA, 62, NA, NA, NA, NA, NA, 81,
                 NA, 107, NA, NA, 115, 104, NA, 83, NA, 129, NA, NA, 225, 73,
                 71, NA, 98, 182, NA, NA, NA, 275, 195, NA, 275, 1, 122.176470588235,
                 105.5))
})
