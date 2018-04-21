test_that("round_away_from_zero", {
  expect_error(concentration_to_index("a"))
  expect_error(concentration_to_index(133, "a"),
               "Invalid pollutant value")
  expect_error(concentration_to_index(c(133, 144, 144), c("O3", "PM10")),
               "longer argument not a multiple of length of shorter")


  expect_equal(concentration_to_index(155, "O3"), "MUY MALA")
  expect_equal(concentration_to_index(96, "O3"), "MALA")
  expect_equal(concentration_to_index(95.4, "O3"), "REGULAR")
  expect_equal(concentration_to_index(95.5, "O3"), "MALA")
  expect_equal(concentration_to_index(c(12.1, 215, 355),
                                      c("PM25", "PM10", "PM10")),
               c("REGULAR", "MUY MALA", "EXTREMADAMENTE MALA"))
})
