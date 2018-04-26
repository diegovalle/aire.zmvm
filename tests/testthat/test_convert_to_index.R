test_that("convert_to_index", {
  expect_error(convert_to_index("ERROR"))
  expect_error(convert_to_index(133, "ERROR"),
               "Invalid pollutant value")
  expect_error(convert_to_index(c(133, 144, 144), c("O3", "PM10")),
               "longer argument not a multiple of length of shorter")
  expect_warning(convert_to_index(c(133,144, NA), "O3"),
                 "The vectors are of unequal length")

  expect_equal(convert_to_index(c(155, NA), c("O3", "O3")), c("MUY MALA",
                                                              NA_character_))
  expect_equal(convert_to_index(155, "O3"), "MUY MALA")
  expect_equal(convert_to_index(96, "O3"), "MALA")
  expect_equal(convert_to_index(95.4, "O3"), "REGULAR")
  expect_equal(convert_to_index(95.5, "O3"), "MALA")
  expect_equal(convert_to_index(c(12.1, 215, 355),
                                      c("PM25", "PM10", "PM10")),
               c("REGULAR", "MUY MALA", "EXTREMADAMENTE MALA"))
})
