
test_that(("convert units"), {
  # Bad arguments
  expect_error(convert_to_imeca(10, NA, show_warnings = FALSE))
  expect_error(convert_to_imeca(10, "NO3", show_warnings = FALSE))
  expect_error(convert_to_imeca(10, NA, show_warnings = FALSE))
  expect_error(convert_to_imeca(10, NA, show_warnings = FALSE))
  expect_error(convert_to_imeca(10, NA, show_warnings = FALSE))
  expect_error(convert_to_imeca(10, NA, show_warnings = FALSE))


  expect_equal(convert_to_imeca(c(10,NA,10), c("O3"), show_warnings = FALSE),
               c(7,NA,7))
  expect_equal(convert_to_imeca(c(10.1, NA, 10.3), "PM10",
                                show_warnings = FALSE),
               c(13, NA, 13))
  expect_equal(convert_to_imeca(c(10.1, NA, 10.3), "PM10", show_warnings = FALSE),
               c(13, NA, 13))
  expect_error(convert_to_imeca(structure(1L, .Label = "a", class = "factor"),
                                "PM10", show_warnings = FALSE))

  # Argument showWarnings was deprecated and should show a warning
  expect_warning(convert_to_imeca(10, "NO2", showWarnings = FALSE))
  expect_warning(convert_to_imeca(10, c("NO2", "O3"), show_warnings = TRUE))

  expect_equal(convert_to_imeca(-1, "NO2", show_warnings = FALSE), NA)
  expect_equal(convert_to_imeca(NA, "NO2", show_warnings = FALSE), NA)
  expect_equal(convert_to_imeca(c(450, 350, 250), "NO2", show_warnings = FALSE),
               c(215,167,119))
  expect_equal(convert_to_imeca(c(450, 350, 48), c("NO2", "NO2", "O3"),
                                show_warnings = FALSE), c(215,167,34))

  expect_equal(convert_to_imeca(90, "NO2", show_warnings = FALSE), 43)
  expect_equal(convert_to_imeca(75, "NO2", show_warnings = FALSE), 36)
  #expect_equal(convert_to_imeca(150, "NO2", show_warnings = FALSE), 71)
  expect_equal(convert_to_imeca(250, "NO2", show_warnings = FALSE), 119)
  expect_equal(convert_to_imeca(350, "NO2", show_warnings = FALSE), 167)
  expect_equal(convert_to_imeca(450, "NO2", show_warnings = FALSE), 215)

  expect_equal(convert_to_imeca(48, "O3", show_warnings = FALSE), 34)
  expect_equal(convert_to_imeca(67, "O3", show_warnings = FALSE), 48)
  expect_equal(convert_to_imeca(77, "O3", show_warnings = FALSE), 63)
  expect_equal(convert_to_imeca(205, "O3", show_warnings = FALSE), 201)
  expect_equal(convert_to_imeca(72, "O3", show_warnings = FALSE), 53)
  expect_equal(convert_to_imeca(98, "O3", show_warnings = FALSE), 103)
  expect_equal(convert_to_imeca(170, "O3", show_warnings = FALSE), 166)

  expect_equal(convert_to_imeca(1.5, "CO", show_warnings = FALSE), 14)
  expect_equal(convert_to_imeca(6, "CO", show_warnings = FALSE), 55)
  # expect_equal(convert_to_imeca(12, "CO", show_warnings = FALSE), 109)
  expect_equal(convert_to_imeca(18, "CO", show_warnings = FALSE), 164)
  # expect_equal(convert_to_imeca(24, "CO", show_warnings = FALSE), 218)

  expect_equal(convert_to_imeca(80, "PM10", show_warnings = FALSE), 102)
  expect_equal(convert_to_imeca(30, "PM10", show_warnings = FALSE), 38)
  #expect_equal(convert_to_imeca(74, "PM10", show_warnings = FALSE), 99)
  expect_equal(convert_to_imeca(215, "PM10", show_warnings = FALSE), 151)
  expect_equal(convert_to_imeca(300, "PM10", show_warnings = FALSE), 181)


  expect_equal(convert_to_imeca(6, "SO2", show_warnings = FALSE), 5)

  expect_warning(convert_to_imeca(80, "PM10"))
  expect_silent(convert_to_imeca(80, "PM10", show_warnings = FALSE))
})
