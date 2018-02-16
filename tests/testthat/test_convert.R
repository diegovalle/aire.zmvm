
test_that(("convert units"), {
  expect_equal(convert_to_imeca(-1, "NO2", showWarnings = FALSE), NA)
  expect_equal(convert_to_imeca(NA, "NO2", showWarnings = FALSE), NA)
  expect_equal(convert_to_imeca(c(450, 350, 250), "NO2", showWarnings = FALSE), c(215,167,119))
  expect_equal(convert_to_imeca(c(450, 350, 48), c("NO2", "NO2", "O3"), showWarnings = FALSE), c(215,167,34))

  expect_equal(convert_to_imeca(90, "NO2", showWarnings = FALSE), 43)
  expect_equal(convert_to_imeca(75, "NO2", showWarnings = FALSE), 36)
  #expect_equal(convert_to_imeca(150, "NO2", showWarnings = FALSE), 71)
  expect_equal(convert_to_imeca(250, "NO2", showWarnings = FALSE), 119)
  expect_equal(convert_to_imeca(350, "NO2", showWarnings = FALSE), 167)
  expect_equal(convert_to_imeca(450, "NO2", showWarnings = FALSE), 215)

  expect_equal(convert_to_imeca(48, "O3", showWarnings = FALSE), 34)
  expect_equal(convert_to_imeca(67, "O3", showWarnings = FALSE), 48)
  expect_equal(convert_to_imeca(77, "O3", showWarnings = FALSE), 63)
  expect_equal(convert_to_imeca(205, "O3", showWarnings = FALSE), 201)
  expect_equal(convert_to_imeca(72, "O3", showWarnings = FALSE), 53)
  expect_equal(convert_to_imeca(98, "O3", showWarnings = FALSE), 103)
  expect_equal(convert_to_imeca(170, "O3", showWarnings = FALSE), 166)

  expect_equal(convert_to_imeca(1.5, "CO", showWarnings = FALSE), 14)
  expect_equal(convert_to_imeca(6, "CO", showWarnings = FALSE), 55)
  # expect_equal(convert_to_imeca(12, "CO", showWarnings = FALSE), 109)
  expect_equal(convert_to_imeca(18, "CO", showWarnings = FALSE), 164)
  # expect_equal(convert_to_imeca(24, "CO", showWarnings = FALSE), 218)

  expect_equal(convert_to_imeca(80, "PM10", showWarnings = FALSE), 102)
  expect_equal(convert_to_imeca(30, "PM10", showWarnings = FALSE), 38)
  #expect_equal(convert_to_imeca(74, "PM10", showWarnings = FALSE), 99)
  expect_equal(convert_to_imeca(215, "PM10", showWarnings = FALSE), 151)
  expect_equal(convert_to_imeca(300, "PM10", showWarnings = FALSE), 181)


  expect_equal(convert_to_imeca(6, "SO2", showWarnings = FALSE), 5)

  expect_warning(convert_to_imeca(80, "PM10"))
  expect_silent(convert_to_imeca(80, "PM10", showWarnings = FALSE))
})
