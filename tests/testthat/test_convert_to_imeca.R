
test_that( ("convert units"), {
  # Bad arguments
  expect_error(convert_to_imeca(10, NA))
  expect_error(convert_to_imeca(10, "NO3"))
  expect_error(convert_to_imeca("10", "O3"))
  expect_error(convert_to_imeca(10:12, c("O3", "PM10", "INVALID")))
  expect_error(convert_to_imeca(10:15, rep("O3", 13)))


  expect_warning(
    convert_to_imeca(c(10, NA, 10), c("O3")),
  "The vectors are of unequal length."
  )
  expect_equal(
    convert_to_imeca(c(10.1, NA, 10.3), rep("PM10", 3)),
  c(13, NA, 13)
  )
  expect_equal(
    convert_to_imeca(c(10.1, NA, 10.3), rep("PM10", 3)),
  c(13, NA, 13))
  expect_error(convert_to_imeca(structure(1L, .Label = "a", class = "factor"),
                                "PM10"))

  # Argument showWarnings was deprecated and should show a warning
  expect_warning(convert_to_imeca(10, "NO2", showWarnings = TRUE))
  expect_warning(convert_to_imeca(10, c("NO2", "O3")),
                 "The vectors are of unequal length")

  expect_equal(
    suppressWarnings(convert_to_imeca(-1, "NO2"))
    , NA)
  expect_equal(
    convert_to_imeca(NA, "NO2")
    , NA)
  expect_equal(
    convert_to_imeca(c(450, 350, 250), rep("NO2", 3)),
    c(155, 132, 110))
  expect_equal(
    convert_to_imeca(c(450, 350, 48), c("NO2", "NO2", "O3")),
    c(155, 132, 34))

  expect_equal(convert_to_imeca(90, "NO2"), 43)
  expect_equal(convert_to_imeca(75, "NO2"), 36)
  expect_equal(convert_to_imeca(150, "NO2"), 72)
  expect_equal(convert_to_imeca(250, "NO2"), 110)
  expect_equal(convert_to_imeca(350, "NO2"), 132)
  expect_equal(convert_to_imeca(450, "NO2"), 155)

  expect_equal(convert_to_imeca(48, "O3"), 34)
  expect_equal(convert_to_imeca(67, "O3"), 48)
  expect_equal(convert_to_imeca(77, "O3"), 63)
  expect_equal(convert_to_imeca(205, "O3"), 201)
  expect_equal(convert_to_imeca(72, "O3"), 53)
  expect_equal(convert_to_imeca(98, "O3"), 103)
  expect_equal(convert_to_imeca(170, "O3"), 166)
  expect_equal(convert_to_imeca(450, "O3"), 346)
  expect_equal(convert_to_imeca(550, "O3"), 446)
  expect_equal(convert_to_imeca(1050, "O3"), NA_real_)

  expect_equal(convert_to_imeca(1.5, "CO"), 14)
  expect_equal(convert_to_imeca(6, "CO"), 55)
  expect_equal(convert_to_imeca(12, "CO"), 124)
  expect_equal(convert_to_imeca(18, "CO"), 218)
  expect_equal(convert_to_imeca(24, "CO"), 257)

  expect_equal(convert_to_imeca(80, "PM10"), 102)
  expect_equal(convert_to_imeca(30, "PM10"), 38)
  expect_equal(convert_to_imeca(74, "PM10"), 99)
  expect_equal(convert_to_imeca(215, "PM10"), 151)
  expect_equal(convert_to_imeca(300, "PM10"), 181)
  expect_equal(convert_to_imeca(400, "PM10"), 266)
  expect_equal(convert_to_imeca(500, "PM10"), 395)
  expect_equal(convert_to_imeca(600, "PM10"), 496)
  expect_equal(convert_to_imeca(1050, "PM10"), NA_real_)

  expect_error(convert_to_imeca())
  expect_error(convert_to_imeca(123))
  expect_error(convert_to_imeca("ERROR", "O3"))
  expect_silent(convert_to_imeca(1, "O3"))

  expect_equal(convert_to_imeca(5, "PM25"), 21)
  expect_equal(convert_to_imeca(30, "PM25"), 78)
  expect_equal(convert_to_imeca(60, "PM25"), 115)
  expect_equal(convert_to_imeca(130, "PM25"), 181)
  expect_equal(convert_to_imeca(200, "PM25"), 250)
  expect_equal(convert_to_imeca(300, "PM25"), 350)
  expect_equal(convert_to_imeca(400, "PM25"), 434)
  expect_equal(convert_to_imeca(500.405, "PM25"), NA_real_)


  expect_equal(convert_to_imeca(6, "SO2"), 12)

})

