
test_that( ("convert units"), {
  # Bad arguments
  expect_error(convert_to_imeca(10, NA))
  expect_error(convert_to_imeca(10, "NO3"))
  expect_error(convert_to_imeca(10, NA))
  expect_error(convert_to_imeca(10, NA))
  expect_error(convert_to_imeca(10, NA))
  expect_error(convert_to_imeca(10, NA))


  expect_equal(suppressWarnings({
    convert_to_imeca(c(10, NA, 10), c("O3"))
  }),
  c(7, NA, 7)
  )
  expect_equal(suppressWarnings({
    convert_to_imeca(c(10.1, NA, 10.3), "PM10")
  }),
  c(13, NA, 13)
  )
  expect_equal(suppressWarnings({
    convert_to_imeca(c(10.1, NA, 10.3), "PM10")
  }),
  c(13, NA, 13))
  expect_error(convert_to_imeca(structure(1L, .Label = "a", class = "factor"),
                                "PM10"))

  # Argument showWarnings was deprecated and should show a warning
  expect_warning(convert_to_imeca(10, "NO2"), "This function is beta.")
  expect_warning(convert_to_imeca(10, c("NO2", "O3")),
                 "The vectors are of unequal length")

  expect_equal(suppressWarnings({
    convert_to_imeca(-1, "NO2")
    })
    , NA)
  expect_equal(suppressWarnings({
    convert_to_imeca(NA, "NO2")
    })
    , NA)
  expect_equal(suppressWarnings({
    convert_to_imeca(c(450, 350, 250), "NO2")
  }),
  c(215, 167, 119))
  expect_equal(suppressWarnings({
    convert_to_imeca(c(450, 350, 48), c("NO2", "NO2", "O3"))
  }),
  c(215, 167, 34))

  suppressWarnings({


  expect_equal(convert_to_imeca(90, "NO2"), 43)
  expect_equal(convert_to_imeca(75, "NO2"), 36)
  # expect_equal(convert_to_imeca(150, "NO2"), 71)
  expect_equal(convert_to_imeca(250, "NO2"), 119)
  expect_equal(convert_to_imeca(350, "NO2"), 167)
  expect_equal(convert_to_imeca(450, "NO2"), 215)

  expect_equal(convert_to_imeca(48, "O3"), 34)
  expect_equal(convert_to_imeca(67, "O3"), 48)
  expect_equal(convert_to_imeca(77, "O3"), 63)
  expect_equal(convert_to_imeca(205, "O3"), 201)
  expect_equal(convert_to_imeca(72, "O3"), 53)
  expect_equal(convert_to_imeca(98, "O3"), 103)
  expect_equal(convert_to_imeca(170, "O3"), 166)

  expect_equal(convert_to_imeca(1.5, "CO"), 14)
  expect_equal(convert_to_imeca(6, "CO"), 55)
  # expect_equal(convert_to_imeca(12, "CO"), 109)
  expect_equal(convert_to_imeca(18, "CO"), 164)
  # expect_equal(convert_to_imeca(24, "CO"), 218)

  expect_equal(convert_to_imeca(80, "PM10"), 102)
  expect_equal(convert_to_imeca(30, "PM10"), 38)
  # expect_equal(convert_to_imeca(74, "PM10"), 99)
  expect_equal(convert_to_imeca(215, "PM10"), 151)
  expect_equal(convert_to_imeca(300, "PM10"), 181)


  expect_equal(convert_to_imeca(6, "SO2"), 5)

  expect_warning(convert_to_imeca(80, "PM10"))
  })
})
