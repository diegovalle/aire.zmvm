test_that( ("recode_pollutant "), {
  expect_equal(.recode_pollutant("pm2"), "PM25")
  expect_equal(.recode_pollutant("so2"), "SO2")
})

test_that( (".recode_unit_code "), {
  expect_equal(.recode_unit_code(9), "mmHg")
  expect_equal(.recode_unit_code(12), "\u00b5S/cm")
})

test_that( ("get_rama"), {
  expect_error(get_rama(1985))
  expect_error(get_rama(numeric(0)))
  expect_error(get_rama("a"))
  expect_error(get_rama(1985.6))
  expect_error(get_rama(c(1990,NA)))
})

test_that( ("get_redmet "), {

  expect_error(get_redmet(1985))
  expect_error(get_redmet(numeric(0)))
  expect_error(get_redmet("a"))
  expect_error(get_redmet(1985.6))
  expect_error(get_redmet(c(1990,NA)))
})

test_that( ("get_redma "), {
  expect_error(get_redma(1985))
  expect_error(get_redma("ERROR"))
})

test_that( (".recode_unit_code "), {
  expect_error(get_redda())
  expect_error(get_redda("ERROR", "CONCENTRACION"))
})

test_that( (".recode_unit_code "), {
  expect_error(get_radiacion("UVA", 1999))
  expect_error(get_radiacion("UVBERROR", 2005))

})

test_that( (".recode_unit_code "), {
  expect_error(get_24hr_average("SO2", 1985))
  expect_error(get_24hr_average("PS", 1994))

})

test_that( (".recode_unit_code "), {
  expect_error(get_pa(1985))
  expect_error(get_pa("PS"))

})
