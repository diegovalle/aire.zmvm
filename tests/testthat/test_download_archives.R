context("Download archives")

test_that("download_pollution", {
  expect_error(download_pollution(1985))
  expect_error(download_pollution(numeric(0)))
  expect_error(download_pollution("a"))
  expect_error(download_pollution(1985.6))
  expect_error(download_pollution(c(1990, NA)))

  skip_on_cran()
  ## FIXME: datosabiertos.aire.cdmx.gob.mx timing out
  skip()

  df <- download_pollution(1986)
  expect_equal(df$station_code[1:5], c("LAG", "TLA", "XAL", "MER", "PED"))
  expect_equal(df$value[1:5], c(rep(NA_real_, 5)))
  expect_equal(subset(df, date == "1986-01-25" & station_code == "BJU" &
                        hour == 14 & pollutant == "CO")$value, 1.7)
})

test_that("download_meteorological ", {

  expect_error(download_meteorological(1985))
  expect_error(download_meteorological(numeric(0)))
  expect_error(download_meteorological("a"))
  expect_error(download_meteorological(1985.6))
  expect_error(download_meteorological(c(1990, NA)))

  skip_on_cran()
  ## FIXME: datosabiertos.aire.cdmx.gob.mx timing out
  skip()

  df <- download_meteorological(1986)
  expect_equal(df$station_code[1:5], c("TAC", "FAC", "SAG", "TLA", "XAL"))
  expect_equal(df$value[1:5], c(rep(NA_real_, 5)))
  expect_equal(subset(df, date == "1986-08-23" & station_code == "TAC" &
                        hour == 15 & pollutant == "RH")$value, 52)

  df <- download_meteorological(2017)
  expect_equal(df$station_code[1:5], c("ACO", "ACO", "ACO", "ACO", "AJU"))
  expect_equal(df$value[1:5], c(70, 12.5, 11.0, 0.9, 90))
  expect_equal(subset(df, date == "2017-01-01" & station_code == "ACO" &
                        hour == 1 & pollutant == "WDR")$value, 11)

  df <- download_meteorological(2016)
  expect_equal(df$station_code[1:5], c("ACO", "ACO", "ACO", "ACO", "AJU"))
  expect_equal(df$value[1:5], c(73, 14.1, 35.0, 2.0, 88.0))
  expect_equal(subset(df, date == "2016-01-01" & station_code == "MON" &
                        hour == 1 & pollutant == "WSP")$value, 1)
})

test_that( ("download_lead "), {
  expect_error(download_lead(1985))
  expect_error(download_lead("PbPST", 1987))

  skip_on_cran()
  ## FIXME: datosabiertos.aire.cdmx.gob.mx timing out
  skip()

  df <- download_lead("PbPST")
  expect_equal(df$station_code[1:5], c("CES", "MER", "PED", "TLA", "XAL"))
  expect_equal(subset(df, date == "1989-01-08" & station_code == "XAL" &
                        pollutant == "PbPST")$value, 3.17)

  df <- download_lead("PST, PM10, PM25")
  expect_equal(df$station_code[1:5], c("CES", "MER", "PED", "TLA", "XAL"))
  expect_equal(subset(df, date == "1989-01-02" & station_code == "XAL" &
                        pollutant == "PM10")$value, 249)
})

test_that( ("download_deposition "), {
  expect_error(download_deposition())
  expect_error(download_deposition("ERROR", "CONCENTRACION"))

  skip_on_cran()
  ## FIXME: datosabiertos.aire.cdmx.gob.mx timing out
  skip()

  df <- download_deposition(deposition = "HUMEDO", type = "CONCENTRACION")
  expect_equal(df$station_code[1:5], c("TEC", "MCM", "TLA", "XAL", "LOM"))
  expect_equal(subset(df, date == "1997-05-05" & station_code == "XAL" &
                        pollutant == "H")$value, 0.0003)
  df2 <- download_deposition(deposition = "HUMEDO", type = "DEPOSITO")
  expect_equal(subset(df2, date == "2002-11-11" & station_code == "LAA" &
                        pollutant == "NH4")$value, 8.28)

  ## For some reason the files come with a tab at the beginning of each line
  ## and readr throws a warning
  suppressWarnings(df3 <- download_deposition(deposition = "TOTAL",
                                             type = "CONCENTRACION"))
  expect_equal(subset(df3, date == "2000-10-09" & station_code == "LAA" &
                        pollutant == "PP")$value, 54.62)
  suppressWarnings(df4 <- download_deposition(deposition = "TOTAL",
                                             type = "DEPOSITO"))
  expect_equal(subset(df4, date == "2000-10-09" & station_code == "LAA" &
                        pollutant == "Mg")$value, 10.54)
})

test_that( ("download_radiation "), {
  expect_error(download_radiation("UVA", 1999))
  expect_error(download_radiation("UVBERROR", 2005))

  skip_on_cran()
  ## FIXME: datosabiertos.aire.cdmx.gob.mx timing out
  skip()

  df <- download_radiation("UVA", 2000)
  expect_equal(df$station_code[1:5], c("HAN", "MER", "MON", "PED", "SAG"))
  expect_equal(subset(df, date == "2000-01-01" & station_code == "TLA" &
                        pollutant == "UVA" & hour == 13)$value, 3.293)

  df <- download_radiation("UVB", 2000)
  expect_equal(df$station_code[1:5], c("HAN", "MER", "MON", "PED", "SAG"))
  expect_equal(subset(df, date == "2000-01-01" & station_code == "PED" &
                        pollutant == "UVB" & hour == 14)$value, 2.306)
})

test_that( ("download_24hr_average "), {
  expect_error(download_24hr_average("SO2", 1985))
  expect_error(download_24hr_average("PS", 1994))

  skip_on_cran()
  ## FIXME: datosabiertos.aire.cdmx.gob.mx timing out
  skip()

  df <- download_24hr_average("PS", 1995)
  expect_equal(df$station_code[1:5], c("LVI", "TLA", "XAL", "MER", "PED"))
  expect_equal(subset(df, date == "1995-01-02" & station_code == "LVI" &
                        pollutant == "PM10")$value, 141)
  df <- download_24hr_average("PS", 2012)
  expect_equal(subset(df, date == "2012-01-01" & station_code == "ACO" &
                        pollutant == "PM10")$value, 63)

  df <- download_24hr_average("SO2", 2012)
  expect_equal(subset(df, date == "2012-01-01" & station_code == "ACO" &
                        pollutant == "SO2")$value, 4)
  df <- download_24hr_average("SO2", 1986)
  expect_equal(subset(df, date == "1986-01-09" & station_code == "CES" &
                        pollutant == "SO2")$value, 45)

})

test_that( ("download_pressure "), {
  expect_error(download_pressure(2008))
  expect_error(download_pressure("PS"))

  skip_on_cran()
  ## FIXME: datosabiertos.aire.cdmx.gob.mx timing out
  skip()

  df <- download_pressure(2009)
  expect_equal(df$station_code[1:5], c("MER", "MER", "MER", "MER", "MER"))
  expect_equal(subset(df, date == "2009-01-01" & station_code == "MER" &
                        pollutant == "PA" & hour == 1)$value, 588)
})
