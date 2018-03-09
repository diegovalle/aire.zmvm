test_that("is.Date works", {
  expect_false(is.Date("test"))
  expect_true(is.Date("2018-01-21"))
  expect_true(is.Date("2005-01-01"))
  expect_true(is.Date("2009-01-01"))
  expect_false(is.Date("2009-13-01"))
  expect_false(is.Date("20011-13-01"))
  expect_false(is.Date("2009-11-34"))
  expect_false(is.Date("2009-02-30"))
  expect_false(is.Date(character(0)))
  expect_false(is.Date(NULL))
  expect_false(is.Date(NA))
})


test_that("is.integer2 works", {
  expect_true(is.integer2(1986))
  expect_true(is.integer2(1986.0))
  expect_false(is.integer2(99.2))
  expect_false(is.integer2(99.0000000001))
  expect_false(is.integer2(character(0)))
  expect_false(is.integer2(NULL))
  expect_false(is.integer2(1.0000000000001))
  expect_true(is.integer2(2018))
  expect_true(is.integer2(c(1999:2010)[3]))
  expect_true(is.integer2(as.numeric(11)))
  expect_true(is.integer2(as.double(11)))
  expect_true(is.integer2(as.integer(22)))
  expect_true(is.integer2(as.single(12)))
  expect_false(is.integer2(NA))
  # Not an integer
  expect_false(is.integer2(NA_real_))
})

test_that("round_away_from_zero", {
  expect_equal(round_away_from_zero(.5), 1)
  expect_equal(round_away_from_zero(1.5), 2)
  expect_equal(round_away_from_zero(2.5), 3)
  expect_equal(round_away_from_zero(3.5), 4)
  expect_equal(round_away_from_zero(-3.5), -4)

  expect_equal(round_away_from_zero(2.4), 2)
  expect_equal(round_away_from_zero(3.6), 4)
})
