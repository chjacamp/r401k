test_that("xirr matches a simple one-year return", {
  dates <- as.Date(c("2024-01-01", "2025-01-01"))
  expect_equal(xirr(dates, c(-100, 110)), 1.1^(365.25 / 366) - 1, tolerance = 1e-8)
})

test_that("xirr stays above -100% for short windows with losses", {
  dates <- as.Date(c("2024-01-01", "2024-01-03"))
  r <- xirr(dates, c(-100, 97))
  expect_true(is.finite(r) && r > -1)
  expect_equal((1 + r)^(2 / 365.25), 0.97, tolerance = 1e-8)
})

test_that("xirr handles a large short-window gain", {
  dates <- as.Date(c("2024-01-01", "2024-01-02"))
  r <- xirr(dates, c(-100, 110))
  expect_equal((1 + r)^(1 / 365.25), 1.1, tolerance = 1e-6)
})

test_that("xirr returns NA for flows that can't have a rate", {
  dates <- as.Date(c("2024-01-01", "2024-06-01"))
  expect_true(is.na(xirr(dates, c(100, 50))))
  expect_true(is.na(xirr(dates[1], -100)))
})

test_that("xirr with several deposits nets the flows to zero", {
  dates <- as.Date(c("2024-01-15", "2024-04-15", "2024-07-15", "2024-12-31"))
  flows <- c(-1000, -1000, -1000, 3300)
  r <- xirr(dates, flows)
  t <- as.numeric(dates - min(dates)) / 365.25
  expect_equal(sum(flows / (1 + r)^t), 0, tolerance = 1e-6)
})

test_that("fund return over the whole demo is the price ratio", {
  x <- load_401k(demo_data_dir())
  s <- window_stats(x, min(x$daily$date), max(x$daily$date))
  expect_equal(s$fund_return, tail(x$daily$nav, 1) / x$daily$nav[1] - 1)
  expect_true(is.finite(s$period_return))
})
