test_that("each day's balance change is deposits plus market plus fees, on the demo", {
  d <- load_401k(demo_data_dir())$daily
  flows <- d$employee + d$employer + d$rollover + d$withdrawals + d$fees + d$market
  expect_equal(d$balance[1], flows[1], tolerance = 1e-8)
  expect_equal(diff(d$balance), flows[-1], tolerance = 1e-8)
})

test_that("the hand-worked fixture rebuilds exactly", {
  x <- load_401k(write_fixture(fixture_tx(), fixture_nav()))
  last <- tail(x$daily, 1)
  expect_equal(last$units, 19)
  expect_equal(last$cash, 131)
  expect_equal(last$balance, 321)

  # Market gain: +15, +15, -75 on the losing day, then +24 plus the $1 dividend.
  expect_equal(x$daily$market, c(0, 15, 15, -75, 25))
  expect_equal(sum(x$daily$market), -20)

  # The weekend dividend is priced on the next trading day.
  div <- x$ledger[x$ledger$type == "dividend", ]
  expect_equal(div$price_date, as.Date("2024-01-08"))

  # Balance identity holds with withdrawals, fees, transfers, and a loss.
  d <- x$daily
  flows <- d$employee + d$employer + d$rollover + d$withdrawals + d$fees + d$market
  expect_equal(diff(d$balance), flows[-1])
})

test_that("market gain is negative when prices fall", {
  nav <- data.frame(date = as.Date(c("2024-01-02", "2024-01-03")), nav = c(10, 8))
  tx <- data.frame(date = "2024-01-02", type = "contribution", amount = 100,
                   source = "Employee pre-tax", holding = "fund")
  x <- load_401k(write_fixture(tx, nav))
  expect_equal(sum(x$daily$market), -20)
  expect_equal(tail(x$daily$balance, 1), 80)
})

test_that("balances by source add up to the total and stay non-negative", {
  fixture <- load_401k(write_fixture(fixture_tx(), fixture_nav()))
  for (x in list(load_401k(demo_data_dir()), fixture)) {
    totals <- tapply(x$sources$balance, x$sources$end, sum)
    expect_equal(as.numeric(totals), x$daily$balance[match(as.Date(names(totals)), x$daily$date)],
                 tolerance = 1e-8)
    expect_true(all(x$sources$balance >= -1e-8))
  }
})

test_that("a transfer out of the fund is split by fund holdings, not cash", {
  # Before the Jan 8 transfer, units are 9.6 pre-tax, 4.8 match, 9.6 Roth, and
  # only the Roth source holds cash. The $30 leaving the fund comes 40/20/40
  # out of those units, so pre-tax ends with $12 of cash rather than none.
  dir <- write_fixture(fixture_tx(), fixture_nav())
  x <- load_401k(dir)
  s <- source_balances(x$ledger, x$nav, as.Date("2024-01-08"))
  expect_equal(s$balance[s$source == "Employee pre-tax"], 88)
  expect_equal(s$balance[s$source == "Employer match"], 44)
  expect_equal(s$balance[s$source == "Employee Roth"], 189)
})

test_that("reconciliation marks dates outside the tolerance", {
  balances <- data.frame(date = as.Date(c("2024-01-05", "2024-01-08")), balance = c(316, 300))
  dir <- write_fixture(fixture_tx(), fixture_nav(), balances)
  expect_warning(x <- load_401k(dir), "misses balances.csv")
  expect_equal(x$recon$status, c("OK", "Off"))
  expect_equal(x$recon$balance_diff, c(0, 21))
  expect_length(x$warnings, 1)

  x <- expect_silent(load_401k(dir, tolerance = 25))
  expect_equal(x$recon$status, c("OK", "OK"))
})

test_that("a transaction priced long after its date is flagged", {
  nav <- data.frame(date = as.Date(c("2024-01-02", "2024-01-12")), nav = c(10, 11))
  tx <- data.frame(date = c("2024-01-02", "2024-01-05"), type = "contribution", amount = 100,
                   source = "Employee pre-tax", holding = "fund")
  expect_warning(x <- load_401k(write_fixture(tx, nav)), "priced more than 4 days later")
  expect_match(x$warnings, "2024-01-05 \\(priced 2024-01-12\\)")
})

test_that("the demo has no data warnings", {
  expect_silent(x <- load_401k(demo_data_dir()))
  expect_length(x$warnings, 0)
})
