base_args <- function(...) {
  modifyList(list(start_balance = 10000, from = as.Date("2026-06-15"), to_year = 2035,
                  income = 80000, raise = 0.03, pct = 0.06, step = 0, cap = 0.15,
                  nonelective = 0.03, match_rate = 1, match_cap = 0.04, ret = 0.07,
                  infl = 0.025, limit = 24500, limit_growth = 0.025, fee = 5),
             list(...))
}

test_that("projected balance is the start plus contributions plus growth", {
  p <- do.call(project, base_args())
  expect_equal(p$balance, p$start + p$you + p$employer + p$growth)
  expect_equal(max(p$year), 2035)
})

test_that("with no return and no fee there is no growth", {
  p <- do.call(project, base_args(ret = 0, fee = 0))
  expect_equal(tail(p$growth, 1), 0, tolerance = 1e-8)
  expect_equal(tail(p$balance, 1), 10000 + tail(p$you, 1) + tail(p$employer, 1))
})

test_that("employee deferrals stop at the IRS limit", {
  p <- do.call(project, base_args(pct = 0.5, limit = 12000, limit_growth = 0, raise = 0))
  yearly <- diff(c(0, p$you))[-1]   # skip the partial first year
  expect_true(all(abs(yearly - 12000) < 1e-6))
})
