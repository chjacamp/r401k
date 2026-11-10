# Projection --------------------------------------------------------------------

# Simple monthly compounding from assumptions you set. Pay and your rate step
# up each January; the IRS limit caps your deferrals but not employer money.
project <- function(start_balance, from, to_year, income, raise, pct, step, cap,
                    nonelective, match_rate, match_cap, ret, infl, limit, limit_growth, fee) {
  first  <- as.Date(format(from, "%Y-%m-01"))
  months <- seq(first, as.Date(sprintf("%d-12-01", to_year)), by = "month")[-1]
  years_in <- as.integer(format(months, "%Y")) - as.integer(format(from, "%Y"))

  pay_t  <- income * (1 + raise)^years_in
  rate_t <- pmin(pct + step * years_in, max(pct, cap))
  lim_t  <- limit * (1 + limit_growth)^years_in
  you_t  <- pmin(rate_t * pay_t, lim_t) / 12
  er_t   <- (nonelective + match_rate * pmin(rate_t, match_cap)) * pay_t / 12

  m   <- (1 + ret)^(1 / 12) - 1
  bal <- numeric(length(months))
  now <- start_balance
  for (i in seq_along(months)) {
    now    <- now * (1 + m) + (you_t[i] + er_t[i]) * (1 + m / 2) - fee
    bal[i] <- now
  }
  tibble(
    month    = months,
    year     = as.integer(format(months, "%Y")),
    balance  = bal,
    you      = cumsum(you_t),
    employer = cumsum(er_t),
    start    = start_balance,
    growth   = bal - start_balance - cumsum(you_t) - cumsum(er_t),
    today    = bal / (1 + infl)^(seq_along(months) / 12)
  ) |>
    filter(format(month, "%m") == "12")
}
