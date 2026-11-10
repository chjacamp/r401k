# Return math -------------------------------------------------------------------

xirr <- function(dates, flows) {
  if (length(flows) < 2 || all(flows >= 0) || all(flows <= 0)) return(NA_real_)
  t <- as.numeric(dates - min(dates)) / 365.25
  f <- function(r) sum(flows / (1 + r)^t)
  tryCatch(uniroot(f, c(-0.95, 20), tol = 1e-9, extendInt = "yes")$root,
           error = function(e) NA_real_)
}

# Everything the dashboard reports for a date range.
window_stats <- function(D, start, end) {
  d <- D$daily
  w <- d[d$date >= start & d$date <= end, ]
  if (!nrow(w)) return(NULL)
  prior     <- d[d$date < start, ]
  has_prior <- nrow(prior) > 0
  begin     <- if (has_prior) tail(prior$balance, 1) else 0
  base_nav  <- if (has_prior) tail(prior$nav, 1) else w$nav[1]
  base_date <- if (has_prior) tail(prior$date, 1) else w$date[1]
  last      <- tail(w, 1)

  flows <- D$ledger |>
    filter(price_date >= start, price_date <= end,
           type %in% c("contribution", "rollover", "withdrawal"))
  if (begin > 0) {
    dates <- c(base_date, flows$price_date, last$date)
    cash  <- c(-begin, -flows$amount, last$balance)
  } else {
    dates <- c(flows$price_date, last$date)
    cash  <- c(-flows$amount, last$balance)
  }
  irr   <- xirr(dates, cash)
  years <- as.numeric(last$date - min(dates)) / 365.25
  rets  <- diff(c(base_nav, w$nav)) / c(base_nav, head(w$nav, -1))
  path  <- c(base_nav, w$nav)

  list(
    start = start, end = last$date, base_date = base_date, years = years,
    begin = begin, end_balance = last$balance,
    employee = sum(w$employee), employer = sum(w$employer), rollover = sum(w$rollover),
    withdrawals = sum(w$withdrawals), fees = sum(w$fees), market = sum(w$market),
    fund_return = last$nav / base_nav - 1,
    irr = irr,
    period_return = if (is.na(irr)) NA_real_ else (1 + irr)^years - 1,
    naive_change = if (begin > 0) last$balance / begin - 1 else NA_real_,
    volatility = if (length(rets) > 20) sd(rets) * sqrt(252) else NA_real_,
    max_drawdown = min(path / cummax(path) - 1)
  )
}

months_back <- function(d, n) {
  anchor <- seq(as.Date(format(d, "%Y-%m-01")), by = paste(-n, "months"), length.out = 2)[2]
  month_end <- seq(anchor, by = "month", length.out = 2)[2] - 1
  min(anchor + as.POSIXlt(d)$mday - 1, month_end)
}
