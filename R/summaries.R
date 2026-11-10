# Summary tables ----------------------------------------------------------------

quarter_label <- function(d) {
  sprintf("%s Q%d", format(d, "%Y"), ceiling(as.integer(format(d, "%m")) / 3))
}

is_nonelective <- function(source) grepl("non-elective", source, ignore.case = TRUE)

quarter_table <- function(D, plan) {
  d <- D$daily[D$daily$date <= D$data_through, ]
  first_nav <- d$nav[1]

  ne <- D$ledger |>
    filter(type == "contribution", is_nonelective(source)) |>
    mutate(period = quarter_label(price_date)) |>
    group_by(period) |>
    summarise(nonelective = sum(amount), .groups = "drop")

  q <- d |>
    mutate(period = quarter_label(date)) |>
    group_by(period) |>
    summarise(start = min(date), end = max(date), rollover = sum(rollover),
              employee = sum(employee), employer = sum(employer),
              withdrawals = sum(withdrawals), market = sum(market), fees = sum(fees),
              end_balance = last(balance), nav_end = last(nav), .groups = "drop") |>
    arrange(start) |>
    mutate(begin = lag(end_balance, default = 0),
           fund_return = nav_end / lag(nav_end, default = first_nav) - 1,
           naive_change = ifelse(begin > 0, end_balance / begin - 1, NA_real_)) |>
    left_join(ne, by = "period") |>
    mutate(nonelective = coalesce(nonelective, 0),
           pay = if (plan$nonelective > 0) ifelse(nonelective > 0, nonelective / plan$nonelective, NA_real_) else NA_real_,
           employee_pct = employee / pay,
           match_pct = (employer - nonelective) / pay)

  q$money_weighted <- vapply(seq_len(nrow(q)), function(i) {
    window_stats(D, q$start[i], q$end[i])$period_return
  }, 0)

  q$reported_return <- NA_real_
  if (!is.null(D$balances)) {
    m <- match(q$period, quarter_label(D$balances$date))
    q$reported_return <- D$balances$reported_return[m]
  }
  q
}

# Balance by money source. Transactions say who funded each deposit but not
# which source a fee came out of, so fees are spread in proportion to holdings.
source_balances <- function(ledger, nav, dates) {
  sources <- sort(unique(na.omit(ledger$source)))
  units <- setNames(numeric(length(sources)), sources)
  cash  <- units
  spread <- function(weights) {
    if (sum(weights) > 0) weights / sum(weights) else rep(1 / length(weights), length(weights))
  }
  out <- vector("list", length(dates))
  pos <- 1
  ledger <- ledger[order(ledger$price_date, ledger$type), ]
  for (k in seq_along(dates)) {
    while (pos <= nrow(ledger) && ledger$price_date[pos] <= dates[k]) {
      r <- ledger[pos, ]
      if (r$type == "transfer") {
        moved <- r$amount * spread(cash)
        cash  <- cash - moved
        units <- units + moved / r$nav
      } else if (!is.na(r$source)) {
        if (r$holding == "fund") units[r$source] <- units[r$source] + r$amount / r$nav
        else cash[r$source] <- cash[r$source] + r$amount
      } else if (r$holding == "fund") {
        units <- units + (r$amount / r$nav) * spread(units)
      } else {
        cash <- cash + r$amount * spread(cash)
      }
      pos <- pos + 1
    }
    price <- nav$nav[findInterval(as.numeric(dates[k]), as.numeric(nav$date))]
    out[[k]] <- tibble(end = dates[k], source = sources, balance = unname(units * price + cash))
  }
  bind_rows(out)
}

# Compare the rebuilt history with data/balances.csv, if it exists.
reconciliation <- function(D) {
  b <- D$balances
  if (is.null(b)) return(NULL)
  bind_rows(lapply(seq_len(nrow(b)), function(i) {
    x <- tail(D$daily[D$daily$date <= b$date[i], ], 1)
    if (!nrow(x)) return(NULL)
    tibble(date = b$date[i],
           reported_balance = b$balance[i], rebuilt_balance = x$balance,
           balance_diff = x$balance - b$balance[i],
           reported_units = b$units[i], rebuilt_units = x$units,
           units_diff = x$units - b$units[i],
           reported_price = b$unit_price[i], file_price = x$nav)
  }))
}

# Annual pay implied by the last 12 months of employer non-elective money.
infer_annual_pay <- function(D, plan) {
  if (plan$nonelective <= 0) return(NA_real_)
  recent <- D$ledger |>
    filter(type == "contribution", is_nonelective(source), date > D$data_through - 365)
  if (!nrow(recent)) return(NA_real_)
  sum(recent$amount) / plan$nonelective
}

# Your deferral rate on the most recent paycheck.
infer_current_rate <- function(D, plan) {
  if (plan$nonelective <= 0) return(NA_real_)
  latest <- max(D$ledger$date[D$ledger$type == "contribution" & is_nonelective(D$ledger$source)],
                as.Date(-Inf))
  if (!is.finite(latest)) return(NA_real_)
  day <- D$ledger[D$ledger$date == latest & D$ledger$type == "contribution", ]
  sum(day$amount[day$who == "employee"]) / (sum(day$amount[is_nonelective(day$source)]) / plan$nonelective)
}
