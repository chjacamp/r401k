# Turn transactions and prices into a daily account history --------------------

build_ledger <- function(tx, nav) {
  # Price each row on the first NAV date on or after it, which handles weekend
  # and holiday dates.
  i <- findInterval(as.numeric(tx$date) - 0.5, as.numeric(nav$date)) + 1
  if (any(i > nrow(nav))) {
    stop(sprintf("Prices end %s but transactions run through %s. Update the price file.",
                 max(nav$date), max(tx$date)), call. = FALSE)
  }
  if (min(tx$date) < min(nav$date) - 7) {
    stop(sprintf("Prices start %s but transactions start %s. Extend the price file.",
                 min(nav$date), min(tx$date)), call. = FALSE)
  }
  tx$price_date <- nav$date[i]
  tx$nav        <- nav$nav[i]
  tx |>
    mutate(
      fund_amount = case_when(type == "transfer" ~ amount, holding == "fund" ~ amount, TRUE ~ 0),
      cash_amount = case_when(type == "transfer" ~ -amount, holding == "cash" ~ amount, TRUE ~ 0),
      units = fund_amount / nav
    ) |>
    arrange(price_date, type)
}

build_daily <- function(ledger, nav) {
  flows <- ledger |>
    group_by(date = price_date) |>
    summarise(
      d_units     = sum(units),
      d_cash      = sum(cash_amount),
      employee    = sum(amount[type == "contribution" & who == "employee"]),
      employer    = sum(amount[type == "contribution" & who == "employer"]),
      rollover    = sum(amount[type == "rollover"]),
      withdrawals = sum(amount[type == "withdrawal"]),
      fees        = sum(amount[type == "fee"]),
      dividends   = sum(amount[type == "dividend"]),
      .groups = "drop"
    )
  nav |>
    filter(date >= min(ledger$price_date)) |>
    left_join(flows, by = "date") |>
    mutate(across(-c(date, nav), ~ coalesce(.x, 0))) |>
    mutate(
      units      = cumsum(d_units),
      cash       = cumsum(d_cash),
      fund_value = units * nav,
      balance    = fund_value + cash,
      # Price change on units already held. New units are bought at the close,
      # so they earn nothing on the day they arrive.
      market     = coalesce(lag(units) * (nav - lag(nav)), 0) + dividends
    )
}

# Everything the dashboard needs, read fresh from data/.
load_dashboard_data <- function(fund_id, plan, data_dir = "data") {
  tx       <- read_transactions(data_dir)
  nav      <- read_nav(fund_id, data_dir)
  balances <- read_balances(data_dir)
  ledger   <- build_ledger(tx, nav)
  daily    <- build_daily(ledger, nav)
  through  <- min(max(c(tx$date, balances$date)), max(nav$date))

  D <- list(transactions = tx, nav = nav, balances = balances, ledger = ledger,
            daily = daily, data_through = through)
  D$quarters <- quarter_table(D, plan)
  D$sources  <- source_balances(ledger, nav, D$quarters$end)
  D$recon    <- reconciliation(D)
  D
}
