#' Load and rebuild an account
#'
#' Reads `transactions.csv`, the price file, and the optional `balances.csv`
#' from `data_dir`, then rebuilds the daily history that the dashboard shows.
#' Use it to work with the numbers directly in R.
#'
#' @inheritParams read_nav
#' @param data_dir Folder holding your `transactions.csv` and price file.
#'   Defaults to the `r401k.data_dir` option, or the bundled demo if that
#'   isn't set. Set it once with `options(r401k.data_dir = "~/my401k")`, for
#'   example in your `.Rprofile`.
#' @param plan Plan settings from [plan_settings()].
#' @param tolerance How far, in dollars, a rebuilt balance may miss a balance
#'   in `balances.csv` before that date is marked "Off".
#' @return A list with:
#'   * `daily`: one row per trading day, with units, cash, balance, the day's
#'     deposits by source, fees, and market gain.
#'   * `quarters`: one row per calendar quarter, with fund return,
#'     money-weighted return, and balance change.
#'   * `ledger`: every transaction with its price and units.
#'   * `sources`: balance by money source at each quarter end.
#'   * `recon`: each `balances.csv` date with a status of `"OK"` or `"Off"`.
#'   * `warnings`: data problems, such as transactions priced long after their
#'     date or balances that don't reconcile. Each is also raised as an R warning.
#'   * `transactions`, `nav`, `balances`, `fund_id`, and `data_through`.
#' @export
#' @examples
#' x <- load_401k()
#' tail(x$daily[, c("date", "balance", "market")])
#' x$quarters[, c("period", "fund_return", "money_weighted", "naive_change")]
load_401k <- function(data_dir = default_data_dir(), fund_id = NULL, plan = plan_settings(),
                      tolerance = 1) {
  if (is.null(fund_id)) fund_id <- detect_fund_id(data_dir)
  tx       <- read_transactions(data_dir)
  nav      <- read_nav(data_dir, fund_id)
  balances <- read_balances(data_dir)
  ledger   <- build_ledger(tx, nav)
  daily    <- build_daily(ledger, nav)
  through  <- min(max(c(tx$date, balances$date)), max(nav$date))

  D <- list(fund_id = fund_id, transactions = tx, nav = nav, balances = balances,
            ledger = ledger, daily = daily, data_through = through)
  D$quarters <- quarter_table(D, plan)
  D$sources  <- source_balances(ledger, nav, D$quarters$end)
  D$recon    <- reconciliation(D, tolerance)
  D$warnings <- data_warnings(D, tolerance)
  for (w in D$warnings) warning(w, call. = FALSE)
  D
}

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
