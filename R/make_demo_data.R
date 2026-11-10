# Generate the fake data shipped with this repo ----------------------------------
#
# The person and the paychecks are invented. Prices are the fund's real daily
# NAV from data/nav_<fund_id>.csv. Five years of paychecks from a $0 balance.
# Re-run with source("global.R"); make_demo_data()

make_demo_data <- function(data_dir = "data", fund_id = FUND_ID, plan = PLAN,
                           start = as.Date("2021-11-01"), end = NULL) {
  prices <- read_nav(fund_id, data_dir)
  if (is.null(end)) end <- max(prices$date)
  days <- prices$date[prices$date >= start & prices$date <= end]

  # Paid on the 15th and the last day of each month, or the prior weekday.
  back_to_weekday <- function(d) {
    wday <- as.POSIXlt(d)$wday
    d - ifelse(wday == 0, 2, ifelse(wday == 6, 1, 0))
  }
  firsts <- seq(as.Date(format(start, "%Y-%m-01")), end + 31, by = "month")
  pay_dates <- back_to_weekday(sort(c(firsts + 14, firsts - 1)))
  pay_dates <- pay_dates[pay_dates >= start & pay_dates <= end]

  # Money reaches the fund two trading days after payday.
  i <- findInterval(as.numeric(pay_dates), as.numeric(days)) + 2
  pay_dates <- pay_dates[i <= length(days)]
  effective <- days[i[i <= length(days)]]

  year   <- pmin(as.integer(format(pay_dates, "%Y")), 2026)
  salary <- c(78000, 81000, 84000, 87500, 90500, 93500)[year - 2020]
  rate   <- c(0.06, 0.06, 0.08, 0.08, 0.10, 0.10)[year - 2020]
  bonus  <- ifelse(format(pay_dates, "%m") == "03" & as.POSIXlt(pay_dates)$mday <= 16, 1.5, 1)
  check  <- salary / 24 * bonus

  deposit <- function(amount, source) {
    data.frame(date = effective, type = "contribution", amount = round(amount, 2),
               source = source, holding = "fund")
  }
  contributions <- rbind(
    deposit(rate * check, "Employee pre-tax"),
    deposit(plan$match_rate * pmin(rate, plan$match_cap) * check, "Employer match"),
    deposit(plan$nonelective * check, "Employer non-elective")
  )

  fee_dates <- back_to_weekday(firsts + 15)
  fee_dates <- fee_dates[fee_dates > min(effective) & fee_dates <= end]
  fees <- data.frame(date = fee_dates, type = "fee", amount = -plan$monthly_fee,
                     source = NA_character_, holding = "fund")

  tx <- rbind(contributions, fees)
  tx <- tx[order(tx$date), ]
  dir.create(data_dir, showWarnings = FALSE)
  write.csv(tx, file.path(data_dir, "transactions.csv"), row.names = FALSE, na = "")

  message(sprintf("Wrote %d transactions to %s/, priced against fund %s.", nrow(tx), data_dir, fund_id))
  invisible(tx)
}
