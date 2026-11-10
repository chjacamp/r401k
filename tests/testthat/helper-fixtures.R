# Writes a throwaway data folder for one test.
write_fixture <- function(tx, nav, balances = NULL, fund_id = "test") {
  dir <- tempfile("r401k-")
  dir.create(dir)
  write.csv(nav, file.path(dir, sprintf("nav_%s.csv", fund_id)), row.names = FALSE)
  write.csv(tx, file.path(dir, "transactions.csv"), row.names = FALSE, na = "")
  if (!is.null(balances)) write.csv(balances, file.path(dir, "balances.csv"), row.names = FALSE)
  dir
}

# Five trading days, Tue Jan 2 to Mon Jan 8, 2024, with one losing day.
fixture_nav <- function() {
  data.frame(date = as.Date(c("2024-01-02", "2024-01-03", "2024-01-04", "2024-01-05", "2024-01-08")),
             nav  = c(10, 11, 12, 9, 10))
}

# Worked out by hand:
#   Jan 2  buy 10 units (you, $100) and 5 units (employer, $50) at $10
#   Jan 3  $220 Roth rollover lands in cash
#   Jan 4  $120 moves from cash into the fund: 10 units at $12
#   Jan 5  $9 fee sells 1 unit at $9 (the price fell from $12)
#   Jan 6  $1 cash dividend on a Saturday, priced Monday Jan 8
#   Jan 8  $20 withdrawal sells 2 units, then $30 moves from the fund to cash: 3 units at $10
# Ends with 19 units at $10 plus $131 cash = $321.
fixture_tx <- function() {
  data.frame(
    date    = c("2024-01-02", "2024-01-02", "2024-01-03", "2024-01-04", "2024-01-05",
                "2024-01-06", "2024-01-08", "2024-01-08"),
    type    = c("contribution", "contribution", "rollover", "transfer", "fee",
                "dividend", "withdrawal", "transfer"),
    amount  = c(100, 50, 220, 120, -9, 1, -20, -30),
    source  = c("Employee pre-tax", "Employer match", "Employee Roth", NA, NA, NA, NA, NA),
    holding = c("fund", "fund", "cash", NA, "fund", "cash", "fund", NA)
  )
}
