test_that("pasted tab-separated rows keep amounts with commas whole", {
  rows <- split_text_rows(c("Date\tActivity\tAmount", "01/15/2024\tContribution\t$1,234.56",
                            "01/31/2024\tFee\t$4.50"))
  df <- table_from_rows(rows)
  expect_equal(names(df), c("Date", "Activity", "Amount"))
  expect_equal(df$Amount, c("$1,234.56", "$4.50"))
})

test_that("CSV exports with title lines and a total row are read from the column names down", {
  lines <- c("Transaction History", "Plan: Example 401(k)", "",
             "\"Trade Date\",\"Activity\",\"Amount\",\"\"",
             "\"01/15/2024\",\"Contribution\",\"$100.00\",\"\"",
             "\"01/16/2024\",\"Employer, match\",\"$50.00\",\"\"",
             "Total,,$150.00,")
  rows <- split_text_rows(lines)
  expect_equal(detect_header_row(rows), 4)
  df <- table_from_rows(rows)
  expect_equal(names(df), c("Trade Date", "Activity", "Amount"))
  expect_equal(df$Activity, c("Contribution", "Employer, match", ""))
  expect_equal(df$`Trade Date`[3], "Total")
  expect_equal(attr(df, "header_row"), 4)
  expect_error(table_from_rows(rows, header = 7), "no rows below")
})

test_that("blank and repeated column names get usable names", {
  df <- table_from_rows(list(c("Date", "", "Amount", "Amount"), c("2024-01-02", "x", "1", "2")))
  expect_equal(names(df), c("Date", "Column 2", "Amount", "Amount 1"))
})

test_that("date formats are detected, month first unless a day can't be a month", {
  expect_equal(detect_date_format(c("01/02/2024", "03/04/2024")), "01/31/2024")
  expect_equal(detect_date_format(c("01/02/2024", "25/04/2024", "13/05/2024")), "31/01/2024")
  expect_equal(detect_date_format(c("2024-01-02 00:00:00", "2024-01-03T16:00:00Z")), "2024-01-31")
  expect_equal(detect_date_format(c("Jan 2, 2024", "February 3, 2024")), "Jan 31, 2024")
  expect_equal(detect_date_format(c("45293", "45294")), "Excel serial number")
  expect_true(is.na(detect_date_format(c("Contribution", "Fee"))))
  expect_equal(parse_dates(c("1/2/2024 4:00 PM", "Total"), "01/31/2024"), as.Date(c("2024-01-02", NA)))
  expect_equal(parse_dates("45293", "Excel serial number"), as.Date("2024-01-02"))
  # A two-digit year isn't read as the first two digits of a four-digit one.
  expect_true(is.na(parse_dates("01/15/2024", "01/31/24")))
})

test_that("money is read the ways statements write it", {
  expect_equal(parse_money(c("$1,234.56", "(4.50)", "-$5.00", "5.00-", "+3", paste0(MINUS, "7"), "", "n/a")),
               c(1234.56, -4.5, -5, -5, 3, -7, NA, NA))
})

test_that("row labels get sensible first guesses", {
  labels <- c(paste("Contribution", MIDDOT, "Employee Pre-Tax"), paste("Contribution", MIDDOT, "Roth"), "Employer Match",
              "Safe Harbor Non-Elective", "Administrative Fee", "Dividend Reinvestment",
              "Rollover In", "Hardship Withdrawal", "Loan Repayment", "Ending Balance", "Mystery")
  expect_equal(guess_category(labels),
               c("employee_pretax", "employee_roth", "employer_match", "employer_nonelective", "fee",
                 "skip", "rollover", "withdrawal", "skip", "skip", "skip"))
})

test_that("an export round-trips to the same transactions", {
  tx <- fixture_tx()[fixture_tx()$type %in% c("contribution", "fee", "withdrawal"), ]
  export <- data.frame(
    `Effective Date` = format(as.Date(tx$date), "%m/%d/%Y"),
    Activity = c("Contribution", "Contribution", "Plan Fee", "Distribution"),
    Source = c("Employee Pre-Tax", "Employer Match", "", ""),
    Investment = "Target Fund",
    Amount = sprintf("$%.2f", abs(tx$amount)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  export <- rbind(export, data.frame(`Effective Date` = "01/03/2024", Activity = "Contribution",
                                     Source = "Employee Pre-Tax", Investment = "Other Fund",
                                     Amount = "$5.00", check.names = FALSE))
  g <- guess_tx_columns(export)
  expect_equal(g$date, "Effective Date")
  expect_equal(g$amount, "Amount")
  expect_setequal(g$labels, c("Activity", "Source"))

  conv <- convert_transactions(export, g$date, g$amount, g$labels,
                               filter_col = "Investment", filter_values = "Target Fund")
  expect_equal(conv$tx$type, tx$type)
  expect_equal(conv$tx$amount, tx$amount)
  expect_equal(conv$tx$source[1:2], c("Employee pre-tax", "Employer match"))
  expect_match(conv$notes, "other investments", all = FALSE)
  expect_match(conv$notes, "Flipped the sign of \"Plan Fee\", \"Distribution\"", all = FALSE)
  expect_silent(check_transactions(conv$tx, lines = FALSE))

  # Choosing a kind by hand overrides the guess, and skipped rows are counted.
  cats <- c("Plan Fee" = "skip")
  conv2 <- convert_transactions(export, g$date, g$amount, g$labels, categories = cats,
                                filter_col = "Investment", filter_values = "Target Fund")
  expect_false("fee" %in% conv2$tx$type)
  expect_match(conv2$notes, "1 row\\(s\\) you chose to skip", all = FALSE)
})

test_that("prices with repeated dates are merged or refused", {
  df <- data.frame(Date = c("Jan 2, 2024", "Jan 2, 2024", "Jan 3, 2024", "Total"),
                   `Adj Close` = c("$10.00", "$10.00", "$11.00", ""), Volume = "100", check.names = FALSE)
  g <- guess_price_columns(df)
  expect_equal(g, list(date = "Date", price = "Adj Close"))
  p <- convert_prices(df, g$date, g$price)
  expect_equal(p$nav$nav, c(10, 11))
  expect_match(p$notes, "Left out 1 row")
  df$`Adj Close`[2] <- "$10.50"
  expect_error(convert_prices(df, "Date", "Adj Close"), "two different prices")
})

test_that("price gaps longer than a market closure are found", {
  nav <- data.frame(date = as.Date(c("2024-01-02", "2024-01-03", "2024-01-12", "2024-01-16")), nav = 1)
  gaps <- price_gaps(nav)
  expect_equal(gaps$after, as.Date("2024-01-03"))
  expect_equal(fund_slug("Target Date 2060 Trust (Class R)"), "target-date-2060-trust-class-r")
  expect_equal(fund_slug(""), "fund")
})

test_that("checks on tables report row numbers rather than file lines", {
  tx <- data.frame(date = c("2024-01-02", "Jan 3"), type = "contribution", amount = c(1, 2))
  expect_error(check_transactions(tx, "Transactions", lines = FALSE), "YYYY-MM-DD on row\\(s\\) 2")
  expect_error(check_transactions(tx[0, ], "Transactions"), "has no transactions")
  expect_error(check_balances(data.frame(date = "2024-01-02", balance = "lots"), "B", lines = FALSE),
               "balance isn't a number on row\\(s\\) 1")
})
