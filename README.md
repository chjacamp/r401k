# 401kR

A small Shiny dashboard for 401(k) transaction data. It answers the question a 401(k) website should answer first: **how much of my balance came from what I put in, and how much came from the market?**

![The dashboard running on the repo's demo data](docs/dashboard.jpg)

## Why this exists

Some 401(k) dashboards show the change in your balance from one date to the next and report it as a percent. That counts every paycheck deposit as if it were investment return. A balance can be up 20% in a year the fund returned 10%, and the chart gives you no way to tell.

This app rebuilds your account from its transactions and your fund's daily price, then keeps three different things separate:

- **Fund return:** what the fund itself did. Your deposits can't affect it.
- **Your return:** a money-weighted return (XIRR) that reflects when your money actually arrived, net of plan fees.
- **Balance change:** the misleading number, shown next to the others so you can see the gap.

Any account you can describe as dated transactions in a single fund will work.

## What's in the dashboard

- **Performance:** for any period, how much came from you, your employer, a rollover, the market, and fees. Also monthly bars, risk stats, and implied pay and contribution rates.
- **Quarters and ledger:** a quarter-by-quarter table, balances by money source, and every transaction.
- **Projection:** retirement balance from your pay, raises, contribution rate, and yearly rate increases, compared across contribution rates.
- **Method and checks:** what each number means, and optionally how the rebuilt history compares with balances from your statements.

## Quick start

The repo ships with demo data: five years of made-up paychecks from a $0 start, priced against the real daily NAV of the Vanguard Target Retirement 2060 Trust Select (fund 1685).

```r
install.packages(c("shiny", "bslib", "plotly", "DT", "dplyr", "scales"))
shiny::runApp("401kR")   # path to this folder
```

It needs R 4.1 or later.

## Use your own fund and data

Everything the app reads is in `data/`. You need two files: daily prices and your transactions.

**1. Add daily prices** as `data/nav_<your_fund_id>.csv`. The id is any name you like.

```csv
date,nav
2024-01-02,50.12
2024-01-03,49.87
```

`nav` has to be a total-return series, meaning it doesn't drop when the fund pays a distribution. Collective trusts usually qualify as published, because they reinvest internally. For a mutual fund or ETF, use an adjusted close, and don't also list reinvested distributions as transactions.

**2. Point the app at your fund** by editing the top of `global.R`:

```r
FUND_ID   <- "your_fund_id"
FUND_NAME <- "Your Fund Name"
```

The plan settings and projection defaults are in the same file. Leaving `annual_pay` and `your_rate` as `NA` infers them from your employer's non-elective contributions, if you have any.

**3. Add your transactions** as `data/transactions.csv`:

```csv
date,type,amount,source,holding
2024-01-17,contribution,300.00,Employee pre-tax,fund
2024-01-17,contribution,150.00,Employer match,fund
2024-02-16,fee,-4.50,,fund
```

| Column | Required | Meaning |
|---|---|---|
| `date` | yes | `YYYY-MM-DD`, the day the money was priced (the "effective date" on a statement) |
| `type` | yes | `contribution`, `rollover`, `fee`, `withdrawal`, `dividend`, or `transfer` |
| `amount` | yes | Dollars. Negative for withdrawals and normal fees |
| `source` | for deposits | Who funded it. Anything starting with `Employer` counts as employer money |
| `holding` | no | `fund` (default) or `cash`. Dividends default to `cash` |

A `transfer` moves money between cash and the fund; a positive amount moves it into the fund. Use `dividend` for interest on a cash position.

Then run `shiny::runApp()` again. Every balance, return, and chart is computed from these two files.

### Optional: check against your statements

If you add `data/balances.csv` with balances copied from your statements, the checks tab compares them with the rebuilt history. That's a quick way to catch a missing or mistyped transaction. The app doesn't use this file for any calculation, and the demo doesn't include one.

```csv
date,balance,units,unit_price,reported_return
2024-03-31,10250.14,165.102,62.08,0.0412
```

Only `date` and `balance` are required.

### Fetching prices with your own function

If you have a price source you're allowed to automate, wrap it in a function of `(start, end)` that returns `date` and `nav`. `update_nav()` requests only the missing dates and merges them into the right file.

```r
source("global.R")

from_my_download <- function(start, end) {
  x <- read.csv("~/Downloads/my_fund_prices.csv")
  data.frame(date = as.Date(x$Date, "%m/%d/%Y"), nav = x$Price)
}

update_nav("your_fund_id", from_my_download, first_needed = as.Date("2024-01-01"))
```

## Price data

`data/nav_1685.csv` holds the trust's published daily NAV, included so the demo runs offline. The repo has no code that downloads prices. For your own fund, supply prices yourself or write a fetch function as described above, and check the terms of any source you automate, since many fund websites restrict automated access.

## Keep your own data private

Your transactions are personal. The `.gitignore` excludes PDFs, but files in `data/` are tracked because the demo data lives there, so keep your real data in a private copy rather than a public fork.

## How the numbers work

Each deposit buys units at the price on its date, and each fee sells units the same way. Daily market gain is yesterday's units times today's price change, plus any cash interest. The fund's return comes only from its prices. Your return is the rate that makes your dated cash flows equal your ending balance.

After your last transaction, units are held flat and priced daily, so later deposits don't appear until you add them.

To regenerate the demo transactions, run `source("global.R"); make_demo_data()`.

## License

[MIT](LICENSE), covering the code in this repo. It doesn't cover `data/nav_1685.csv`, which is the fund's published price history, or any third-party names.

## Disclaimer

Not affiliated with or endorsed by Vanguard or any 401(k) plan provider. Fund names are trademarks of their owners and are used only to identify the demo's price data. This is a personal project for information only and isn't investment, tax, or legal advice. Projections are arithmetic on assumptions you choose, not forecasts. Provided as is, with no warranty.
