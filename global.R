# global.R: parameters, packages, and data loading.
# Run the app from this folder with shiny::runApp().

# Fund --------------------------------------------------------------------------
FUND_ID   <- "1685"     # prices are read from data/nav_<FUND_ID>.csv
FUND_NAME <- "Vanguard Target Retirement 2060 Trust Select"
DATA_DIR  <- "data"

# Plan --------------------------------------------------------------------------
PLAN <- list(
  nonelective        = 0.03,     # employer contribution regardless of yours
  match_rate         = 1,        # employer matches this much per dollar...
  match_cap          = 0.04,     # ...on your deferrals up to this share of pay
  monthly_fee        = 4.50,
  fund_expense_ratio = 0.00045
)

# Projection starting values ------------------------------------------------------
# NA means work it out from data/transactions.csv.
PROJECTION <- list(
  annual_pay       = NA,
  your_rate        = NA,    # percent of pay
  raise            = 3,
  rate_step        = 0,
  rate_cap         = 15,
  expected_return  = 7,
  return_range     = 2,
  inflation        = 2.5,
  retirement_year  = 2060,
  irs_limit        = 24500,
  irs_limit_growth = 2.5
)

# Packages -----------------------------------------------------------------------
needed <- c("shiny", "bslib", "plotly", "DT", "dplyr", "scales")
absent <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(absent)) {
  stop('Install the missing packages first: install.packages(c("',
       paste(absent, collapse = '", "'), '"))', call. = FALSE)
}
suppressWarnings(suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(plotly)
  library(dplyr)    # attached after plotly so dplyr::filter wins
}))

for (helper in list.files("R", pattern = "\\.[rR]$", full.names = TRUE)) source(helper)

# Data ---------------------------------------------------------------------------
# The app only reads files in data/. It never downloads anything.
D <- NULL
if (file.exists(file.path(DATA_DIR, "transactions.csv")) && file.exists(nav_path(FUND_ID, DATA_DIR))) {
  D <- load_dashboard_data(FUND_ID, PLAN, DATA_DIR)

  first_date    <- min(D$daily$date)
  last_nav_date <- max(D$daily$date)
  data_through  <- D$data_through

  first_known <- function(...) {
    for (x in list(...)) if (!is.na(x)) return(x)
  }
  default_pay  <- first_known(PROJECTION$annual_pay, infer_annual_pay(D, PLAN), 75000)
  default_rate <- first_known(PROJECTION$your_rate, infer_current_rate(D, PLAN) * 100, 6)
}
