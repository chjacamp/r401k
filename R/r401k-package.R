#' @keywords internal
#' @import shiny
#' @import dplyr
#' @importFrom bslib accordion accordion_panel bs_theme card card_body card_header
#'   font_collection layout_columns layout_sidebar nav_panel nav_panel_hidden
#'   nav_select nav_spacer navset_hidden page_navbar sidebar
#' @importFrom plotly add_bars add_lines add_trace config layout plot_ly
#'   plotlyOutput renderPlotly
#' @importFrom stats na.omit sd setNames uniroot
#' @importFrom utils head modifyList read.csv tail write.csv
#' @importFrom grDevices col2rgb
"_PACKAGE"

# Column names used inside dplyr verbs.
utils::globalVariables(c(
  "Price file", "Rebuilt units", "Reported price", "Reported units", "Units difference",
  "amount", "balance", "balance_diff", "begin", "cash", "cash_amount", "change", "d_cash",
  "d_units", "dividends", "employee", "employee_pct", "employer", "end_balance", "er", "fees",
  "file_price", "fund_amount", "fund_return", "fund_value", "growth", "holding", "label", "market",
  "money_weighted", "month", "naive_change", "nav", "nav_end", "nonelective", "pay", "period",
  "price_date", "rate", "rebuilt_balance", "rebuilt_units", "reported_balance", "reported_price",
  "reported_return", "reported_units", "roll", "rollover", "start", "status", "this_year", "today", "type",
  "units", "units_diff", "value", "who", "withdrawals", "y1", "y2", "y3", "year", "you"
))
