# Dashboard UI. Everything it shows comes from the context built by
# app_context(), so the app is configured through run_app()'s arguments.
app_ui <- function(ctx) {
  D             <- ctx$D
  plan          <- ctx$plan
  projection    <- ctx$projection
  first_date    <- ctx$first_date
  last_nav_date <- ctx$last_nav_date
  data_through  <- ctx$data_through
  default_pay   <- ctx$default_pay
  default_rate  <- ctx$default_rate

  page_navbar(
    title = "401(k) performance",
    fillable = FALSE,
    theme = bs_theme(version = 5, bg = INK$page, fg = INK$primary, primary = HUE$you,
                     base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "sans-serif")),
    header = tags$head(tags$style(HTML(APP_CSS))),

    nav_panel("Performance",
      div(class = "period-row",
          radioButtons("period", NULL, inline = TRUE, selected = "YTD",
                       choices = c("1M", "3M", "YTD", "1Y", "All", "Custom")),
          conditionalPanel("input.period == 'Custom'",
                           dateRangeInput("custom", NULL, start = max(first_date, data_through - 365),
                                          end = last_nav_date, min = first_date, max = last_nav_date))),
      uiOutput("narrative"),
      uiOutput("tiles"),
      layout_columns(col_widths = c(8, 4),
        chart_card("Where the change in balance came from",
                   "Bands are money added since the period began. The green area above them is what the market added.",
                   plotlyOutput("balance_plot", height = "340px")),
        chart_card("What changed the balance",
                   "Each source over the selected period.",
                   plotlyOutput("drivers_plot", height = "340px"))),
      layout_columns(col_widths = c(6, 6),
        chart_card("Fund return vs. balance change",
                   "Balance change is what some 401(k) sites chart. It counts your own deposits as gains.",
                   plotlyOutput("return_plot", height = "300px")),
        chart_card("Money added and market gain by month",
                   "Rollovers are left out so the monthly scale stays readable.",
                   plotlyOutput("monthly_plot", height = "300px"))),
      layout_columns(col_widths = c(6, 6),
        card(class = "chart-card", card_header(div(class = "card-title-text", "Risk and cost in this period")),
             card_body(uiOutput("risk"))),
        card(class = "chart-card", card_header(div(class = "card-title-text", "Pay and contribution rates")),
             card_body(uiOutput("pay"))))
    ),

    nav_panel("Quarters and ledger",
      chart_card("Quarter by quarter",
                 "Built from data/transactions.csv and the daily price file.",
                 DT::dataTableOutput("quarter_table", height = "auto")),
      layout_columns(col_widths = c(5, 7),
        chart_card("Balance by money source", "At each quarter end. Fees are spread in proportion to holdings.",
                   plotlyOutput("source_plot", height = "360px")),
        chart_card("Every transaction", "Units are amount divided by that day's price.",
                   DT::dataTableOutput("ledger_table")))
    ),

    nav_panel("Projection",
      layout_sidebar(
        fillable = FALSE,
        sidebar = sidebar(width = 310, open = "desktop",
          numericInput("p_income", "Annual pay ($)", value = round(default_pay, -2), min = 0, step = 1000),
          sliderInput("p_raise", "Annual raise (%)", 0, 10, projection$raise, step = 0.25, ticks = FALSE),
          sliderInput("p_pct", "Your contribution (% of pay)", 0, 50, round(default_rate, 1), step = 0.5, ticks = FALSE),
          sliderInput("p_step", "Raise your rate each year by (points)", 0, 3, projection$rate_step, step = 0.25, ticks = FALSE),
          sliderInput("p_cap", "Stop raising it at (%)", 0, 50, projection$rate_cap, step = 1, ticks = FALSE),
          sliderInput("p_ret", "Expected annual return (%)", 0, 12, projection$expected_return, step = 0.25, ticks = FALSE),
          sliderInput("p_spread", "Show a range of +/- (points)", 0, 4, projection$return_range, step = 0.5, ticks = FALSE),
          sliderInput("p_infl", "Inflation (%)", 0, 6, projection$inflation, step = 0.25, ticks = FALSE),
          numericInput("p_year", "Retirement year", projection$retirement_year,
                       min = as.integer(format(last_nav_date, "%Y")) + 1, max = 2100),
          accordion(open = FALSE,
            accordion_panel("Plan details",
              numericInput("p_ne", "Employer non-elective (% of pay)", plan$nonelective * 100, min = 0, step = 0.5),
              numericInput("p_mr", "Employer match per dollar", plan$match_rate, min = 0, step = 0.25),
              numericInput("p_mc", "Employer match, up to (% of pay)", plan$match_cap * 100, min = 0, step = 0.5),
              numericInput("p_limit", "IRS employee limit this year ($)", projection$irs_limit, min = 0, step = 500),
              numericInput("p_limg", "IRS limit growth (%/yr)", projection$irs_limit_growth, min = 0, step = 0.5),
              numericInput("p_fee", "Plan fee ($/month)", plan$monthly_fee, min = 0, step = 0.25)))
        ),
        uiOutput("proj_tiles"),
        chart_card("Projected balance",
                   "Stacked bands use your expected return. Dashed lines show the low and high returns.",
                   plotlyOutput("proj_plot", height = "400px")),
        chart_card("What different contribution rates would do",
                   "Same pay, raises, and return as the settings on the left.",
                   DT::dataTableOutput("scenario_table", height = "auto")),
        div(class = "caveat",
            "This is arithmetic on the assumptions above, not a forecast or advice. Real returns vary year to year, and a target-date fund shifts toward bonds as its target year nears.")
      )
    ),

    nav_panel("Method and checks",
      div(class = "method",
        if (is.null(D$balances)) {
          card(class = "chart-card", card_body(
            div(class = "card-title-text", "Does this tie out?"),
            div(class = "card-sub", "Optional: add data/balances.csv with balances from your statements (date, balance) and this tab compares them with the rebuilt history.")))
        } else {
          chart_card("Does this tie out?",
                     "The rebuilt history compared with data/balances.csv.",
                     DT::dataTableOutput("recon_table", height = "auto"))
        },
        card(class = "chart-card", card_body(uiOutput("method")))
      )
    )
  )
}
