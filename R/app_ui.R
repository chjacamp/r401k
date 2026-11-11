# Dashboard UI for a data folder. Everything it shows comes from the context
# built by app_context(), so the app is configured through run_app()'s
# arguments.
app_ui <- function(ctx) {
  p <- dashboard_panels(ctx, NS("dash"))
  page_navbar(
    title = "401(k) performance",
    fillable = FALSE,
    theme = app_theme(),
    header = tagList(tags$head(tags$style(HTML(APP_CSS))), p$header),
    nav_panel("Performance", p$performance),
    nav_panel("Quarters and ledger", p$ledger),
    nav_panel("Projection", p$projection),
    nav_panel("Method and checks", p$method)
  )
}

# The dashboard's pages for one account, with ids from `ns` so that
# dashboard_server() can fill them. The web app builds a fresh set, under a
# new namespace, each time different data is loaded.
dashboard_panels <- function(ctx, ns) {
  D             <- ctx$D
  plan          <- ctx$plan
  projection    <- ctx$projection
  first_date    <- ctx$first_date
  last_nav_date <- ctx$last_nav_date
  data_through  <- ctx$data_through
  default_pay   <- ctx$default_pay
  default_rate  <- ctx$default_rate

  list(
    header = uiOutput(ns("warnings")),

    performance = tagList(
      div(class = "period-row",
          radioButtons(ns("period"), NULL, inline = TRUE, selected = "YTD",
                       choices = c("1M", "3M", "YTD", "1Y", "All", "Custom")),
          conditionalPanel("input.period == 'Custom'", ns = ns,
                           dateRangeInput(ns("custom"), NULL, start = max(first_date, data_through - 365),
                                          end = last_nav_date, min = first_date, max = last_nav_date))),
      uiOutput(ns("narrative")),
      uiOutput(ns("tiles")),
      layout_columns(col_widths = c(8, 4),
        chart_card("Where the change in balance came from",
                   "Bands are money added since the period began. Green above them is what the market added; red cutting into them is what it lost.",
                   plotlyOutput(ns("balance_plot"), height = "340px")),
        chart_card("What changed the balance",
                   "Each source over the selected period.",
                   plotlyOutput(ns("drivers_plot"), height = "340px"))),
      layout_columns(col_widths = c(6, 6),
        chart_card("Fund return vs. balance change",
                   "Balance change is what some 401(k) sites chart. It counts your own deposits as gains.",
                   plotlyOutput(ns("return_plot"), height = "300px")),
        chart_card("Money added and market gain by month",
                   "Rollovers are left out so the monthly scale stays readable.",
                   plotlyOutput(ns("monthly_plot"), height = "300px"))),
      layout_columns(col_widths = c(6, 6),
        card(class = "chart-card", card_header(div(class = "card-title-text", "Risk and cost in this period")),
             card_body(uiOutput(ns("risk")))),
        card(class = "chart-card", card_header(div(class = "card-title-text", "Pay and contribution rates")),
             card_body(uiOutput(ns("pay")))))
    ),

    ledger = tagList(
      chart_card("Quarter by quarter",
                 sprintf("Built from %s and the daily price file.", ctx$tx_label),
                 DT::dataTableOutput(ns("quarter_table"), height = "auto")),
      layout_columns(col_widths = c(5, 7),
        chart_card("Balance by money source",
                   "Estimated at each quarter end: fees, withdrawals, and transfers are spread in proportion to holdings.",
                   plotlyOutput(ns("source_plot"), height = "360px")),
        chart_card("Every transaction", "Units are amount divided by that day's price.",
                   DT::dataTableOutput(ns("ledger_table"))))
    ),

    projection = layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(width = 310, open = "desktop",
        numericInput(ns("p_income"), "Annual pay ($)", value = round(default_pay, -2), min = 0, step = 1000),
        sliderInput(ns("p_raise"), "Annual raise (%)", 0, 10, projection$raise, step = 0.25, ticks = FALSE),
        sliderInput(ns("p_pct"), "Your contribution (% of pay)", 0, 50, round(default_rate, 1), step = 0.5, ticks = FALSE),
        sliderInput(ns("p_step"), "Raise your rate each year by (points)", 0, 3, projection$rate_step, step = 0.25, ticks = FALSE),
        sliderInput(ns("p_cap"), "Stop raising it at (%)", 0, 50, projection$rate_cap, step = 1, ticks = FALSE),
        sliderInput(ns("p_ret"), "Expected annual return (%)", 0, 12, projection$expected_return, step = 0.25, ticks = FALSE),
        sliderInput(ns("p_spread"), "Show a range of +/- (points)", 0, 4, projection$return_range, step = 0.5, ticks = FALSE),
        sliderInput(ns("p_infl"), "Inflation (%)", 0, 6, projection$inflation, step = 0.25, ticks = FALSE),
        numericInput(ns("p_year"), "Retirement year", projection$retirement_year,
                     min = as.integer(format(last_nav_date, "%Y")) + 1, max = 2100),
        accordion(open = FALSE,
          accordion_panel("Plan details",
            numericInput(ns("p_ne"), "Employer non-elective (% of pay)", plan$nonelective * 100, min = 0, step = 0.5),
            numericInput(ns("p_mr"), "Employer match per dollar", plan$match_rate, min = 0, step = 0.25),
            numericInput(ns("p_mc"), "Employer match, up to (% of pay)", plan$match_cap * 100, min = 0, step = 0.5),
            numericInput(ns("p_limit"), "IRS employee limit this year ($)", projection$irs_limit, min = 0, step = 500),
            numericInput(ns("p_limg"), "IRS limit growth (%/yr)", projection$irs_limit_growth, min = 0, step = 0.5),
            numericInput(ns("p_fee"), "Plan fee ($/month)", plan$monthly_fee, min = 0, step = 0.25)))
      ),
      uiOutput(ns("proj_tiles")),
      chart_card("Projected balance",
                 "Stacked bands use your expected return. Dashed lines show the low and high returns.",
                 plotlyOutput(ns("proj_plot"), height = "400px")),
      chart_card("What different contribution rates would do",
                 "Same pay, raises, and return as the settings on the left.",
                 DT::dataTableOutput(ns("scenario_table"), height = "auto")),
      div(class = "caveat",
          "This is arithmetic on the assumptions above, not a forecast or advice. Real returns vary year to year, and a target-date fund shifts toward bonds as its target year nears.")
    ),

    method = div(class = "method",
      if (is.null(D$balances)) {
        card(class = "chart-card", card_body(
          div(class = "card-title-text", "Does this tie out?"),
          div(class = "card-sub", ctx$balances_hint)))
      } else {
        chart_card("Does this tie out?",
                   "The rebuilt history compared with the balances from your statements.",
                   DT::dataTableOutput(ns("recon_table"), height = "auto"))
      },
      card(class = "chart-card", card_body(uiOutput(ns("method"))))
    )
  )
}
