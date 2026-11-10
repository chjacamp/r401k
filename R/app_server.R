# Dashboard server, built from the same context as app_ui().
app_server <- function(ctx) {
  D             <- ctx$D
  plan          <- ctx$plan
  fund_id       <- ctx$fund_id
  fund_name     <- ctx$fund_name
  data_dir      <- ctx$data_dir
  first_date    <- ctx$first_date
  last_nav_date <- ctx$last_nav_date
  data_through  <- ctx$data_through
  default_pay   <- ctx$default_pay
  default_rate  <- ctx$default_rate

  function(input, output, session) {

    range_sel <- reactive({
      end <- last_nav_date
      if (input$period == "Custom") {
        req(input$custom)
        validate(need(input$custom[1] < input$custom[2], "Pick a start date before the end date."))
        return(list(start = max(input$custom[1], first_date), end = min(input$custom[2], end)))
      }
      start <- switch(input$period,
        "1M"  = months_back(end, 1) + 1,
        "3M"  = months_back(end, 3) + 1,
        "YTD" = as.Date(format(end, "%Y-01-01")),
        "1Y"  = months_back(end, 12) + 1,
        "All" = first_date)
      list(start = max(start, first_date), end = end)
    })

    stats <- reactive({
      r <- range_sel()
      s <- window_stats(D, r$start, r$end)
      validate(need(!is.null(s), "No data in that range."))
      s
    })
    win <- reactive({
      r <- range_sel()
      D$daily[D$daily$date >= r$start & D$daily$date <= r$end, ]
    })

    output$narrative <- renderUI({
      s <- stats()
      added <- s$employee + s$employer + s$rollover
      verb <- if (s$end_balance >= s$begin) "rose" else "fell"
      out <- if (s$withdrawals < 0) sprintf(" Withdrawals took %s.", usd(abs(s$withdrawals))) else ""
      tagList(
        div(class = "narrative", sprintf(
          "From %s to %s the balance %s %s. Deposits added %s, the market %s %s, and plan fees took %s.%s",
          long_date(s$base_date), long_date(s$end), verb, usd(abs(s$end_balance - s$begin)), usd(added),
          if (s$market >= 0) "added" else "took away", usd(abs(s$market)), usd(abs(s$fees)), out)),
        if (s$end > data_through) div(class = "caveat", sprintf(
          "Priced through %s. Transactions after %s aren't in the data yet, so later deposits are missing.",
          long_date(last_nav_date), long_date(data_through)))
      )
    })

    output$tiles <- renderUI({
      s <- stats()
      per_year <- function(x) sprintf("%s per year", pct2((1 + x)^(1 / s$years) - 1))
      div(class = "tiles",
        tile("Balance", usd(s$end_balance), sprintf("as of %s", long_date(s$end))),
        tile("Market gain", signed(s$market), "price change on units held, plus cash interest"),
        tile("Deposits", usd(s$employee + s$employer + s$rollover),
             sprintf("You %s, employer %s%s", usd(s$employee), usd(s$employer),
                     if (s$rollover > 0) sprintf(", rollover %s", usd(s$rollover)) else "")),
        tile("Fund return", pct_s(s$fund_return),
             if (s$years >= 1) per_year(s$fund_return) else "the fund's own price change"),
        tile("Your return", pct_s(s$period_return),
             if (!is.na(s$irr) && s$years >= 1) sprintf("%s per year, money-weighted", pct2(s$irr))
             else "money-weighted, after plan fees"),
        tile("Balance change", pct_s(s$naive_change),
             if (is.na(s$naive_change)) "needs a non-zero starting balance" else "includes deposits, so not performance")
      )
    })

    output$balance_plot <- renderPlotly({
      s <- stats()
      lv <- win() |>
        transmute(date, roll = cumsum(rollover), you = cumsum(employee), er = cumsum(employer),
                  change = balance - s$begin, balance) |>
        mutate(y1 = roll, y2 = y1 + you, y3 = y2 + er, gain = change - y3, y4 = pmax(change, y3))
      band <- function(p, y, name, color, value, fill = "tonexty") {
        add_trace(p, data = lv, x = ~date, y = y, type = "scatter", mode = "lines", name = name,
                  line = list(color = color, width = 1.5), fill = fill,
                  fillcolor = wash(color, 0.22), customdata = value,
                  hovertemplate = paste0(name, ": %{customdata:$,.0f}<extra></extra>"))
      }
      has_roll <- s$rollover > 0
      p <- plot_ly()
      if (has_roll) p <- band(p, ~y1, "Rollover", HUE$start, ~roll, "tozeroy")
      p <- p |>
        band(~y2, "Your contributions", HUE$you, ~you, if (has_roll) "tonexty" else "tozeroy") |>
        band(~y3, "Employer contributions", HUE$employer, ~er) |>
        band(~y4, "Market gain", HUE$market, ~gain) |>
        add_trace(data = lv, x = ~date, y = ~change, type = "scatter", mode = "lines",
                  name = "Net change", line = list(color = INK$primary, width = 2),
                  customdata = ~balance,
                  hovertemplate = "Net change: %{y:$,.0f}<br>Balance: %{customdata:$,.0f}<extra></extra>") |>
        style_plot(filename = "change-in-balance")
      if (s$end > data_through && s$base_date < data_through) p <- data_marker(p, data_through)
      p
    })

    output$drivers_plot <- renderPlotly({
      s <- stats()
      d <- tibble(
        label = c("Rollover", "Your contributions", "Employer contributions", "Market gain",
                  "Withdrawals", "Plan fees", "Net change"),
        value = c(s$rollover, s$employee, s$employer, s$market, s$withdrawals, s$fees, s$end_balance - s$begin),
        color = c(HUE$start, HUE$you, HUE$employer, HUE$market, INK$muted, INK$muted, INK$primary)
      ) |> filter(!(label %in% c("Rollover", "Withdrawals") & value == 0))
      pad <- diff(range(c(0, d$value))) * 0.32
      plot_ly(d, y = ~factor(label, levels = rev(label)), x = ~value, type = "bar", orientation = "h",
              marker = list(color = d$color), text = signed(d$value), textposition = "outside",
              textfont = list(color = INK$primary, size = 12), cliponaxis = FALSE,
              hovertemplate = "%{y}: %{x:$,.2f}<extra></extra>") |>
        style_plot(hover = "closest", legend = FALSE, filename = "what-changed-the-balance") |>
        layout(margin = list(l = 138, r = 12, t = 6, b = 28), bargap = 0.6,
               xaxis = list(range = c(min(0, min(d$value)) - pad, max(0, max(d$value)) + pad),
                            tickformat = "$~s", showgrid = TRUE, gridcolor = INK$grid,
                            zeroline = TRUE, zerolinecolor = INK$axis),
               yaxis = list(tickformat = "", showgrid = FALSE, zeroline = FALSE))
    })

    output$return_plot <- renderPlotly({
      s <- stats()
      w <- win()
      base_row <- tibble(date = s$base_date, nav = D$daily$nav[D$daily$date == s$base_date], balance = s$begin)
      show_naive <- s$begin > 0 && s$rollover == 0
      d <- bind_rows(base_row, select(w, date, nav, balance)) |>
        distinct(date, .keep_all = TRUE) |>
        mutate(fund = nav / nav[1] - 1, naive = if (show_naive) balance / s$begin - 1 else NA_real_)
      p <- plot_ly(d, x = ~date) |>
        add_lines(y = ~fund, name = "Fund return", line = list(color = HUE$market, width = 2),
                  hovertemplate = "Fund return: %{y:.2%}<extra></extra>")
      if (show_naive) {
        p |>
          add_lines(y = ~naive, name = "Balance change", line = list(color = INK$secondary, width = 2),
                    hovertemplate = "Balance change: %{y:.2%}<extra></extra>") |>
          style_plot(y_format = ".0%", filename = "fund-return-vs-balance-change")
      } else {
        p |>
          style_plot(y_format = ".0%", legend = FALSE, filename = "fund-return-vs-balance-change") |>
          layout(annotations = list(list(
            x = 0.02, y = 0.98, xref = "paper", yref = "paper", xanchor = "left", showarrow = FALSE,
            text = "Balance change is hidden here because this period includes a rollover or starts from $0.",
            font = list(size = 11, color = INK$muted))))
      }
    })

    output$monthly_plot <- renderPlotly({
      m <- win() |>
        mutate(month = as.Date(format(date, "%Y-%m-15"))) |>
        group_by(month) |>
        summarise(you = sum(employee), er = sum(employer), market = sum(market), .groups = "drop")
      bars <- function(p, y, name, color) {
        add_bars(p, y = y, name = name, marker = list(color = color, line = list(color = INK$surface, width = 1)),
                 hovertemplate = paste0(name, ": %{y:$,.0f}<extra></extra>"))
      }
      plot_ly(m, x = ~month) |>
        bars(~you, "You", HUE$you) |>
        bars(~er, "Employer", HUE$employer) |>
        bars(~market, "Market", HUE$market) |>
        style_plot(filename = "monthly-deposits-and-market") |>
        layout(barmode = "relative", bargap = 0.35,
               xaxis = list(type = "date", tickformat = "%b %y", dtick = if (nrow(m) > 14) "M3" else "M1"))
    })

    output$risk <- renderUI({
      s <- stats()
      w <- win()
      rets <- c(NA, diff(w$nav) / head(w$nav, -1))
      day <- function(i) sprintf("%s (%s)", pct_s(rets[i]), format(w$date[i], "%b %d, %Y"))
      rows <- list(
        "Largest fund drop within the period" = pct2(s$max_drawdown),
        "Fund vs. its high since you joined" = pct2(tail(D$daily$nav, 1) / max(D$daily$nav) - 1),
        "Fund volatility, annualized" = pct2(s$volatility),
        "Best day" = if (nrow(w) > 1) day(which.max(rets)) else "n/a",
        "Worst day" = if (nrow(w) > 1) day(which.min(rets)) else "n/a",
        "Plan fees paid" = usd2(abs(s$fees))
      )
      if (plan$fund_expense_ratio > 0) {
        label <- sprintf("Fund expenses, estimated at %s/yr",
                         scales::percent(plan$fund_expense_ratio, accuracy = 0.001))
        rows[[label]] <- usd2(mean(w$balance) * plan$fund_expense_ratio * s$years)
      }
      stat_list(rows)
    })

    output$pay <- renderUI({
      q <- tail(D$quarters, 1)
      recent <- tail(D$quarters, 4)
      rows <- list(
        "Pay over the last 12 months" = usd(default_pay),
        "Your rate on the latest paycheck" = pct1(default_rate / 100),
        "Your average rate last quarter" = if (is.na(q$employee_pct)) "n/a" else pct1(q$employee_pct),
        "Employer non-elective" = pct1(plan$nonelective),
        "Employer match last quarter" = if (is.na(q$match_pct)) "n/a" else pct1(q$match_pct),
        "Your contributions, last four quarters" = usd(sum(recent$employee)),
        "Employer contributions, last four quarters" = usd(sum(recent$employer))
      )
      tagList(
        stat_list(rows),
        if (plan$nonelective > 0) div(class = "caveat", style = "margin-top:10px",
          "Pay is inferred from the employer's non-elective contribution, so it reflects plan-eligible pay.")
      )
    })

    output$quarter_table <- DT::renderDataTable({
      q <- D$quarters
      tot <- tibble(period = "Total", begin = 0, rollover = sum(q$rollover), employee = sum(q$employee),
                    employer = sum(q$employer), withdrawals = sum(q$withdrawals), market = sum(q$market),
                    fees = sum(q$fees), end_balance = tail(q$end_balance, 1))
      tab <- bind_rows(q, tot) |>
        transmute(Quarter = period, Start = begin, Rollover = rollover, You = employee, Employer = employer,
                  Withdrawals = withdrawals, Market = market, Fees = fees, End = end_balance,
                  `Fund return` = fund_return, `Your return` = money_weighted, Reported = reported_return,
                  `Balance change` = naive_change, `Your rate` = employee_pct)
      if (all(tab$Withdrawals == 0)) tab$Withdrawals <- NULL
      if (all(is.na(tab$Reported))) tab$Reported <- NULL
      if (all(is.na(tab$`Your rate`))) tab$`Your rate` <- NULL

      money <- intersect(c("Start", "Rollover", "You", "Employer", "Withdrawals", "Market", "Fees", "End"), names(tab))
      rates <- intersect(c("Fund return", "Your return", "Reported", "Balance change"), names(tab))
      dt <- DT::datatable(tab, rownames = FALSE, class = "compact stripe",
                          options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE,
                                         columnDefs = list(list(className = "dt-right", targets = seq_len(ncol(tab) - 1))))) |>
        DT::formatCurrency(money, digits = 0) |>
        DT::formatPercentage(rates, digits = 2)
      if ("Your rate" %in% names(tab)) dt <- DT::formatPercentage(dt, "Your rate", digits = 1)
      dt
    })

    output$ledger_table <- DT::renderDataTable({
      tab <- D$ledger |>
        arrange(desc(price_date)) |>
        transmute(Date = date, Type = type, Source = source, Amount = amount, Holding = holding,
                  Price = ifelse(units != 0, nav, NA), Units = ifelse(units != 0, units, NA))
      DT::datatable(tab, rownames = FALSE, class = "compact stripe",
                    options = list(pageLength = 10, dom = "ftip", scrollX = TRUE)) |>
        DT::formatCurrency(c("Amount", "Price"), digits = 2) |>
        DT::formatRound("Units", digits = 3)
    })

    output$source_plot <- renderPlotly({
      preferred <- c("Employee pre-tax" = HUE$you, "Employee Roth" = HUE$you_light,
                     "Employer non-elective" = HUE$employer, "Employer match" = HUE$employer_light)
      present <- unique(D$sources$source)
      extras  <- setdiff(present, names(preferred))
      extra_colors <- setNames(ifelse(grepl("^employer", extras, ignore.case = TRUE), HUE$employer, HUE$you), extras)
      colors <- c(preferred[names(preferred) %in% present], extra_colors)
      s <- D$sources |> mutate(quarter = factor(format(end, "%b %Y"), levels = unique(format(end, "%b %Y"))))
      p <- plot_ly()
      for (k in names(colors)) {
        d <- s[s$source == k, ]
        p <- add_bars(p, data = d, x = ~quarter, y = ~balance, name = k,
                      marker = list(color = unname(colors[k]), line = list(color = INK$surface, width = 1)),
                      hovertemplate = paste0(k, ": %{y:$,.0f}<extra></extra>"))
      }
      p |> style_plot(filename = "balance-by-source") |> layout(barmode = "stack", bargap = 0.45, legend = list(y = 1.18))
    })

    # Projection
    proj_args <- reactive({
      req(input$p_income, input$p_year, input$p_ne, input$p_mr, input$p_mc,
          input$p_limit, input$p_limg, input$p_fee)
      validate(need(input$p_year > as.integer(format(last_nav_date, "%Y")), "Choose a retirement year after this one."))
      list(start_balance = tail(D$daily$balance, 1), from = last_nav_date, to_year = as.integer(input$p_year),
           income = input$p_income, raise = input$p_raise / 100, pct = input$p_pct / 100,
           step = input$p_step / 100, cap = input$p_cap / 100, nonelective = input$p_ne / 100,
           match_rate = input$p_mr, match_cap = input$p_mc / 100, ret = input$p_ret / 100,
           infl = input$p_infl / 100, limit = input$p_limit, limit_growth = input$p_limg / 100, fee = input$p_fee)
    })
    base_run <- reactive(do.call(project, proj_args()))
    low_run  <- reactive(do.call(project, modifyList(proj_args(), list(ret = proj_args()$ret - input$p_spread / 100))))
    high_run <- reactive(do.call(project, modifyList(proj_args(), list(ret = proj_args()$ret + input$p_spread / 100))))

    output$proj_tiles <- renderUI({
      b <- tail(base_run(), 1)
      l <- tail(low_run(), 1)
      h <- tail(high_run(), 1)
      div(class = "tiles",
        tile(sprintf("Balance in %d", b$year), usd(b$balance), sprintf("%s to %s across the range", usd(l$balance), usd(h$balance))),
        tile("In today's dollars", usd(b$today), sprintf("at %s inflation", pct1(input$p_infl / 100))),
        tile("You would add", usd(b$you), "from future paychecks"),
        tile("Employer would add", usd(b$employer), "non-elective plus match"),
        tile("Investment growth", usd(b$growth), sprintf("%s of the final balance", scales::percent(b$growth / b$balance, accuracy = 1)))
      )
    })

    output$proj_plot <- renderPlotly({
      start <- tail(D$daily$balance, 1)
      now <- tibble(year = as.integer(format(last_nav_date, "%Y")), balance = start,
                    you = 0, employer = 0, start = start, growth = 0, today = start)
      b <- bind_rows(now, select(base_run(), year, balance, you, employer, start, growth, today)) |>
        mutate(y1 = start, y2 = y1 + you, y3 = y2 + employer, y4 = y3 + growth)
      edge <- function(run) bind_rows(select(now, year, balance), select(run, year, balance))
      band <- function(p, y, name, color, value, fill = "tonexty") {
        add_trace(p, data = b, x = ~year, y = y, type = "scatter", mode = "lines", name = name,
                  line = list(color = color, width = 1.5), fill = fill, fillcolor = wash(color, 0.22),
                  customdata = value, hovertemplate = paste0(name, ": %{customdata:$,.0f}<extra></extra>"))
      }
      line <- function(p, d, name) {
        add_trace(p, data = d, x = ~year, y = ~balance, type = "scatter", mode = "lines", name = name,
                  line = list(color = INK$secondary, width = 1.5, dash = "dash"),
                  hovertemplate = paste0(name, ": %{y:$,.0f}<extra></extra>"))
      }
      plot_ly() |>
        band(~y1, "Today's balance", HUE$start, ~start, "tozeroy") |>
        band(~y2, "Your future contributions", HUE$you, ~you) |>
        band(~y3, "Employer future contributions", HUE$employer, ~employer) |>
        band(~y4, "Investment growth", HUE$market, ~growth) |>
        line(edge(low_run()), sprintf("At %s", pct1((input$p_ret - input$p_spread) / 100))) |>
        line(edge(high_run()), sprintf("At %s", pct1((input$p_ret + input$p_spread) / 100))) |>
        style_plot(filename = "projected-balance") |>
        layout(legend = list(y = 1.12), xaxis = list(dtick = 5, tickformat = "d"))
    })

    output$scenario_table <- DT::renderDataTable({
      args <- proj_args()
      rates <- sort(unique(c(1, 3, 6, 10, 15, 20, round(args$pct * 100, 1))))
      runs <- bind_rows(lapply(rates / 100, function(r) {
        x <- tail(do.call(project, modifyList(args, list(pct = r))), 1)
        tibble(rate = r, this_year = min(r * args$income, args$limit), you = x$you,
               balance = x$balance, today = x$today)
      }))
      base <- runs$balance[which.min(abs(runs$rate - args$pct))]
      tab <- runs |>
        transmute(`Your rate` = rate, ` ` = ifelse(abs(rate - args$pct) < 1e-9, "your setting", ""),
                  `Your contributions per year` = this_year, `Your total contributions` = you,
                  `Balance at retirement` = balance, `In today's dollars` = today,
                  `Vs. your setting` = balance - base)
      DT::datatable(tab, rownames = FALSE, class = "compact stripe",
                    options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE)) |>
        DT::formatPercentage("Your rate", digits = 1) |>
        DT::formatCurrency(c("Your contributions per year", "Your total contributions",
                             "Balance at retirement", "In today's dollars", "Vs. your setting"), digits = 0)
    })

    output$recon_table <- DT::renderDataTable({
      validate(need(!is.null(D$recon), "Add data/balances.csv (date, balance) to compare against reported balances."))
      tab <- D$recon |>
        transmute(Date = date, `Reported balance` = reported_balance, `Rebuilt balance` = rebuilt_balance,
                  `Balance difference` = balance_diff, `Reported units` = reported_units,
                  `Rebuilt units` = rebuilt_units, `Units difference` = units_diff,
                  `Reported price` = reported_price, `Price file` = file_price)
      if (all(is.na(tab$`Reported units`))) tab <- select(tab, -`Reported units`, -`Rebuilt units`, -`Units difference`)
      if (all(is.na(tab$`Reported price`))) tab <- select(tab, -`Reported price`, -`Price file`)
      dt <- DT::datatable(tab, rownames = FALSE, class = "compact stripe",
                          options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE)) |>
        DT::formatCurrency(c("Reported balance", "Rebuilt balance", "Balance difference"), digits = 2)
      if ("Reported units" %in% names(tab)) {
        dt <- dt |>
          DT::formatRound(c("Reported units", "Rebuilt units"), digits = 3) |>
          DT::formatRound("Units difference", digits = 4)
      }
      if ("Reported price" %in% names(tab)) dt <- DT::formatCurrency(dt, c("Reported price", "Price file"), digits = 2)
      dt
    })

    output$method <- renderUI({
      transfer <- D$ledger[D$ledger$type == "transfer", ]
      tagList(
        h6("What the numbers mean"),
        tags$ul(
          tags$li(tags$b("Fund return"), " is the change in the fund's price. Deposits can't affect it."),
          tags$li(tags$b("Your return"), " is money-weighted (XIRR): it uses the actual dates money arrived, so it reflects your timing, and it is net of plan fees."),
          tags$li(tags$b("Balance change"), " is ending balance divided by starting balance, minus 1. It counts deposits as if they were gains."),
          tags$li(tags$b("Market gain"), " is units held each day times that day's price change, plus interest on any cash position.")
        ),
        h6("Where the data comes from"),
        tags$ul(
          tags$li(sprintf("Fund: %s. Prices are read from %s, through %s.",
                          fund_name, nav_path(data_dir, fund_id), long_date(max(D$nav$date)))),
          tags$li(sprintf("Transactions are read from %s/transactions.csv, through %s. Each deposit buys units at the price on its date, and each fee sells units the same way.",
                          data_dir, long_date(max(D$transactions$date)))),
          if (nrow(transfer)) tags$li(sprintf("Includes %d transfer(s) between cash and the fund, first on %s.",
                                              nrow(transfer), long_date(min(transfer$date)))),
          tags$li(sprintf("After %s, units are held flat and priced daily, so later deposits are missing until you add them.",
                          long_date(data_through)))
        )
      )
    })
  }
}
