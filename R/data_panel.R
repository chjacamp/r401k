# The web app's "Your data" tab: a start page and a six-step import wizard ----

WIZARD_STEPS <- c(s1 = "Import transactions", s2 = "Check transactions", s3 = "Prices",
                  s4 = "Your plan", s5 = "Statement balances", s6 = "Finish")

data_panel_ui <- function(id) {
  ns <- NS(id)
  div(class = "data-page",
    uiOutput(ns("stepbar")),
    navset_hidden(id = ns("step"),
      nav_panel_hidden("start", start_step_ui(ns)),
      nav_panel_hidden("s1", import_step_ui(ns)),
      nav_panel_hidden("s2", check_step_ui(ns)),
      nav_panel_hidden("s3", prices_step_ui(ns)),
      nav_panel_hidden("s4", plan_step_ui(ns)),
      nav_panel_hidden("s5", balances_step_ui(ns)),
      nav_panel_hidden("s6", finish_step_ui(ns))
    ),
    conditionalPanel("output.step_name != 'start'", ns = ns,
      div(class = "wiz-nav",
        actionButton(ns("back"), "Back", class = "btn-outline-secondary"),
        conditionalPanel("output.step_name != 's6'", ns = ns, style = "display:inline-block",
                         actionButton(ns("next"), "Next", class = "btn-primary"))))
  )
}

start_step_ui <- function(ns) {
  tagList(
    uiOutput(ns("status")),
    div(class = "start-grid",
      card(class = "wiz-card", card_body(
        h5("Import from your plan"),
        p("Use the transaction history from your 401(k) plan's website: a CSV or Excel download, or rows copied from a table on the page. You'll match up the columns, then add your fund's daily prices."),
        actionButton(ns("start_import"), "Start the import", class = "btn-primary"))),
      card(class = "wiz-card", card_body(
        h5("Open files you saved"),
        p("A zip downloaded from this page, or the transactions.csv and nav_<fund>.csv files from an r401k data folder."),
        fileInput(ns("restore"), NULL, multiple = TRUE, accept = c(".zip", ".json", ".csv"),
                  buttonLabel = "Choose files", placeholder = "zip, json, or csv", width = "100%"))),
      card(class = "wiz-card", card_body(
        h5("Type it in"),
        p("Start with an empty table and enter each transaction yourself. Handy for a short history or a quick try."),
        actionButton(ns("start_manual"), "Start with an empty table", class = "btn-outline-primary")))
    ),
    uiOutput(ns("privacy"))
  )
}

import_step_ui <- function(ns) {
  tagList(
    h4("Import your transactions"),
    p(class = "step-help",
      "Plan websites usually offer your transaction history under a name like Activity or History, as a download or as a table you can copy. Include every transaction since the account started if you can. If not, you can add your starting balance on the next step."),
    table_input_ui(ns("tx_src")),
    div(class = "step-alt", actionLink(ns("skip_import"), "Or skip this and type your transactions in")),
    conditionalPanel("output.tx_ready", ns = ns,
      card(class = "wiz-card",
        card_header(div(class = "card-title-text", "Your file"),
                    div(class = "card-sub", "The first rows, as read.")),
        card_body(DT::dataTableOutput(ns("tx_raw_preview")))),
      card(class = "wiz-card",
        card_header(div(class = "card-title-text", "Match the columns")),
        card_body(
          layout_columns(col_widths = c(3, 3, 3, 3),
            selectInput(ns("tx_date"), "Date", choices = NULL),
            selectInput(ns("tx_date_fmt"), "Date format", choices = DATE_FORMAT_CHOICES),
            selectInput(ns("tx_amount"), "Amount", choices = NULL),
            selectizeInput(ns("tx_labels"), "What each row is", choices = NULL, multiple = TRUE,
                           options = list(placeholder = "Type or source columns"))),
          div(class = "field-help",
              "Use the effective date if there's more than one: the day the money was priced. \"What each row is\" takes one or more columns, such as a transaction type and a money source."),
          tags$details(class = "filter-box",
            tags$summary("Your file covers more than one investment?"),
            p(class = "field-help", "The dashboard follows one fund. Keep only the rows for it."),
            layout_columns(col_widths = c(4, 8),
              selectInput(ns("tx_filter_col"), "Investment column", choices = c("Keep every row" = "")),
              selectizeInput(ns("tx_filter_vals"), "Keep rows for", choices = NULL, multiple = TRUE)))
        )),
      card(class = "wiz-card",
        card_header(div(class = "card-title-text", "What each kind of row is"),
                    div(class = "card-sub", "Guessed from the wording. Check each one.")),
        card_body(
          uiOutput(ns("tx_categories")),
          checkboxInput(ns("tx_fix_signs"), "Fix signs: make deposits positive and fees and withdrawals negative when a whole kind of row has them the other way round", TRUE,
                        width = "100%"),
          div(class = "field-help",
              "Dividends and interest are skipped at first, because a total-return price already includes them. Choose \"Interest paid in cash\" only for interest on money held as cash outside the fund."))),
      card(class = "wiz-card",
        card_header(div(class = "card-title-text", "Result")),
        card_body(uiOutput(ns("tx_result")), DT::dataTableOutput(ns("tx_converted_preview"))))
    )
  )
}

check_step_ui <- function(ns) {
  tagList(
    h4("Check your transactions"),
    p(class = "step-help",
      "Double-click a cell to change it, then click anywhere else to keep the change. Problems are listed below the table by row number."),
    div(class = "table-tools",
      actionButton(ns("tx_add"), "Add a row", class = "btn-sm btn-outline-primary"),
      actionButton(ns("tx_add_start"), "Add a starting balance", class = "btn-sm btn-outline-primary"),
      actionButton(ns("tx_delete"), "Delete selected rows", class = "btn-sm btn-outline-danger")),
    DT::dataTableOutput(ns("tx_table")),
    uiOutput(ns("tx_check")),
    tags$details(class = "help-box",
      tags$summary("What the columns mean"),
      tags$ul(
        tags$li(tags$b("Date: "), "YYYY-MM-DD, the day the money was priced (the effective date on a statement)."),
        tags$li(tags$b("Type: "), "contribution, rollover, fee, withdrawal, dividend, or transfer."),
        tags$li(tags$b("Amount: "), "dollars. Negative for withdrawals and normal fees."),
        tags$li(tags$b("Source: "), "who funded a contribution or rollover. Anything starting with \"Employer\" counts as employer money, and \"Employer non-elective\" is used to estimate your pay."),
        tags$li(tags$b("Holding: "), "fund (the default) or cash. A transfer moves money between them; a positive amount moves it into the fund."),
        tags$li(tags$b("Starting balance: "), "if your history starts after the account did, add your balance on the first day as a rollover. The dashboard counts it as money you brought in.")))
  )
}

prices_step_ui <- function(ns) {
  tagList(
    h4("Add your fund's daily prices"),
    p(class = "step-help",
      "Each transaction is priced with your fund's price that day, so the dashboard needs a price for every trading day from your first transaction on. Use a total-return series: one that doesn't drop when the fund pays a distribution. Collective trusts usually qualify as published, because they reinvest internally. For a mutual fund or ETF, use an adjusted close."),
    p(class = "step-help",
      "Fund and plan websites often have a price history you can download or copy. Check the terms of any site you get prices from."),
    textInput(ns("fund_name"), "Fund name", placeholder = "As it appears on your statement", width = "420px"),
    uiOutput(ns("nav_status")),
    table_input_ui(ns("nav_src")),
    conditionalPanel("output.nav_ready", ns = ns,
      card(class = "wiz-card",
        card_header(div(class = "card-title-text", "Match the columns")),
        card_body(
          layout_columns(col_widths = c(4, 4, 4),
            selectInput(ns("nav_date"), "Date", choices = NULL),
            selectInput(ns("nav_date_fmt"), "Date format", choices = DATE_FORMAT_CHOICES),
            selectInput(ns("nav_price"), "Price", choices = NULL)),
          uiOutput(ns("nav_result"))))
    ),
    conditionalPanel("output.nav_any", ns = ns,
      card(class = "wiz-card",
        card_header(div(class = "card-title-text", "Prices"),
                    div(class = "card-sub", "A quick look for jumps that suggest a bad row.")),
        card_body(plotlyOutput(ns("nav_plot"), height = "240px"))))
  )
}

plan_step_ui <- function(ns) {
  tagList(
    h4("Describe your plan"),
    p(class = "step-help",
      "These settings describe your employer's contributions and fees. They estimate your pay and project future balances, and you can try other values on the Projection tab."),
    layout_columns(col_widths = c(6, 6),
      card(class = "wiz-card",
        card_header(div(class = "card-title-text", "Employer contributions")),
        card_body(
          numericInput(ns("ne"), "Non-elective contribution (% of pay)", 3, min = 0, step = 0.5),
          div(class = "field-help", "Paid whether or not you contribute. It's also how the dashboard estimates your pay. Use 0 if your employer doesn't make one."),
          numericInput(ns("mr"), "Match per dollar you put in", 1, min = 0, step = 0.25),
          numericInput(ns("mc"), "Match applies up to (% of pay)", 4, min = 0, step = 0.5))),
      card(class = "wiz-card",
        card_header(div(class = "card-title-text", "Costs and projection")),
        card_body(
          numericInput(ns("fee"), "Plan fee ($ per month)", 0, min = 0, step = 0.25),
          numericInput(ns("er"), "Fund expense ratio (% per year)", 0, min = 0, step = 0.005),
          div(class = "field-help", "For example 0.045 for 0.045%. Leave it at 0 to hide the estimate."),
          numericInput(ns("ret_year"), "Retirement year", 2060, min = 2000, max = 2100, step = 1),
          numericInput(ns("exp_ret"), "Expected annual return for projections (%)", 7, min = 0, step = 0.25)))
    )
  )
}

balances_step_ui <- function(ns) {
  tagList(
    h4("Check against your statements (optional)"),
    p(class = "step-help",
      "Add a few balances from your statements, and the dashboard compares them with the history it rebuilds from your transactions. It's the quickest way to catch a missing or mistyped transaction. Use each statement's ending balance and end date."),
    div(class = "table-tools",
      actionButton(ns("bal_add"), "Add a row", class = "btn-sm btn-outline-primary"),
      actionButton(ns("bal_delete"), "Delete selected rows", class = "btn-sm btn-outline-danger")),
    DT::dataTableOutput(ns("bal_table")),
    uiOutput(ns("bal_check")),
    tags$details(class = "filter-box",
      tags$summary("Import balances from a file or pasted rows instead"),
      table_input_ui(ns("bal_src")),
      conditionalPanel("output.bal_ready", ns = ns,
        layout_columns(col_widths = c(4, 4, 4),
          selectInput(ns("bal_date"), "Date", choices = NULL),
          selectInput(ns("bal_date_fmt"), "Date format", choices = DATE_FORMAT_CHOICES),
          selectInput(ns("bal_value"), "Balance", choices = NULL)),
        uiOutput(ns("bal_result")),
        actionButton(ns("bal_use"), "Replace the table with these balances", class = "btn-sm btn-outline-primary"))),
    numericInput(ns("tol"), "Mark a date \"Off\" when the rebuilt balance misses by more than ($)", 1,
                 min = 0, step = 0.5, width = "420px")
  )
}

finish_step_ui <- function(ns) {
  tagList(
    h4("Finish"),
    uiOutput(ns("finish_summary")),
    checkboxInput(ns("remember"), "Keep this data in this browser, so it's here next time", TRUE,
                  width = "100%"),
    div(class = "finish-actions",
      actionButton(ns("open"), "Open the dashboard", class = "btn-primary btn-lg"),
      downloadButton(ns("download"), "Download your files", class = "btn-outline-primary")),
    p(class = "step-help",
      "The download is a zip of transactions.csv, your price file, balances.csv if you added balances, a backup you can open on this page again, and an R script that opens the same files with the r401k package.")
  )
}

# `current` is a reactive holding what the dashboard shows: list(bundle, demo,
# saved). `actions$open(bundle, ctx, remember)` shows new data, and
# `actions$forget()` removes saved data and returns to the demo.
data_panel_server <- function(id, current, actions, privacy_note) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    step <- reactiveVal("start")
    rv <- reactiveValues(tx = NULL, tx_v = 0, nav = NULL, fund_id = NULL, bal = empty_balances(),
                         bal_v = 0, projection = projection_settings())

    go <- function(s) {
      step(s)
      nav_select("step", s, session = session)
      session$sendCustomMessage("r401k-scroll-top", list())
    }
    output$step_name <- reactive(step())
    outputOptions(output, "step_name", suspendWhenHidden = FALSE)

    output$stepbar <- renderUI({
      s <- step()
      if (s == "start") return(NULL)
      at <- match(s, names(WIZARD_STEPS))
      div(class = "wiz-steps", lapply(seq_along(WIZARD_STEPS), function(i) {
        key <- names(WIZARD_STEPS)[i]
        tags$button(type = "button",
                    class = paste("wiz-step", if (i == at) "active" else if (i < at) "done"),
                    onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("goto"), key),
                    tags$span(class = "num", i), WIZARD_STEPS[[i]])
      }))
    })
    observeEvent(input$goto, go(input$goto))

    observe({
      s <- step()
      label <- switch(s,
        s1 = if (!is.null(tx_raw())) "Use these transactions" else "Next: check transactions",
        s2 = "Next: prices", s3 = "Next: your plan", s4 = "Next: statement balances",
        s5 = if (NROW(rv$bal)) "Next: finish" else "Skip and finish", "Next")
      updateActionButton(session, "next", label = label)
    })

    load_bundle <- function(b) {
      rv$tx <- as_tx_table(b$transactions)
      rv$tx_v <- rv$tx_v + 1
      rv$nav <- b$nav
      rv$fund_id <- b$fund_id
      rv$bal <- as_bal_table(b$balances)
      rv$bal_v <- rv$bal_v + 1
      rv$projection <- modifyList(projection_settings(), b$projection %||% list())
      p <- modifyList(plan_settings(), b$plan %||% list())
      updateTextInput(session, "fund_name", value = b$fund_name %||% "")
      updateNumericInput(session, "ne", value = p$nonelective * 100)
      updateNumericInput(session, "mr", value = p$match_rate)
      updateNumericInput(session, "mc", value = p$match_cap * 100)
      updateNumericInput(session, "fee", value = p$monthly_fee)
      updateNumericInput(session, "er", value = p$fund_expense_ratio * 100)
      updateNumericInput(session, "ret_year", value = rv$projection$retirement_year)
      updateNumericInput(session, "exp_ret", value = rv$projection$expected_return)
      updateNumericInput(session, "tol", value = b$tolerance %||% 1)
    }
    user_bundle <- function() {
      cur <- current()
      if (!is.null(cur) && !cur$demo) cur$bundle
    }

    # Start page ---------------------------------------------------------------

    output$status <- renderUI({
      cur <- current()
      if (is.null(cur)) return(div(class = "status-card", "Loading..."))
      if (cur$demo) {
        return(div(class = "status-card",
          h4("Bring your own data"),
          p("The dashboard is showing the demo: five years of made-up paychecks priced against a real fund's daily prices. Choose how to add your own below.")))
      }
      tx <- cur$bundle$transactions
      saved <- if (!is.null(cur$saved)) {
        sprintf("Saved in this browser on %s.", long_date(as.Date(substr(cur$saved, 1, 10))))
      } else {
        "Not saved in this browser: it will be gone when you close the page."
      }
      div(class = "status-card",
        h4("Your data"),
        p(sprintf("%s transactions from %s to %s, priced with %s. %s",
                  format(nrow(tx), big.mark = ","), long_date(min(as.Date(tx$date))),
                  long_date(max(as.Date(tx$date))), cur$bundle$fund_name, saved)),
        div(class = "status-actions",
          actionButton(ns("edit"), "Edit this data", class = "btn-primary"),
          downloadButton(ns("download_current"), "Download your files", class = "btn-outline-primary"),
          actionButton(ns("forget"), "Remove it and show the demo", class = "btn-outline-danger")))
    })
    output$privacy <- renderUI(div(class = "privacy-note", privacy_note()))

    observeEvent(input$start_import, {
      if (is.null(rv$tx) && !is.null(user_bundle())) load_bundle(user_bundle())
      go("s1")
    })
    start_empty <- function() {
      if (is.null(rv$nav) && !is.null(user_bundle())) load_bundle(user_bundle())
      rv$tx <- as_tx_table(NULL)
      rv$tx <- add_tx_row(rv$tx)
      rv$tx_v <- rv$tx_v + 1
      go("s2")
    }
    observeEvent(input$start_manual, start_empty())
    observeEvent(input$skip_import, start_empty())
    observeEvent(input$edit, {
      load_bundle(user_bundle())
      go("s2")
    })
    observeEvent(input$forget, actions$forget())
    observeEvent(input$restore, {
      b <- tryCatch(bundle_from_files(input$restore), error = function(e) e)
      if (inherits(b, "error")) {
        showNotification(conditionMessage(b), type = "error", duration = 10)
        return()
      }
      load_bundle(b)
      go("s6")
    })
    output$download_current <- downloadHandler(
      filename = function() download_name(),
      content = function(file) write_bundle_download(user_bundle(), file)
    )

    # Step 1: import -------------------------------------------------------------

    tx_raw <- table_input_server("tx_src")
    output$tx_ready <- reactive(!is.null(tx_raw()))
    outputOptions(output, "tx_ready", suspendWhenHidden = FALSE)
    tx_guess <- reactive(guess_tx_columns(req(tx_raw())))

    observeEvent(tx_raw(), {
      df <- tx_raw()
      g <- tx_guess()
      cols <- names(df)
      updateSelectInput(session, "tx_date", choices = cols, selected = na_to(g$date, cols[1]))
      updateSelectInput(session, "tx_date_fmt", selected = "auto")
      updateSelectInput(session, "tx_amount", choices = cols, selected = na_to(g$amount, cols[min(2, length(cols))]))
      updateSelectizeInput(session, "tx_labels", choices = cols, selected = g$labels)
      updateSelectInput(session, "tx_filter_col", choices = c("Keep every row" = "", cols), selected = "")
    })

    tx_map <- reactive({
      df <- req(tx_raw())
      g <- tx_guess()
      cols <- names(df)
      list(date = pick_col(input$tx_date, cols, g$date),
           amount = pick_col(input$tx_amount, cols, g$amount),
           labels = if (length(input$tx_labels)) intersect(input$tx_labels, cols) else g$labels,
           fmt = pick_format(input$tx_date_fmt),
           filter_col = pick_col(input$tx_filter_col, cols, ""))
    })

    observeEvent(input$tx_filter_col, {
      df <- tx_raw()
      col <- input$tx_filter_col
      if (is.null(df) || !nzchar(col) || !col %in% names(df)) {
        updateSelectizeInput(session, "tx_filter_vals", choices = character(), selected = character())
        return()
      }
      counts <- sort(table(trimws(df[[col]])), decreasing = TRUE)
      vals <- names(counts)
      updateSelectizeInput(session, "tx_filter_vals",
                           choices = stats::setNames(vals, sprintf("%s (%d rows)", vals, as.integer(counts))),
                           selected = vals[1])
    })

    tx_filtered <- reactive({
      df <- req(tx_raw())
      m <- tx_map()
      if (nzchar(m$filter_col)) df <- df[trimws(df[[m$filter_col]]) %in% (input$tx_filter_vals %||% character()), , drop = FALSE]
      df
    })

    label_ids <- new.env(parent = emptyenv())
    label_id <- function(label) {
      id <- label_ids[[label]]
      if (is.null(id)) {
        id <- paste0("cat_", length(ls(label_ids, all.names = TRUE)) + 1)
        assign(label, id, envir = label_ids)
      }
      id
    }

    tx_label_info <- reactive({
      df <- tx_filtered()
      m <- tx_map()
      labels <- row_labels(df, m$labels)
      amounts <- if (!is.na(m$amount) && m$amount %in% names(df)) parse_money(df[[m$amount]]) else rep(NA_real_, nrow(df))
      if (!length(labels)) return(data.frame(label = character(), n = integer(), total = numeric()))
      info <- data.frame(label = labels, amount = amounts, stringsAsFactors = FALSE) |>
        group_by(label) |>
        summarise(n = n(), total = sum(amount, na.rm = TRUE), .groups = "drop") |>
        arrange(desc(n))
      as.data.frame(info)
    })

    output$tx_categories <- renderUI({
      info <- tx_label_info()
      if (!nrow(info)) return(div(class = "msg msg-error", "No rows left to sort."))
      if (nrow(info) > 60) {
        return(div(class = "msg msg-error", sprintf(
          "The columns under \"What each row is\" have %d different values. Choose a column that says what kind of transaction each row is, such as a type or activity column.",
          nrow(info))))
      }
      rows <- lapply(seq_len(nrow(info)), function(i) {
        lab <- info$label[i]
        id <- label_id(lab)
        tags$tr(
          tags$td(lab),
          tags$td(class = "num", format(info$n[i], big.mark = ",")),
          tags$td(class = "num", usd2(info$total[i])),
          tags$td(selectInput(ns(id), NULL, TX_CATEGORIES, selectize = FALSE, width = "100%",
                              selected = isolate(input[[id]]) %||% guess_category(lab))))
      })
      tags$table(class = "cat-table",
        tags$thead(tags$tr(tags$th("Rows described as"), tags$th(class = "num", "Rows"),
                           tags$th(class = "num", "Total"), tags$th("Count them as"))),
        tags$tbody(rows))
    })

    tx_categories <- reactive({
      info <- tx_label_info()
      if (!nrow(info) || nrow(info) > 60) return(stats::setNames(rep("skip", nrow(info)), info$label))
      stats::setNames(vapply(info$label, function(lab) input[[label_id(lab)]] %||% guess_category(lab), ""),
                      info$label)
    })

    tx_converted <- reactive({
      df <- req(tx_raw())
      m <- tx_map()
      tryCatch(
        convert_transactions(df, m$date, m$amount, m$labels, tx_categories(), date_format = m$fmt,
                             fix_signs = !isFALSE(input$tx_fix_signs), filter_col = m$filter_col,
                             filter_values = input$tx_filter_vals %||% character()),
        error = function(e) list(error = conditionMessage(e)))
    })

    output$tx_raw_preview <- DT::renderDataTable({
      DT::datatable(utils::head(req(tx_raw()), 6), rownames = FALSE, class = "compact",
                    options = list(dom = "t", ordering = FALSE, scrollX = TRUE))
    }, server = FALSE)

    output$tx_result <- renderUI({
      conv <- tx_converted()
      if (!is.null(conv$error)) return(div(class = "msg msg-error", conv$error))
      tx <- conv$tx
      problem <- tryCatch({ check_transactions(tx, "These transactions", lines = FALSE); NULL },
                          error = function(e) conditionMessage(e))
      tagList(
        if (nrow(tx)) {
          div(class = if (is.null(problem)) "msg msg-ok" else "msg msg-warn",
              sprintf("%s transactions from %s to %s, with dates read as %s.",
                      format(nrow(tx), big.mark = ","), long_date(as.Date(min(tx$date))),
                      long_date(as.Date(max(tx$date))), conv$date_format),
              tx_totals_list(tx))
        } else {
          div(class = "msg msg-error", "No transactions left. Check the columns and the kinds of rows above.")
        },
        if (!is.null(problem) && nrow(tx)) div(class = "msg msg-warn", problem, " You can fix this on the next step."),
        if (length(conv$notes)) tags$ul(class = "notes", lapply(conv$notes, tags$li))
      )
    })

    output$tx_converted_preview <- DT::renderDataTable({
      conv <- tx_converted()
      req(is.null(conv$error), nrow(conv$tx) > 0)
      DT::datatable(conv$tx, rownames = FALSE, class = "compact stripe",
                    options = list(pageLength = 8, dom = "tip", ordering = FALSE, scrollX = TRUE))
    }, server = FALSE)

    conv_stamp <- reactiveVal(0)
    used_stamp <- reactiveVal(-1)
    observeEvent(tx_converted(), conv_stamp(conv_stamp() + 1))

    # Step 2: check --------------------------------------------------------------

    output$tx_table <- DT::renderDataTable({
      rv$tx_v
      tx <- isolate(rv$tx)
      req(!is.null(tx))
      DT::datatable(tx, rownames = TRUE, selection = "multiple", class = "compact stripe",
                    colnames = c("Row", "Date", "Type", "Amount", "Source", "Holding"),
                    editable = list(target = "cell", disable = list(columns = 0)),
                    options = list(pageLength = 25, dom = "ftip", scrollX = TRUE, ordering = FALSE))
    }, server = FALSE)

    observeEvent(input$tx_table_cell_edit, {
      rv$tx <- apply_edits(rv$tx, input$tx_table_cell_edit, numeric_cols = "amount")
    })
    observeEvent(input$tx_add, {
      rv$tx <- add_tx_row(rv$tx)
      rv$tx_v <- rv$tx_v + 1
    })
    observeEvent(input$tx_add_start, {
      rv$tx <- add_tx_row(rv$tx, type = "rollover", source = "Starting balance", first = TRUE)
      rv$tx_v <- rv$tx_v + 1
    })
    observeEvent(input$tx_delete, {
      sel <- input$tx_table_rows_selected
      if (!length(sel)) {
        showNotification("Select rows first by clicking them.", type = "message")
        return()
      }
      rv$tx <- reset_rows(rv$tx[-sel, , drop = FALSE])
      rv$tx_v <- rv$tx_v + 1
    })

    tx_check <- reactive({
      tx <- rv$tx
      if (is.null(tx) || !nrow(tx)) return(list(error = "Add at least one transaction."))
      tryCatch(list(ok = check_transactions(tx, "Transactions", lines = FALSE)),
               error = function(e) list(error = conditionMessage(e)))
    })

    output$tx_check <- renderUI({
      chk <- tx_check()
      if (!is.null(chk$error)) return(div(class = "msg msg-error", chk$error))
      tx <- chk$ok
      dup <- duplicated(tx[, c("date", "type", "amount", "source")])
      tagList(
        div(class = "msg msg-ok",
            sprintf("%s transactions from %s to %s.", format(nrow(tx), big.mark = ","),
                    long_date(min(tx$date)), long_date(max(tx$date))),
            tx_totals_list(tx)),
        if (any(dup)) div(class = "msg msg-warn", sprintf(
          "%d row(s) repeat another row's date, type, amount, and source. That can be right, such as two paychecks priced the same day, but check that nothing was imported twice.",
          sum(dup)))
      )
    })

    # Step 3: prices -------------------------------------------------------------

    nav_raw <- table_input_server("nav_src")
    output$nav_ready <- reactive(!is.null(nav_raw()))
    outputOptions(output, "nav_ready", suspendWhenHidden = FALSE)
    nav_guess <- reactive(guess_price_columns(req(nav_raw())))

    observeEvent(nav_raw(), {
      cols <- names(nav_raw())
      g <- nav_guess()
      updateSelectInput(session, "nav_date", choices = cols, selected = na_to(g$date, cols[1]))
      updateSelectInput(session, "nav_date_fmt", selected = "auto")
      updateSelectInput(session, "nav_price", choices = cols, selected = na_to(g$price, cols[min(2, length(cols))]))
    })

    nav_converted <- reactive({
      df <- req(nav_raw())
      g <- nav_guess()
      cols <- names(df)
      tryCatch(convert_prices(df, pick_col(input$nav_date, cols, g$date),
                              pick_col(input$nav_price, cols, g$price), pick_format(input$nav_date_fmt)),
               error = function(e) list(error = conditionMessage(e)))
    })
    nav_stamp <- reactiveVal(0)
    nav_used <- reactiveVal(-1)
    observeEvent(nav_converted(), nav_stamp(nav_stamp() + 1))

    # The prices that Next would use: a new import if there is one, else the
    # prices already chosen.
    nav_candidate <- reactive({
      if (!is.null(nav_raw()) && nav_used() != nav_stamp()) {
        conv <- nav_converted()
        if (is.null(conv$error)) return(conv$nav)
      }
      rv$nav
    })
    output$nav_any <- reactive(!is.null(nav_candidate()))
    outputOptions(output, "nav_any", suspendWhenHidden = FALSE)

    output$nav_status <- renderUI({
      if (is.null(rv$nav) || (!is.null(nav_raw()) && nav_used() != nav_stamp())) return(NULL)
      div(class = "msg msg-ok", sprintf(
        "Using %s prices from %s to %s. Add a file below to replace them.",
        format(nrow(rv$nav), big.mark = ","), long_date(min(rv$nav$date)), long_date(max(rv$nav$date))),
        price_coverage(rv$nav, tx_check()$ok))
    })

    output$nav_result <- renderUI({
      conv <- nav_converted()
      if (!is.null(conv$error)) return(div(class = "msg msg-error", conv$error))
      nav <- conv$nav
      tagList(
        div(class = "msg msg-ok", sprintf("%s prices from %s to %s, with dates read as %s.",
                                          format(nrow(nav), big.mark = ","), long_date(min(nav$date)),
                                          long_date(max(nav$date)), conv$date_format)),
        price_coverage(nav, tx_check()$ok),
        if (length(conv$notes)) tags$ul(class = "notes", lapply(conv$notes, tags$li))
      )
    })

    output$nav_plot <- renderPlotly({
      nav <- req(nav_candidate())
      tx <- tx_check()$ok
      if (!is.null(tx)) nav <- nav[nav$date >= min(tx$date) - 7, ]
      plot_ly(nav, x = ~date, y = ~nav, type = "scatter", mode = "lines",
              line = list(color = HUE$market, width = 1.5),
              hovertemplate = "%{x|%b %d, %Y}: %{y:$,.2f}<extra></extra>") |>
        style_plot(y_format = "$,.2f", hover = "closest", legend = FALSE, filename = "prices")
    })

    # Step 5: balances -----------------------------------------------------------

    output$bal_table <- DT::renderDataTable({
      rv$bal_v
      bal <- isolate(rv$bal)
      DT::datatable(bal, rownames = TRUE, selection = "multiple", class = "compact stripe",
                    colnames = c("Row", "Date", "Balance"),
                    editable = list(target = "cell", disable = list(columns = 0)),
                    options = list(pageLength = 10, dom = "tip", scrollX = FALSE, ordering = FALSE,
                                   language = list(emptyTable = "No balances yet. Add a row, or import them below.")))
    }, server = FALSE)
    observeEvent(input$bal_table_cell_edit, {
      rv$bal <- apply_edits(rv$bal, input$bal_table_cell_edit, numeric_cols = "balance")
    })
    observeEvent(input$bal_add, {
      bal <- rv$bal
      last <- tx_check()$ok
      default_date <- if (!is.null(last)) format(max(last$date)) else format(Sys.Date())
      rv$bal <- reset_rows(rbind(data.frame(date = default_date, balance = NA_real_, stringsAsFactors = FALSE), bal))
      rv$bal_v <- rv$bal_v + 1
    })
    observeEvent(input$bal_delete, {
      sel <- input$bal_table_rows_selected
      if (!length(sel)) {
        showNotification("Select rows first by clicking them.", type = "message")
        return()
      }
      rv$bal <- reset_rows(rv$bal[-sel, , drop = FALSE])
      rv$bal_v <- rv$bal_v + 1
    })

    bal_raw <- table_input_server("bal_src")
    output$bal_ready <- reactive(!is.null(bal_raw()))
    outputOptions(output, "bal_ready", suspendWhenHidden = FALSE)
    bal_guess <- reactive(guess_balance_columns(req(bal_raw())))
    observeEvent(bal_raw(), {
      cols <- names(bal_raw())
      g <- bal_guess()
      updateSelectInput(session, "bal_date", choices = cols, selected = na_to(g$date, cols[1]))
      updateSelectInput(session, "bal_date_fmt", selected = "auto")
      updateSelectInput(session, "bal_value", choices = cols, selected = na_to(g$balance, cols[min(2, length(cols))]))
    })
    bal_converted <- reactive({
      df <- req(bal_raw())
      g <- bal_guess()
      cols <- names(df)
      tryCatch(convert_balances(df, pick_col(input$bal_date, cols, g$date),
                                pick_col(input$bal_value, cols, g$balance), pick_format(input$bal_date_fmt)),
               error = function(e) list(error = conditionMessage(e)))
    })
    output$bal_result <- renderUI({
      conv <- bal_converted()
      if (!is.null(conv$error)) return(div(class = "msg msg-error", conv$error))
      tagList(div(class = "msg msg-ok", sprintf("%d balance(s) read.", nrow(conv$balances))),
              if (length(conv$notes)) tags$ul(class = "notes", lapply(conv$notes, tags$li)))
    })
    observeEvent(input$bal_use, {
      conv <- bal_converted()
      if (!is.null(conv$error)) {
        showNotification(conv$error, type = "error")
        return()
      }
      rv$bal <- as_bal_table(conv$balances)
      rv$bal_v <- rv$bal_v + 1
    })

    bal_check <- reactive({
      if (!NROW(rv$bal)) return(list(ok = NULL))
      tryCatch(list(ok = check_balances(rv$bal, "Statement balances", lines = FALSE)),
               error = function(e) list(error = conditionMessage(e)))
    })
    output$bal_check <- renderUI({
      chk <- bal_check()
      if (!is.null(chk$error)) div(class = "msg msg-error", chk$error)
    })

    # Step 6: finish -------------------------------------------------------------

    wizard_bundle <- reactive({
      name <- trimws(input$fund_name %||% "")
      if (!nzchar(name)) name <- "My fund"
      new_bundle(
        transactions = rv$tx, nav = rv$nav, balances = if (NROW(rv$bal)) rv$bal,
        fund_name = name, fund_id = rv$fund_id %||% fund_slug(name),
        plan = plan_settings(nonelective = num_or(input$ne, 0) / 100, match_rate = num_or(input$mr, 0),
                             match_cap = num_or(input$mc, 0) / 100, monthly_fee = num_or(input$fee, 0),
                             fund_expense_ratio = num_or(input$er, 0) / 100),
        projection = modifyList(rv$projection, list(retirement_year = num_or(input$ret_year, 2060),
                                                    expected_return = num_or(input$exp_ret, 7))),
        tolerance = num_or(input$tol, 1)
      )
    })

    finish <- reactive({
      req(step() == "s6")
      b <- wizard_bundle()
      if (is.null(b$transactions) || !NROW(b$transactions)) return(list(error = "Add your transactions first.", step = "s1"))
      if (is.null(b$nav)) return(list(error = "Add your fund's daily prices first.", step = "s3"))
      tryCatch(list(bundle = b, ctx = bundle_context(b)),
               error = function(e) list(error = conditionMessage(e), step = error_step(conditionMessage(e))))
    })

    output$finish_summary <- renderUI({
      f <- finish()
      if (!is.null(f$error)) {
        return(div(class = "msg msg-error", f$error, " ",
                   actionLink(ns("fix"), sprintf("Go to %s", tolower(WIZARD_STEPS[[f$step]])))))
      }
      D <- f$ctx$D
      last <- tail(D$daily, 1)
      recon <- D$recon
      tagList(
        div(class = "msg msg-ok",
            sprintf("Ready: %s transactions from %s to %s, priced with %s. Rebuilt balance on %s: %s.",
                    format(nrow(D$transactions), big.mark = ","), long_date(min(D$transactions$date)),
                    long_date(max(D$transactions$date)), f$bundle$fund_name, long_date(last$date),
                    usd2(last$balance))),
        if (!is.null(recon)) {
          off <- sum(recon$status == "Off")
          div(class = if (off) "msg msg-warn" else "msg msg-ok",
              if (off) sprintf("The rebuilt balance misses %d of your %d statement balance(s) by more than %s. A transaction may be missing or mistyped; the Method and checks tab lists each date.",
                               off, nrow(recon), usd2(f$bundle$tolerance))
              else sprintf("The rebuilt balance matches all %d statement balance(s) within %s.",
                           nrow(recon), usd2(f$bundle$tolerance)))
        },
        if (length(other <- grep("statement balance", D$warnings, value = TRUE, invert = TRUE))) {
          div(class = "msg msg-warn", tags$b("Check your data"), tags$ul(lapply(other, tags$li)))
        }
      )
    })
    observeEvent(input$fix, go(finish()$step))

    observeEvent(input$open, {
      f <- finish()
      if (!is.null(f$error)) {
        showNotification(f$error, type = "error")
        return()
      }
      actions$open(f$bundle, f$ctx, isTRUE(input$remember))
      go("start")
    })

    output$download <- downloadHandler(
      filename = function() download_name(),
      content = function(file) write_bundle_download(wizard_bundle(), file)
    )

    # Back and Next ----------------------------------------------------------------

    observeEvent(input$back, {
      at <- match(step(), names(WIZARD_STEPS))
      go(if (is.na(at) || at == 1) "start" else names(WIZARD_STEPS)[at - 1])
    })

    observeEvent(input[["next"]], {
      blocked <- function(msg) showNotification(msg, type = "error", duration = 8)
      switch(step(),
        s1 = {
          if (!is.null(tx_raw())) {
            conv <- tx_converted()
            if (!is.null(conv$error)) return(blocked(conv$error))
            if (!nrow(conv$tx)) return(blocked("No transactions left to use. Check the columns and the kinds of rows."))
            if (used_stamp() != conv_stamp()) {
              rv$tx <- as_tx_table(conv$tx)
              rv$tx_v <- rv$tx_v + 1
              used_stamp(conv_stamp())
            }
          } else if (is.null(rv$tx)) {
            return(blocked("Choose a file or paste rows first, or type your transactions in."))
          }
          go("s2")
        },
        s2 = {
          chk <- tx_check()
          if (!is.null(chk$error)) return(blocked(chk$error))
          go("s3")
        },
        s3 = {
          nav <- nav_candidate()
          if (is.null(nav)) {
            conv <- if (!is.null(nav_raw())) nav_converted()
            return(blocked(conv$error %||% "Add your fund's daily prices to continue."))
          }
          gap <- price_coverage_error(nav, tx_check()$ok)
          if (!is.null(gap)) return(blocked(gap))
          if (!identical(nav, rv$nav)) {
            rv$nav <- nav
            rv$fund_id <- NULL
            nav_used(nav_stamp())
          }
          go("s4")
        },
        s4 = go("s5"),
        s5 = {
          chk <- bal_check()
          if (!is.null(chk$error)) return(blocked(chk$error))
          go("s6")
        }
      )
    })
  })
}

# Helpers for the data tab ---------------------------------------------------------

empty_balances <- function() data.frame(date = character(), balance = numeric(), stringsAsFactors = FALSE)

as_tx_table <- function(tx) {
  if (is.null(tx) || !NROW(tx)) {
    return(data.frame(date = character(), type = character(), amount = numeric(),
                      source = character(), holding = character(), stringsAsFactors = FALSE))
  }
  tx <- as.data.frame(tx, stringsAsFactors = FALSE)
  col <- function(name) if (is.null(tx[[name]])) rep(NA_character_, nrow(tx)) else as.character(tx[[name]])
  reset_rows(data.frame(date = col("date"), type = col("type"), amount = parse_money(tx$amount),
                        source = col("source"), holding = col("holding"), stringsAsFactors = FALSE))
}

as_bal_table <- function(b) {
  if (is.null(b) || !NROW(b)) return(empty_balances())
  b <- as.data.frame(b, stringsAsFactors = FALSE)
  reset_rows(data.frame(date = as.character(b$date), balance = parse_money(b$balance),
                        stringsAsFactors = FALSE))
}

add_tx_row <- function(tx, type = "contribution", source = "Employee pre-tax", first = FALSE) {
  dates <- suppressWarnings(as.Date(tx$date, format = "%Y-%m-%d"))
  date <- if (any(!is.na(dates))) format(if (first) min(dates, na.rm = TRUE) else max(dates, na.rm = TRUE)) else format(Sys.Date())
  row <- data.frame(date = date, type = type, amount = NA_real_, source = source, holding = "fund",
                    stringsAsFactors = FALSE)
  reset_rows(rbind(row, tx))
}

reset_rows <- function(df) {
  rownames(df) <- NULL
  df
}

# Applies DataTables cell edits. Column 0 is the row number, so data columns
# start at 1.
apply_edits <- function(df, info, numeric_cols = character()) {
  for (k in seq_len(nrow(info))) {
    i <- info$row[k]
    j <- info$col[k]
    if (j < 1 || j > ncol(df) || i < 1 || i > nrow(df)) next
    col <- names(df)[j]
    value <- as.character(info$value[k])
    df[i, col] <- if (col %in% numeric_cols) parse_money(value) else trimws(value)
  }
  df
}

tx_totals_list <- function(tx) {
  amt <- as.numeric(tx$amount)
  who <- ifelse(grepl("^employer", tx$source %||% "", ignore.case = TRUE), "employer", "employee")
  part <- function(label, mask) if (any(mask, na.rm = TRUE)) sprintf("%s %s", label, usd2(sum(amt[mask], na.rm = TRUE)))
  items <- c(
    part("You:", tx$type == "contribution" & who == "employee"),
    part("Employer:", tx$type == "contribution" & who == "employer"),
    part("Rollovers:", tx$type == "rollover"),
    part("Fees:", tx$type == "fee"),
    part("Withdrawals:", tx$type == "withdrawal"),
    part("Interest:", tx$type == "dividend"),
    part("Transfers:", tx$type == "transfer")
  )
  if (length(items)) div(class = "totals", paste(items, collapse = paste0(" ", MIDDOT, " ")))
}

# Whether the prices cover the transactions, as the ledger requires.
price_coverage_error <- function(nav, tx) {
  if (is.null(tx) || !nrow(tx)) return(NULL)
  if (max(tx$date) > max(nav$date)) {
    return(sprintf("The prices end %s, but your transactions run through %s. Add prices through at least %s.",
                   long_date(max(nav$date)), long_date(max(tx$date)), long_date(max(tx$date))))
  }
  if (min(tx$date) < min(nav$date) - 7) {
    return(sprintf("The prices start %s, but your first transaction is %s. Add prices from %s or earlier.",
                   long_date(min(nav$date)), long_date(min(tx$date)), long_date(min(tx$date))))
  }
  NULL
}

price_coverage <- function(nav, tx) {
  if (is.null(tx)) return(div(class = "field-help", "Once your transactions check out, this compares the dates."))
  err <- price_coverage_error(nav, tx)
  if (!is.null(err)) return(div(class = "msg msg-error", err))
  gaps <- price_gaps(nav, min(tx$date))
  tagList(
    div(class = "field-help", sprintf("Covers your transactions, %s to %s.",
                                      long_date(min(tx$date)), long_date(max(tx$date)))),
    if (nrow(gaps)) div(class = "msg msg-warn", sprintf(
      "%d gap(s) longer than a normal market closure, such as %s to %s. Transactions in a gap are priced on the next date with a price.",
      nrow(gaps), long_date(gaps$after[1]), long_date(gaps$before[1])))
  )
}

error_step <- function(msg) {
  if (grepl("^Transactions", msg)) "s2"
  else if (grepl("^Prices|price", msg)) "s3"
  else if (grepl("^Statement", msg)) "s5"
  else "s2"
}

pick_col <- function(value, cols, fallback) {
  if (length(value) == 1 && !is.na(value) && value %in% cols) value else fallback
}

pick_format <- function(value) {
  if (is.null(value) || identical(value, "auto") || !value %in% names(DATE_FORMATS)) NULL else value
}

na_to <- function(x, y) if (length(x) != 1 || is.na(x)) y else x

num_or <- function(x, default) if (is.null(x) || length(x) != 1 || is.na(x)) default else as.numeric(x)

download_name <- function() {
  if (requireNamespace("zip", quietly = TRUE)) "r401k-data.zip" else "r401k-backup.json"
}

write_bundle_download <- function(b, file) {
  if (requireNamespace("zip", quietly = TRUE)) write_bundle_zip(b, file) else writeLines(bundle_to_json(b), file)
}
