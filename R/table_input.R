# A file or paste box that reads any table into text columns -------------------

table_input_ui <- function(id) {
  ns <- NS(id)
  div(class = "table-input",
    fileInput(ns("file"), NULL, accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls"),
              buttonLabel = "Choose file", placeholder = "CSV or Excel file", width = "100%"),
    tags$details(class = "paste-box",
      tags$summary("Or paste rows copied from a spreadsheet or web page"),
      textAreaInput(ns("paste"), NULL, rows = 6, width = "100%",
                    placeholder = "Select the table, including its column names, then copy and paste it here."),
      actionButton(ns("read_paste"), "Read pasted rows", class = "btn-sm btn-outline-primary")),
    uiOutput(ns("sheet_ui")),
    uiOutput(ns("status")),
    conditionalPanel("output.has_source", ns = ns,
      tags$details(class = "header-fix",
        tags$summary("Column names wrong?"),
        numericInput(ns("header"), "The column names are on row", value = 1, min = 1, step = 1,
                     width = "240px")))
  )
}

# Returns a reactive holding the table as a data frame of text, or NULL.
table_input_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    src <- reactiveVal(NULL)
    hdr <- reactiveVal(NULL)
    read_error <- reactiveVal(NULL)

    use_rows <- function(res, name, path = NULL) {
      src(list(rows = res$rows, name = name, sheets = res$sheets, sheet = res$sheet, path = path))
      h <- detect_header_row(res$rows)
      hdr(h)
      updateNumericInput(session, "header", value = h)
      read_error(NULL)
    }

    observeEvent(input$file, {
      f <- input$file
      res <- tryCatch(read_source_rows(f$datapath, f$name), error = function(e) e)
      if (inherits(res, "error")) {
        src(NULL)
        read_error(sprintf("Couldn't read %s: %s", f$name, conditionMessage(res)))
        return()
      }
      use_rows(res, f$name, f$datapath)
    })

    observeEvent(input$read_paste, {
      txt <- input$paste %||% ""
      if (!nzchar(trimws(txt))) {
        read_error("Paste some rows first.")
        return()
      }
      lines <- strsplit(txt, "\r\n|\n|\r")[[1]]
      use_rows(list(rows = split_text_rows(lines), sheets = NULL, sheet = NULL), "the pasted rows")
    })

    observeEvent(input$sheet, {
      s <- src()
      req(s, s$path, !identical(input$sheet, s$sheet))
      res <- tryCatch(read_source_rows(s$path, s$name, sheet = input$sheet), error = function(e) e)
      if (inherits(res, "error")) {
        read_error(conditionMessage(res))
        return()
      }
      use_rows(res, s$name, s$path)
    }, ignoreInit = TRUE)

    observeEvent(input$header, {
      h <- input$header
      if (!is.null(h) && !is.na(h)) hdr(as.integer(h))
    }, ignoreInit = TRUE)

    parsed <- reactive({
      s <- src()
      if (is.null(s)) return(NULL)
      tryCatch(list(df = table_from_rows(s$rows, hdr())),
               error = function(e) list(error = conditionMessage(e)))
    })

    output$has_source <- reactive(!is.null(src()))
    outputOptions(output, "has_source", suspendWhenHidden = FALSE)

    output$sheet_ui <- renderUI({
      s <- src()
      if (is.null(s) || length(s$sheets) < 2) return(NULL)
      selectInput(session$ns("sheet"), "Sheet", s$sheets, selected = s$sheet, width = "320px")
    })

    output$status <- renderUI({
      if (!is.null(read_error())) return(div(class = "msg msg-error", read_error()))
      p <- parsed()
      if (is.null(p)) return(NULL)
      if (!is.null(p$error)) return(div(class = "msg msg-error", p$error))
      div(class = "msg msg-ok", sprintf(
        "Read %s row%s and %d columns from %s. The column names are on row %d.",
        format(nrow(p$df), big.mark = ","), if (nrow(p$df) == 1) "" else "s", ncol(p$df),
        src()$name, attr(p$df, "header_row")))
    })

    reactive({
      p <- parsed()
      if (is.null(p) || !is.null(p$error)) NULL else p$df
    })
  })
}
