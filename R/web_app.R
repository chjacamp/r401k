#' Run the web version of the dashboard
#'
#' The dashboard plus a "Your data" tab that imports your plan's transaction
#' history and your fund's prices, with no files to arrange and no R code to
#' write. It reads CSV and Excel exports or rows copied from a web page, lets
#' you match up the columns, and checks the result against your statements.
#'
#' This is the same app that is published as a web page, where it runs
#' entirely in your browser through WebAssembly. Data you import is kept in the
#' browser rather than in a folder. Download it from the Your data tab to keep
#' a copy, or to open it with [run_app()].
#'
#' @param demo_dir Folder with the data shown before you import your own.
#' @param ... Options passed to [shiny::shinyApp()], such as `port`. The browser
#'   keeps saved data separately for each address, so pass the same `port`
#'   each time to find it again.
#' @return A Shiny app object. It runs when printed, so calling
#'   `run_web_app()` at the console opens it.
#' @export
#' @examples
#' if (interactive()) {
#'   run_web_app()
#' }
run_web_app <- function(demo_dir = demo_data_dir(), ...) {
  shiny::shinyApp(ui = web_ui(), server = web_server(demo_dir), options = list(...))
}

web_ui <- function() {
  page_navbar(
    id = "main",
    title = "401(k) performance",
    window_title = "r401k: how much came from you, and how much from the market",
    selected = "performance",
    fillable = FALSE,
    theme = app_theme(),
    header = tagList(
      tags$head(tags$style(HTML(APP_CSS)), tags$style(HTML(WEB_CSS))),
      htmltools::attachDependencies(tags$span(), widget_dependencies()),
      uiOutput("dash_header")
    ),
    footer = tags$script(HTML(WEB_JS)),
    nav_panel("Performance", value = "performance", uiOutput("dash_performance")),
    nav_panel("Quarters and ledger", value = "ledger", uiOutput("dash_ledger")),
    nav_panel("Projection", value = "projection", uiOutput("dash_projection")),
    nav_panel("Method and checks", value = "method", uiOutput("dash_method")),
    nav_spacer(),
    nav_panel("Your data", value = "data", data_panel_ui("data"))
  )
}

# The chart and table libraries, loaded with the page. The dashboard is drawn
# inside dynamic UI, and in the browser build a chart could otherwise try to
# draw before the plotting library finished loading and stay blank. Shiny
# supplies jQuery itself.
widget_dependencies <- function() {
  deps <- c(
    htmltools::findDependencies(htmltools::as.tags(plot_ly(x = 1, y = 1, type = "scatter", mode = "lines"))),
    htmltools::findDependencies(htmltools::as.tags(DT::datatable(data.frame(x = 1))))
  )
  deps[!vapply(deps, function(d) identical(d$name, "jquery"), TRUE)]
}

web_server <- function(demo_dir) {
  # Built once, when the app starts, and shared by every visitor.
  demo <- demo_bundle(demo_dir)
  demo_ctx <- bundle_context(demo, is_demo = TRUE)

  function(input, output, session) {
    current <- reactiveVal(NULL)
    shown <- 0

    # Each account gets its own copy of the dashboard's server, under a new
    # namespace, so nothing from the previous account carries over.
    show <- function(bundle, ctx, demo, saved = NULL) {
      shown <<- shown + 1
      id <- paste0("d", shown)
      dashboard_server(id, ctx)
      current(list(bundle = bundle, ctx = ctx, demo = demo, saved = saved, id = id))
    }
    show_demo <- function() show(demo, demo_ctx, TRUE)

    panels <- reactive({
      cur <- req(current())
      dashboard_panels(cur$ctx, NS(cur$id))
    })
    loading <- div(class = "loading", "Loading...")
    output$dash_header <- renderUI({
      cur <- current()
      if (is.null(cur) || identical(input$main, "data")) return(NULL)
      tagList(
        if (cur$demo) div(class = "demo-banner",
          "You're looking at made-up demo data.",
          actionLink("to_data", "Use your own data")),
        panels()$header
      )
    })
    output$dash_performance <- renderUI(if (is.null(current())) loading else panels()$performance)
    output$dash_ledger      <- renderUI(if (is.null(current())) loading else panels()$ledger)
    output$dash_projection  <- renderUI(if (is.null(current())) loading else panels()$projection)
    output$dash_method      <- renderUI(if (is.null(current())) loading else panels()$method)
    observeEvent(input$to_data, nav_select("main", "data", session = session))

    # The browser sends whatever it saved last time as soon as it connects.
    observeEvent(input$stored, {
      txt <- input$stored
      if (is.character(txt) && nzchar(txt)) {
        res <- tryCatch({
          b <- bundle_from_json(txt)
          list(bundle = b, ctx = bundle_context(b))
        }, error = function(e) e)
        if (!inherits(res, "error")) {
          show(res$bundle, res$ctx, FALSE, saved = res$bundle$saved)
          return()
        }
        showNotification(paste("Couldn't open the data saved in this browser.", conditionMessage(res),
                               "Showing the demo instead."), type = "warning", duration = NULL)
      }
      if (is.null(current())) show_demo()
    }, once = TRUE)

    # In case the browser never answers, show the demo after a few seconds.
    waited <- FALSE
    fallback <- observe({
      if (waited) {
        if (is.null(isolate(current()))) show_demo()
        fallback$destroy()
      } else {
        waited <<- TRUE
        invalidateLater(6000)
      }
    })

    observeEvent(input$store_result, {
      res <- input$store_result
      if (isTRUE(res$ok)) {
        cur <- current()
        if (!is.null(cur) && !cur$demo) {
          cur$saved <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
          current(cur)
        }
      } else {
        showNotification(paste("Couldn't save in this browser:", res$error %||% "storage is unavailable.",
                               "Download your files to keep a copy."), type = "warning", duration = NULL)
      }
    })

    actions <- list(
      open = function(bundle, ctx, remember) {
        show(bundle, ctx, FALSE)
        if (remember) {
          session$sendCustomMessage("r401k-store", list(value = bundle_to_json(bundle)))
        } else {
          session$sendCustomMessage("r401k-forget", list())
        }
        nav_select("main", "performance", session = session)
      },
      forget = function() {
        session$sendCustomMessage("r401k-forget", list())
        show_demo()
        showNotification("Removed your data from this browser.", type = "message")
      }
    )

    privacy_note <- reactive(where_data_goes(session$clientData$url_hostname))
    data_panel_server("data", current, actions, privacy_note)
  }
}

# Says honestly where imported files go, which depends on how the app is running.
where_data_goes <- function(host) {
  if (grepl("emscripten", R.version$platform, fixed = TRUE)) {
    "Your data stays on this computer. This page runs the dashboard's R code inside your browser, so the files you add are read here and never uploaded, and what you save is kept in this browser only."
  } else if (isTRUE(host %in% c("127.0.0.1", "localhost", "::1", "[::1]"))) {
    "Your data stays on this computer. The app is running in R here, and what you save is kept in this browser only."
  } else {
    sprintf("This copy of the app runs on a server (%s), so files you add are sent to it. What you save is kept in this browser.",
            host %||% "unknown")
  }
}

WEB_JS <- "
(function () {
  var KEY = 'r401k-data';
  function send(name, value) { Shiny.setInputValue(name, value, {priority: 'event'}); }
  $(document).on('shiny:connected', function () {
    var saved = '';
    try { saved = window.localStorage.getItem(KEY) || ''; } catch (e) {}
    send('stored', saved);
  });
  Shiny.addCustomMessageHandler('r401k-store', function (msg) {
    try {
      window.localStorage.setItem(KEY, msg.value);
      send('store_result', {ok: true});
    } catch (e) {
      send('store_result', {ok: false, error: String((e && e.message) || e)});
    }
  });
  Shiny.addCustomMessageHandler('r401k-forget', function (msg) {
    try { window.localStorage.removeItem(KEY); } catch (e) {}
  });
  Shiny.addCustomMessageHandler('r401k-scroll-top', function (msg) {
    window.scrollTo({top: 0, behavior: 'smooth'});
  });
})();
"
