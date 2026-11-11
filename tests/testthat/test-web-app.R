test_that("the web app opens saved data, or the demo when there is none", {
  saved <- bundle_to_json(new_bundle(fixture_tx(), fixture_nav(), fund_name = "Test fund"))
  shiny::testServer(web_server(demo_data_dir()), {
    session$setInputs(stored = saved)
    expect_false(current()$demo)
    expect_equal(current()$bundle$fund_name, "Test fund")
  })
  shiny::testServer(web_server(demo_data_dir()), {
    session$setInputs(stored = "")
    expect_true(current()$demo)
  })
  shiny::testServer(web_server(demo_data_dir()), {
    expect_warning(session$setInputs(stored = "{not json"), NA)
    expect_true(current()$demo)
  })
})

test_that("the import step turns an uploaded export into transactions", {
  csv <- tempfile(fileext = ".csv")
  writeLines(c("Account history", "",
               "Date,Description,Amount",
               "01/02/2024,Employee Deferral,$100.00",
               "01/02/2024,Employer Match,$50.00",
               "01/05/2024,Recordkeeping Fee,$9.00"), csv)
  opened <- NULL
  shiny::testServer(data_panel_server, args = list(
    current = reactiveVal(NULL),
    actions = list(open = function(bundle, ctx, remember) opened <<- bundle, forget = function() NULL),
    privacy_note = reactive("note")
  ), {
    session$setInputs(start_import = 1)
    session$setInputs(`tx_src-file` = data.frame(name = "history.csv", datapath = csv))
    conv <- tx_converted()
    expect_null(conv$error)
    expect_equal(conv$tx$type, c("contribution", "contribution", "fee"))
    expect_equal(conv$tx$amount, c(100, 50, -9))
    expect_equal(conv$tx$source[1:2], c("Employee pre-tax", "Employer match"))

    session$setInputs(`next` = 1)
    expect_equal(step(), "s2")
    expect_equal(nrow(rv$tx), 3)
    expect_null(tx_check()$error)
  })
})
