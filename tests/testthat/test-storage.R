fixture_bundle <- function() {
  tx <- fixture_tx()
  new_bundle(tx, fixture_nav(),
             balances = data.frame(date = c("2024-01-05", "2024-01-08"), balance = c(316, 321)),
             fund_name = "Test fund", plan = plan_settings(nonelective = 0.05, monthly_fee = 2),
             projection = projection_settings(retirement_year = 2050), tolerance = 0.5)
}

test_that("a bundle survives the trip through JSON", {
  b <- fixture_bundle()
  b2 <- bundle_from_json(bundle_to_json(b))
  expect_equal(b2$fund_name, "Test fund")
  expect_equal(b2$fund_id, "test-fund")
  expect_equal(b2$plan$nonelective, 0.05)
  expect_equal(b2$projection$retirement_year, 2050)
  expect_true(is.na(b2$projection$annual_pay))
  expect_equal(b2$tolerance, 0.5)
  expect_equal(b2$balances$balance, c(316, 321))
  expect_equal(bundle_context(b2)$D$daily$balance, bundle_context(b)$D$daily$balance)
  expect_error(bundle_from_json("{\"format\": \"other\"}"), "isn't an r401k backup")
})

test_that("downloaded files open with load_401k and restore to the same bundle", {
  b <- fixture_bundle()
  dir <- tempfile("r401k-export-")
  files <- write_bundle_files(b, dir)
  expect_setequal(files, c("transactions.csv", "nav_test-fund.csv", "balances.csv",
                           "r401k-backup.json", "open-in-r.R"))
  x <- load_401k(dir)
  expect_equal(tail(x$daily$balance, 1), 321)
  expect_true(all(x$recon$status == "OK"))
  script <- readLines(file.path(dir, "open-in-r.R"))
  expect_true(any(grepl("nonelective = 0.05", script, fixed = TRUE)))
  expect_silent(parse(text = script))

  # Plain CSVs restore with settings from the backup that came with them.
  csvs <- data.frame(name = files[files != "open-in-r.R"], stringsAsFactors = FALSE)
  csvs$datapath <- file.path(dir, csvs$name)
  b2 <- bundle_from_files(csvs)
  expect_equal(b2$fund_id, "test-fund")
  expect_equal(b2$plan$monthly_fee, 2)
  expect_equal(bundle_context(b2)$D$daily$balance, x$daily$balance)

  skip_if_not_installed("zip")
  zipfile <- tempfile(fileext = ".zip")
  write_bundle_zip(b, zipfile)
  b3 <- bundle_from_files(data.frame(name = "r401k-data.zip", datapath = zipfile))
  expect_equal(nrow(b3$transactions), nrow(b$transactions))
  notes <- tempfile(fileext = ".txt")
  writeLines("hello", notes)
  expect_error(bundle_from_files(data.frame(name = "notes.txt", datapath = notes)), "Add transactions.csv")
})

test_that("the demo bundle matches the folder demo", {
  ctx <- bundle_context(demo_bundle(), is_demo = TRUE)
  expect_equal(ctx$D$daily$balance, load_401k(demo_data_dir())$daily$balance)
  expect_match(ctx$source_note, "Your data tab")
})

test_that("the privacy note depends on where the app runs", {
  expect_match(where_data_goes("127.0.0.1"), "stays on this computer")
  expect_match(where_data_goes("example.com"), "sent to it")
})
