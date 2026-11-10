test_that("read_transactions rejects malformed rows with the line number", {
  dir <- tempfile("r401k-")
  dir.create(dir)
  bad <- function(lines) {
    writeLines(lines, file.path(dir, "transactions.csv"))
    tryCatch({ read_transactions(dir); "accepted" }, error = conditionMessage)
  }
  expect_match(bad(c("date,type,amount", "01/15/2024,contribution,100")), "YYYY-MM-DD on line\\(s\\) 2")
  expect_match(bad(c("date,type,amount", "2024-01-15,bonus,100")), "type must be one of")
  expect_match(bad(c("date,type,amount", "2024-01-15,withdrawal,100")), "withdrawals must be negative")
  expect_match(bad(c("date,amount", "2024-01-15,100")), "missing column")
  expect_equal(bad(c("date,type,amount", "2024-01-15,contribution,100")), "accepted")
})

test_that("use_example_data copies the demo and won't overwrite by accident", {
  dir <- file.path(tempfile("r401k-"), "my401k")
  path <- use_example_data(dir)
  expect_setequal(list.files(path), c("nav_1685.csv", "transactions.csv"))
  expect_error(use_example_data(dir), "overwrite = TRUE")
  expect_silent(suppressMessages(use_example_data(dir, overwrite = TRUE)))
  expect_equal(nrow(load_401k(path)$transactions), nrow(load_401k(demo_data_dir())$transactions))
})

test_that("the r401k.data_dir option sets the default folder", {
  dir <- suppressMessages(use_example_data(file.path(tempfile("r401k-"), "mine")))
  old <- options(r401k.data_dir = NULL)
  on.exit(options(old))
  expect_equal(default_data_dir(), demo_data_dir())
  options(r401k.data_dir = dir)
  expect_equal(default_data_dir(), dir)
})

test_that("make_demo_data reproduces the bundled demo", {
  dir <- tempfile("r401k-")
  dir.create(dir)
  file.copy(file.path(demo_data_dir(), "nav_1685.csv"), dir)
  suppressMessages(make_demo_data(dir))
  expect_identical(readLines(file.path(dir, "transactions.csv")),
                   readLines(file.path(demo_data_dir(), "transactions.csv")))
})
