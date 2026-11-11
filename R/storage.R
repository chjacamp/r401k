# What the web app keeps: the three tables plus the settings, as one "bundle".
# A bundle is saved in the browser as JSON and downloaded as a zip of the same
# files a data folder holds, so the R package can open it too.

new_bundle <- function(transactions, nav, balances = NULL, fund_name = "My fund",
                       fund_id = fund_slug(fund_name), plan = plan_settings(),
                       projection = projection_settings(), tolerance = 1) {
  list(transactions = transactions, nav = nav, balances = balances, fund_name = fund_name,
       fund_id = fund_id, plan = plan, projection = projection, tolerance = tolerance)
}

# The bundled demo as a bundle.
demo_bundle <- function(demo_dir = demo_data_dir()) {
  fund_id <- detect_fund_id(demo_dir)
  tx <- read.csv(file.path(demo_dir, "transactions.csv"), stringsAsFactors = FALSE,
                 colClasses = "character", na.strings = c("", "NA"))
  tx$amount <- as.numeric(tx$amount)
  new_bundle(tx, read_nav(demo_dir, fund_id), fund_name = default_fund_name(fund_id),
             fund_id = fund_id)
}

# Checks a bundle and rebuilds its account for the dashboard. Errors name the
# step of the import wizard to fix.
bundle_context <- function(b, is_demo = FALSE) {
  tx <- check_transactions(b$transactions, "Transactions", lines = FALSE)
  nav <- check_nav(b$nav, "Prices")
  balances <- if (!is.null(b$balances) && NROW(b$balances)) {
    check_balances(b$balances, "Statement balances", lines = FALSE)
  }
  plan <- modifyList(plan_settings(), b$plan %||% list())
  projection <- modifyList(projection_settings(), b$projection %||% list())
  D <- build_401k(tx, nav, balances, fund_id = b$fund_id %||% "fund", plan = plan,
                  tolerance = b$tolerance %||% 1, warn = FALSE)
  dashboard_context(
    D, fund_name = b$fund_name, plan = plan, projection = projection, is_demo = is_demo,
    source_note = if (is_demo) {
      sprintf("This is the demo: made-up transactions priced against the published daily NAV of the %s. To use your own data, open the Your data tab.",
              b$fund_name)
    } else {
      sprintf("Fund: %s. Imported on the Your data tab.", b$fund_name)
    },
    price_label = "your price file", tx_label = "your transaction history",
    balances_hint = "Optional: add balances from your statements in the Your data tab, and this tab compares them with the rebuilt history. It's a quick way to catch a missing or mistyped transaction."
  )
}

bundle_to_json <- function(b) {
  doc <- list(
    format = "r401k", version = 1L, saved = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    fund_name = b$fund_name, fund_id = b$fund_id, plan = b$plan, projection = b$projection,
    tolerance = b$tolerance,
    transactions = as.data.frame(b$transactions)[, c("date", "type", "amount", "source", "holding")],
    nav = data.frame(date = format(as.Date(b$nav$date)), nav = b$nav$nav),
    balances = if (NROW(b$balances)) as.data.frame(b$balances)[, c("date", "balance")]
  )
  doc$transactions$date <- as.character(doc$transactions$date)
  if (!is.null(doc$balances)) doc$balances$date <- as.character(doc$balances$date)
  as.character(jsonlite::toJSON(doc, dataframe = "columns", auto_unbox = TRUE, digits = NA,
                                na = "null", null = "null"))
}

bundle_from_json <- function(txt) {
  x <- jsonlite::fromJSON(txt, simplifyVector = TRUE)
  if (!identical(x$format, "r401k")) stop("That isn't an r401k backup.", call. = FALSE)
  table <- function(cols, required) {
    if (is.null(cols) || !length(cols)) return(NULL)
    df <- as.data.frame(lapply(cols, function(v) if (is.null(v)) NA else v), stringsAsFactors = FALSE)
    for (col in setdiff(required, names(df))) df[[col]] <- NA_character_
    df
  }
  tx <- table(x$transactions, c("date", "type", "amount", "source", "holding"))
  for (col in c("date", "type", "source", "holding")) tx[[col]] <- as.character(tx[[col]])
  nav <- table(x$nav, c("date", "nav"))
  balances <- table(x$balances, c("date", "balance"))
  if (!is.null(balances)) balances$date <- as.character(balances$date)
  plan <- modifyList(plan_settings(), as.list(x$plan %||% list()))
  projection <- modifyList(projection_settings(), lapply(as.list(x$projection %||% list()),
                                                         function(v) if (is.null(v)) NA else v))
  b <- new_bundle(tx, check_nav(nav, "Prices in the backup"), balances,
                  fund_name = x$fund_name %||% "My fund", fund_id = x$fund_id %||% "fund",
                  plan = plan, projection = projection, tolerance = x$tolerance %||% 1)
  b$saved <- x$saved
  b
}

# Writes the bundle's files into `dir`: the same files run_app() reads, plus
# the settings as a backup the web app can restore, and an R script.
write_bundle_files <- function(b, dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  tx <- as.data.frame(b$transactions)[, c("date", "type", "amount", "source", "holding")]
  write.csv(tx, file.path(dir, "transactions.csv"), row.names = FALSE, na = "")
  write.csv(data.frame(date = format(as.Date(b$nav$date)), nav = b$nav$nav),
            file.path(dir, sprintf("nav_%s.csv", b$fund_id)), row.names = FALSE)
  if (NROW(b$balances)) {
    write.csv(as.data.frame(b$balances)[, c("date", "balance")], file.path(dir, "balances.csv"),
              row.names = FALSE)
  }
  writeLines(bundle_to_json(b), file.path(dir, "r401k-backup.json"))
  writeLines(open_in_r_script(b), file.path(dir, "open-in-r.R"))
  invisible(list.files(dir))
}

open_in_r_script <- function(b) {
  p <- modifyList(plan_settings(), b$plan %||% list())
  num <- function(x) format(x, digits = 10, trim = TRUE)
  c("# Opens these files in the r401k dashboard on your computer.",
    "# Install it once with:",
    "#   install.packages(\"remotes\")",
    "#   remotes::install_github(\"chjacamp/r401k\")",
    "",
    "data_dir <- \"~/my401k\"   # the folder you unzipped these files into",
    "",
    "r401k::run_app(",
    "  data_dir,",
    sprintf("  fund_id = %s,", deparse(b$fund_id)),
    sprintf("  fund_name = %s,", deparse(b$fund_name)),
    sprintf("  plan = r401k::plan_settings(nonelective = %s, match_rate = %s, match_cap = %s,",
            num(p$nonelective), num(p$match_rate), num(p$match_cap)),
    sprintf("                             monthly_fee = %s, fund_expense_ratio = %s),",
            num(p$monthly_fee), num(p$fund_expense_ratio)),
    sprintf("  tolerance = %s", num(b$tolerance %||% 1)),
    ")")
}

write_bundle_zip <- function(b, zipfile) {
  dir <- tempfile("r401k-export-")
  files <- write_bundle_files(b, dir)
  zip::zip(zipfile, files = files, root = dir)
  invisible(zipfile)
}

# A bundle from files someone brings back: a backup (.json), a zip from the
# web app, or the CSV files of a data folder. `files` has columns name and
# datapath, like a Shiny fileInput.
bundle_from_files <- function(files) {
  dir <- tempfile("r401k-restore-")
  dir.create(dir)
  for (i in seq_len(nrow(files))) {
    name <- basename(files$name[i])
    if (grepl("\\.zip$", name, ignore.case = TRUE)) {
      if (!requireNamespace("zip", quietly = TRUE)) stop("Opening zip files needs the zip package.", call. = FALSE)
      zip::unzip(files$datapath[i], exdir = dir, junkpaths = TRUE)
    } else {
      file.copy(files$datapath[i], file.path(dir, name), overwrite = TRUE)
    }
  }
  backups <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
  have_csv <- file.exists(file.path(dir, "transactions.csv"))
  if (length(backups) && !have_csv) {
    return(bundle_from_json(paste(readLines(backups[1], warn = FALSE), collapse = "\n")))
  }
  if (!have_csv) {
    stop("Add transactions.csv and a nav_<fund>.csv price file, or a backup from this app.", call. = FALSE)
  }
  saved <- if (length(backups)) {
    tryCatch(bundle_from_json(paste(readLines(backups[1], warn = FALSE), collapse = "\n")),
             error = function(e) NULL)
  }
  fund_id <- detect_fund_id(dir)
  tx <- read.csv(file.path(dir, "transactions.csv"), stringsAsFactors = FALSE,
                 colClasses = "character", na.strings = c("", "NA"))
  check_transactions(tx, "transactions.csv")
  tx$amount <- parse_money(tx$amount)
  for (col in setdiff(c("source", "holding"), names(tx))) tx[[col]] <- NA_character_
  balances <- if (file.exists(file.path(dir, "balances.csv"))) {
    b <- read.csv(file.path(dir, "balances.csv"), stringsAsFactors = FALSE, colClasses = "character")
    check_balances(b, "balances.csv")
    data.frame(date = b$date, balance = parse_money(b$balance), stringsAsFactors = FALSE)
  }
  new_bundle(tx[, c("date", "type", "amount", "source", "holding")], read_nav(dir, fund_id), balances,
             fund_name = saved$fund_name %||% fund_id, fund_id = fund_id,
             plan = saved$plan %||% plan_settings(), projection = saved$projection %||% projection_settings(),
             tolerance = saved$tolerance %||% 1)
}
