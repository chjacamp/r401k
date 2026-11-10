# Input files in data/ --------------------------------------------------------
#
# transactions.csv (required). Columns:
#   date     YYYY-MM-DD the money was actually priced (the "effective date")
#   type     contribution, rollover, fee, withdrawal, dividend, or transfer
#   amount   dollars; negative for withdrawals and normal fees
#   source   who funded a contribution or rollover. Anything starting with
#            "Employer" counts as employer money.
#   holding  optional: fund (default) or cash. Dividends default to cash.
#
# A transfer moves money between cash and the fund; positive means into the fund.
#
# balances.csv (optional, used only for the checks tab). Columns: date, balance,
# and optionally units, unit_price, reported_return.

TX_TYPES <- c("contribution", "rollover", "fee", "withdrawal", "dividend", "transfer")

read_transactions <- function(data_dir = "data") {
  path <- file.path(data_dir, "transactions.csv")
  if (!file.exists(path)) {
    stop("Missing ", path, ". See the README for the expected columns.", call. = FALSE)
  }
  tx <- read.csv(path, stringsAsFactors = FALSE, colClasses = "character", na.strings = c("", "NA"))
  absent <- setdiff(c("date", "type", "amount"), names(tx))
  if (length(absent)) {
    stop(path, " is missing column(s): ", paste(absent, collapse = ", "), call. = FALSE)
  }
  if (is.null(tx$source))  tx$source  <- NA_character_
  if (is.null(tx$holding)) tx$holding <- NA_character_

  reject <- function(mask, problem) {
    if (any(mask)) {
      stop(sprintf("%s: %s on line(s) %s", path, problem,
                   paste(head(which(mask) + 1, 10), collapse = ", ")), call. = FALSE)
    }
  }
  tx$date   <- as.Date(tx$date, format = "%Y-%m-%d")
  tx$amount <- suppressWarnings(as.numeric(gsub("[$,]", "", tx$amount)))
  tx$type   <- tolower(trimws(tx$type))
  reject(is.na(tx$date), "date isn't YYYY-MM-DD")
  reject(is.na(tx$amount), "amount isn't a number")
  reject(!tx$type %in% TX_TYPES, paste("type must be one of", paste(TX_TYPES, collapse = ", ")))
  reject(tx$type %in% c("contribution", "rollover") & tx$amount <= 0,
         "contributions and rollovers must be positive")
  reject(tx$type == "withdrawal" & tx$amount >= 0, "withdrawals must be negative")

  tx$holding <- tolower(trimws(tx$holding))
  blank <- is.na(tx$holding)
  tx$holding[blank] <- ifelse(tx$type[blank] == "dividend", "cash", "fund")
  reject(!tx$holding %in% c("fund", "cash"), "holding must be fund or cash")

  money_in <- tx$type %in% c("contribution", "rollover")
  tx$source <- trimws(tx$source)
  tx$source[money_in & is.na(tx$source)] <- "Employee"
  tx$source[!money_in] <- NA_character_
  tx$who <- ifelse(grepl("^employer", tx$source, ignore.case = TRUE), "employer", "employee")

  tx[order(tx$date), c("date", "type", "amount", "source", "holding", "who")]
}

read_balances <- function(data_dir = "data") {
  path <- file.path(data_dir, "balances.csv")
  if (!file.exists(path)) return(NULL)
  b <- read.csv(path, stringsAsFactors = FALSE)
  if (!all(c("date", "balance") %in% names(b))) {
    stop(path, " needs at least columns named date and balance.", call. = FALSE)
  }
  b$date <- as.Date(b$date)
  for (col in c("units", "unit_price", "reported_return")) {
    if (is.null(b[[col]])) b[[col]] <- NA_real_
  }
  b[order(b$date), c("date", "balance", "units", "unit_price", "reported_return")]
}
