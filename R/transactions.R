TX_TYPES <- c("contribution", "rollover", "fee", "withdrawal", "dividend", "transfer")

#' Read your transactions
#'
#' Reads and checks `<data_dir>/transactions.csv`, the only input besides a
#' price file. Its columns:
#'
#' * `date`: `YYYY-MM-DD`, the day the money was priced (the "effective date"
#'   on a statement).
#' * `type`: `contribution`, `rollover`, `fee`, `withdrawal`, `dividend`, or
#'   `transfer`.
#' * `amount`: dollars; negative for withdrawals and normal fees.
#' * `source`: who funded a contribution or rollover. Anything starting with
#'   "Employer" counts as employer money.
#' * `holding`: optional, `fund` (default) or `cash`. Dividends default to `cash`.
#'
#' A `transfer` moves money between cash and the fund; a positive amount moves
#' it into the fund. Use `dividend` for interest on a cash position.
#'
#' @inheritParams read_nav
#' @return A data frame sorted by date, with a `who` column added
#'   (`"employee"` or `"employer"`).
#' @export
#' @examples
#' head(read_transactions(demo_data_dir()))
read_transactions <- function(data_dir) {
  path <- file.path(data_dir, "transactions.csv")
  if (!file.exists(path)) {
    stop("Missing ", path, ". See ?read_transactions for the expected columns.", call. = FALSE)
  }
  tx <- read.csv(path, stringsAsFactors = FALSE, colClasses = "character", na.strings = c("", "NA"))
  check_transactions(tx, path)
}

# Checks transactions held in a data frame and returns them ready to use.
# `where` names them in error messages. Problems are reported by file line,
# counting the header, or by table row when `lines = FALSE`.
check_transactions <- function(tx, where = "transactions", lines = TRUE) {
  tx <- as.data.frame(tx, stringsAsFactors = FALSE)
  absent <- setdiff(c("date", "type", "amount"), names(tx))
  if (length(absent)) {
    stop(where, " is missing column(s): ", paste(absent, collapse = ", "), call. = FALSE)
  }
  if (!nrow(tx)) stop(where, " has no transactions.", call. = FALSE)
  if (is.null(tx$source))  tx$source  <- NA_character_
  if (is.null(tx$holding)) tx$holding <- NA_character_
  reject <- row_rejecter(where, lines)

  tx$date   <- as.Date(as.character(tx$date), format = "%Y-%m-%d")
  tx$amount <- suppressWarnings(as.numeric(gsub("[$,]", "", tx$amount)))
  tx$type   <- tolower(trimws(tx$type))
  reject(is.na(tx$date), "date isn't YYYY-MM-DD")
  reject(is.na(tx$amount), "amount isn't a number")
  reject(!tx$type %in% TX_TYPES, paste("type must be one of", paste(TX_TYPES, collapse = ", ")))
  reject(tx$type %in% c("contribution", "rollover") & tx$amount <= 0,
         "contributions and rollovers must be positive")
  reject(tx$type == "withdrawal" & tx$amount >= 0, "withdrawals must be negative")

  tx$holding <- blank_to_na(tolower(trimws(tx$holding)))
  blank <- is.na(tx$holding)
  tx$holding[blank] <- ifelse(tx$type[blank] == "dividend", "cash", "fund")
  reject(!tx$holding %in% c("fund", "cash"), "holding must be fund or cash")

  money_in <- tx$type %in% c("contribution", "rollover")
  tx$source <- blank_to_na(trimws(tx$source))
  tx$source[money_in & is.na(tx$source)] <- "Employee"
  tx$source[!money_in] <- NA_character_
  tx$who <- ifelse(grepl("^employer", tx$source, ignore.case = TRUE), "employer", "employee")

  tx[order(tx$date), c("date", "type", "amount", "source", "holding", "who")]
}

# Optional balances copied from statements, used only by the checks tab.
read_balances <- function(data_dir) {
  path <- file.path(data_dir, "balances.csv")
  if (!file.exists(path)) return(NULL)
  check_balances(read.csv(path, stringsAsFactors = FALSE, colClasses = "character"), path)
}

check_balances <- function(b, where = "balances", lines = TRUE) {
  b <- as.data.frame(b, stringsAsFactors = FALSE)
  if (!all(c("date", "balance") %in% names(b))) {
    stop(where, " needs at least columns named date and balance.", call. = FALSE)
  }
  if (!nrow(b)) return(NULL)
  reject <- row_rejecter(where, lines)
  b$date <- as.Date(as.character(b$date), format = "%Y-%m-%d")
  reject(is.na(b$date), "date isn't YYYY-MM-DD")
  as_number <- function(x) suppressWarnings(as.numeric(gsub("[$,%]", "", as.character(x))))
  b$balance <- as_number(b$balance)
  reject(is.na(b$balance), "balance isn't a number")
  for (col in c("units", "unit_price", "reported_return")) {
    b[[col]] <- if (is.null(b[[col]])) NA_real_ else as_number(b[[col]])
  }
  b[order(b$date), c("date", "balance", "units", "unit_price", "reported_return")]
}

# Stops with the first few offending lines or rows when `mask` is TRUE anywhere.
row_rejecter <- function(where, lines = TRUE) {
  function(mask, problem) {
    mask <- mask %in% TRUE
    if (any(mask)) {
      stop(sprintf("%s: %s on %s %s", where, problem, if (lines) "line(s)" else "row(s)",
                   paste(head(which(mask) + if (lines) 1 else 0, 10), collapse = ", ")),
           call. = FALSE)
    }
  }
}

blank_to_na <- function(x) {
  x <- as.character(x)
  x[!is.na(x) & !nzchar(x)] <- NA_character_
  x
}
