# Reading the tables people bring to the import wizard: CSV or Excel exports,
# or rows copied from a spreadsheet or a web page. Cells stay text until the
# person has said which column is which, so nothing is guessed silently.

# Characters built from code points, so the source stays ASCII.
BOM <- intToUtf8(0xFEFF)                                   # byte-order mark
MINUS <- intToUtf8(0x2212)                                 # minus sign
MIDDOT <- intToUtf8(0xB7)                                  # middle dot
OTHER_MONEY_CHARS <- intToUtf8(c(0xA0, 0x20AC, 0xA3), multiple = TRUE) |>
  paste(collapse = "")                                     # no-break space, euro, pound

# Rows of cells, one character vector per line of the file, with "" for empty
# cells. Excel files read the named sheet, or the biggest one.
read_source_rows <- function(path, name = basename(path), sheet = NULL) {
  ext <- tolower(sub("^.*\\.", "", name))
  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Reading Excel files needs the readxl package. Save the sheet as CSV, or install readxl.",
           call. = FALSE)
    }
    sheets <- readxl::excel_sheets(path)
    read_sheet <- function(s) {
      suppressMessages(readxl::read_excel(path, sheet = s, col_names = FALSE, col_types = "list",
                                          .name_repair = "minimal"))
    }
    if (is.null(sheet) || !sheet %in% sheets) {
      # Exports often open with a summary sheet, so start with the biggest one.
      all <- lapply(sheets, read_sheet)
      pick <- which.max(vapply(all, nrow, 0L))
      return(list(rows = excel_cells_to_rows(all[[pick]]), sheets = sheets, sheet = sheets[pick]))
    }
    list(rows = excel_cells_to_rows(read_sheet(sheet)), sheets = sheets, sheet = sheet)
  }
  list(rows = split_text_rows(read_text_lines(path)), sheets = NULL, sheet = NULL)
}

read_text_lines <- function(path) {
  x <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (length(x) && any(!validUTF8(x))) x <- iconv(x, "latin1", "UTF-8")
  if (length(x)) x[1] <- sub(paste0("^", BOM), "", x[1])
  x
}

excel_cells_to_rows <- function(cells) {
  if (!nrow(cells)) return(list())
  as_text <- function(v) {
    if (is.null(v) || length(v) == 0 || (length(v) == 1 && is.na(v))) return("")
    if (inherits(v, "POSIXt")) return(format(v, "%Y-%m-%d"))
    if (inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
    if (is.numeric(v)) return(format(v, digits = 15, scientific = FALSE, trim = TRUE))
    trimws(as.character(v))
  }
  lapply(seq_len(nrow(cells)), function(i) {
    vapply(cells, function(col) as_text(col[[i]]), "", USE.NAMES = FALSE)
  })
}

# Splits text lines into cells. The delimiter is whichever of tab, comma,
# semicolon, or bar splits the most lines into the same number of cells, so
# a tab-separated paste with "$1,234.56" amounts still reads as tabs.
split_text_rows <- function(lines) {
  lines <- sub("\r$", "", lines)
  if (!length(lines)) return(list())
  delim <- detect_delimiter(lines)
  if (is.na(delim)) return(as.list(trimws(lines)))
  n <- count_fields(lines, delim, skip_blank = FALSE)
  width <- max(c(1L, n), na.rm = TRUE)
  m <- tryCatch(
    utils::read.table(text = lines, sep = delim, quote = "\"", header = FALSE,
                      colClasses = "character", col.names = paste0("V", seq_len(width)),
                      fill = TRUE, comment.char = "", strip.white = TRUE,
                      na.strings = character(), blank.lines.skip = FALSE, skipNul = TRUE),
    error = function(e) NULL
  )
  if (is.null(m)) return(lapply(strsplit(lines, delim, fixed = TRUE), trimws))
  m <- as.matrix(m)
  m[is.na(m)] <- ""
  lapply(seq_len(nrow(m)), function(i) unname(m[i, ]))
}

count_fields <- function(lines, delim, skip_blank = TRUE) {
  con <- textConnection(lines)
  on.exit(close(con))
  suppressWarnings(utils::count.fields(con, sep = delim, quote = "\"", comment.char = "",
                                       blank.lines.skip = skip_blank))
}

detect_delimiter <- function(lines) {
  sample <- utils::head(lines[nzchar(trimws(lines))], 200)
  if (!length(sample)) return(NA_character_)
  candidates <- c("\t", ",", ";", "|")
  score <- vapply(candidates, function(d) {
    n <- count_fields(sample, d)
    n <- n[!is.na(n)]
    m <- most_common(n[n >= 2])
    if (is.na(m)) 0 else sum(n == m) + m / 1000
  }, 0)
  if (all(score == 0)) NA_character_ else candidates[which.max(score)]
}

most_common <- function(x) {
  if (!length(x)) return(NA_integer_)
  tab <- table(x)
  as.integer(names(tab)[which.max(tab)])
}

# Number of cells up to the last non-empty one.
row_width <- function(row) {
  filled <- which(nzchar(row))
  if (length(filled)) max(filled) else 0L
}

# The first row as wide as the table's typical row, which skips title lines
# and account details that exports often put above the column names.
detect_header_row <- function(rows) {
  if (!length(rows)) return(1L)
  w <- vapply(rows, row_width, 0L)
  typical <- most_common(w[w >= 2])
  if (is.na(typical)) return(which(w > 0)[1] %||% 1L)
  which(w >= typical)[1]
}

# A data frame of text from rows of cells, using row `header` for column
# names (detected when NULL). Blank rows and empty columns are dropped.
table_from_rows <- function(rows, header = NULL) {
  if (!length(rows)) stop("The file is empty.", call. = FALSE)
  if (is.null(header) || is.na(header)) header <- detect_header_row(rows)
  header <- as.integer(header)
  if (header < 1 || header >= length(rows)) {
    stop("There are no rows below the column names. Check the row the column names are on.",
         call. = FALSE)
  }
  body <- rows[(header + 1):length(rows)]
  body <- body[vapply(body, row_width, 0L) > 0]
  if (!length(body)) stop("There are no rows below the column names.", call. = FALSE)
  width <- max(row_width(rows[[header]]), vapply(body, row_width, 0L))
  pad <- function(r) {
    r <- r[seq_len(min(length(r), width))]
    c(r, rep("", width - length(r)))
  }
  m <- do.call(rbind, lapply(body, pad))
  if (is.null(dim(m))) m <- matrix(m, nrow = 1)
  names_raw <- trimws(pad(rows[[header]]))
  keep <- nzchar(names_raw) | colSums(m != "") > 0
  names_raw[!nzchar(names_raw)] <- paste("Column", which(!nzchar(names_raw)))
  df <- as.data.frame(m[, keep, drop = FALSE], stringsAsFactors = FALSE)
  names(df) <- make.unique(names_raw[keep], sep = " ")
  attr(df, "header_row") <- header
  df
}

# Dates ------------------------------------------------------------------------

DATE_FORMATS <- list(
  "2024-01-31"   = list(fmt = "%Y-%m-%d", re = "^\\d{4}-\\d{1,2}-\\d{1,2}$"),
  "01/31/2024"   = list(fmt = "%m/%d/%Y", re = "^\\d{1,2}/\\d{1,2}/\\d{4}$"),
  "31/01/2024"   = list(fmt = "%d/%m/%Y", re = "^\\d{1,2}/\\d{1,2}/\\d{4}$"),
  "01/31/24"     = list(fmt = "%m/%d/%y", re = "^\\d{1,2}/\\d{1,2}/\\d{2}$"),
  "31/01/24"     = list(fmt = "%d/%m/%y", re = "^\\d{1,2}/\\d{1,2}/\\d{2}$"),
  "2024/01/31"   = list(fmt = "%Y/%m/%d", re = "^\\d{4}/\\d{1,2}/\\d{1,2}$"),
  "01-31-2024"   = list(fmt = "%m-%d-%Y", re = "^\\d{1,2}-\\d{1,2}-\\d{4}$"),
  "31.01.2024"   = list(fmt = "%d.%m.%Y", re = "^\\d{1,2}\\.\\d{1,2}\\.\\d{4}$"),
  "31-Jan-2024"  = list(fmt = "%d-%b-%Y", re = "^\\d{1,2}-[A-Za-z]{3,9}-\\d{4}$"),
  "31-Jan-24"    = list(fmt = "%d-%b-%y", re = "^\\d{1,2}-[A-Za-z]{3,9}-\\d{2}$"),
  "Jan 31, 2024" = list(fmt = "%b %d %Y", re = "^[A-Za-z]{3,9} \\d{1,2} \\d{4}$", words = TRUE),
  "31 Jan 2024"  = list(fmt = "%d %b %Y", re = "^\\d{1,2} [A-Za-z]{3,9} \\d{4}$", words = TRUE),
  "20240131"     = list(fmt = "%Y%m%d", re = "^\\d{8}$"),
  "Excel serial number" = list(fmt = "excel", re = "^\\d{5}(\\.0+)?$")
)

DATE_FORMAT_CHOICES <- c("Detect automatically" = "auto",
                         stats::setNames(names(DATE_FORMATS), names(DATE_FORMATS)))

# Removes a time of day, so "2024-01-31 00:00:00" and "1/31/2024 4:00 PM" read
# as dates.
clean_date_text <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x)] <- ""
  sub("[T ]+\\d{1,2}:\\d{2}(:\\d{2}(\\.\\d+)?)?( ?[AaPp]\\.?[Mm]\\.?)?( ?(Z|[+-]\\d{2}:?\\d{2}|[A-Z]{2,4}))?$",
      "", x)
}

parse_dates <- function(x, format) {
  spec <- DATE_FORMATS[[format]]
  if (is.null(spec)) stop("Unknown date format: ", format, call. = FALSE)
  x <- clean_date_text(x)
  if (isTRUE(spec$words)) x <- gsub("\\s+", " ", trimws(gsub("[,.]", " ", x)))
  ok <- grepl(spec$re, x, perl = TRUE)
  out <- rep(as.Date(NA), length(x))
  if (spec$fmt == "excel") {
    out[ok] <- as.Date(floor(as.numeric(x[ok])), origin = "1899-12-30")
  } else {
    out[ok] <- as.Date(x[ok], format = spec$fmt)
  }
  out[!is.na(out) & (out < as.Date("1950-01-01") | out > as.Date("2100-12-31"))] <- NA
  out
}

# The format that reads the most non-empty values, or NA when none reads at
# least half of them. Month-first wins ties with day-first.
detect_date_format <- function(x) {
  x <- clean_date_text(x)
  x <- x[nzchar(x)]
  if (!length(x)) return(NA_character_)
  x <- utils::head(x, 500)
  share <- vapply(names(DATE_FORMATS), function(f) mean(!is.na(parse_dates(x, f))), 0)
  if (max(share) < 0.5) return(NA_character_)
  names(DATE_FORMATS)[which.max(share)]
}

# Money ------------------------------------------------------------------------

# Reads amounts written like "$1,234.56", "(1,234.56)", "-$5.00", or "5.00-".
# Returns NA for anything else.
parse_money <- function(x) {
  s <- trimws(as.character(x))
  s[is.na(s)] <- ""
  s <- gsub(MINUS, "-", s, fixed = TRUE)
  s <- gsub(paste0("[[:space:]$,", OTHER_MONEY_CHARS, "]|USD"), "", s)
  paren <- grepl("^\\(.*\\)$", s)
  s <- gsub("[()]", "", s)
  trailing <- grepl("^[0-9.]+-$", s)
  s <- sub("-$", "", s)
  s <- sub("^\\+", "", s)
  v <- suppressWarnings(as.numeric(s))
  flip <- (paren | trailing) & !is.na(v)
  v[flip] <- -abs(v[flip])
  v
}

numeric_share <- function(x) {
  x <- x[nzchar(trimws(x))]
  if (!length(x)) return(0)
  mean(!is.na(parse_money(x)))
}

# Guessing columns -------------------------------------------------------------

guess_date_column <- function(df) {
  if (!ncol(df)) return(NA_character_)
  share <- vapply(df, function(col) {
    f <- detect_date_format(col)
    if (is.na(f)) 0 else mean(!is.na(parse_dates(col[nzchar(trimws(col))], f)))
  }, 0)
  ok <- names(df)[share >= 0.6]
  if (!length(ok)) return(NA_character_)
  for (p in c("effective", "trade", "^date$", "date", "as of")) {
    hit <- ok[grepl(p, ok, ignore.case = TRUE)]
    if (length(hit)) return(hit[1])
  }
  ok[which.max(share[ok])]
}

guess_number_column <- function(df, prefer, avoid = NULL, exclude = NULL) {
  cols <- setdiff(names(df), exclude)
  if (!length(cols)) return(NA_character_)
  numeric_cols <- cols[vapply(df[cols], numeric_share, 0) >= 0.6]
  if (!length(numeric_cols)) return(NA_character_)
  if (!is.null(avoid)) {
    allowed <- numeric_cols[!grepl(avoid, numeric_cols, ignore.case = TRUE)]
  } else {
    allowed <- numeric_cols
  }
  for (p in prefer) {
    hit <- numeric_cols[grepl(p, numeric_cols, ignore.case = TRUE)]
    if (length(hit)) return(hit[1])
  }
  if (length(allowed)) allowed[1] else numeric_cols[1]
}

# Columns that say what each row is: a type-like column and, if there is one,
# a source-like column, each with few enough distinct values to map by hand.
guess_label_columns <- function(df, exclude = NULL) {
  cols <- setdiff(names(df), exclude)
  if (!length(cols)) return(character())
  distinct <- vapply(df[cols], function(x) length(unique(x[nzchar(x)])), 0L)
  text_like <- cols[vapply(df[cols], numeric_share, 0) < 0.5 & distinct >= 1 & distinct <= 40]
  pick <- function(pattern) {
    hit <- text_like[grepl(pattern, text_like, ignore.case = TRUE)]
    if (length(hit)) hit[which.min(distinct[hit])] else character()
  }
  source_col <- pick("source|money type|contribution type")
  type_col <- pick("type|activity|transaction|action|description|category|memo|detail")
  out <- unique(c(type_col, source_col))
  if (!length(out)) {
    fallback <- text_like[distinct[text_like] >= 2 & distinct[text_like] <= 30]
    if (length(fallback)) out <- fallback[which.min(distinct[fallback])]
  }
  out
}

guess_tx_columns <- function(df) {
  date <- guess_date_column(df)
  amount <- guess_number_column(df, prefer = c("^amount$", "amount", "net", "total", "value", "\\$"),
                                avoid = "share|unit|price|nav|balance|qty|quantity",
                                exclude = date)
  list(date = date, amount = amount, labels = guess_label_columns(df, exclude = c(date, amount)))
}

guess_price_columns <- function(df) {
  date <- guess_date_column(df)
  price <- guess_number_column(df, prefer = c("adj.*close", "^nav$", "nav", "price", "close", "value"),
                               avoid = "volume|change|return|%|high|low|open",
                               exclude = date)
  list(date = date, price = price)
}

guess_balance_columns <- function(df) {
  date <- guess_date_column(df)
  balance <- guess_number_column(df, prefer = c("balance", "ending", "market value", "value", "total"),
                                 avoid = "share|unit|price|change|return|%",
                                 exclude = date)
  list(date = date, balance = balance)
}

# What each row is ---------------------------------------------------------------

# The choices offered for each kind of row, and what each becomes.
TX_CATEGORIES <- c(
  "Your contribution, pre-tax"   = "employee_pretax",
  "Your contribution, Roth"      = "employee_roth",
  "Your contribution, after-tax" = "employee_aftertax",
  "Employer match"               = "employer_match",
  "Employer non-elective (paid whether or not you contribute)" = "employer_nonelective",
  "Employer, other"              = "employer_other",
  "Rollover into the plan"       = "rollover",
  "Fee"                          = "fee",
  "Withdrawal or loan"           = "withdrawal",
  "Interest paid in cash"        = "dividend",
  "Transfer between cash and the fund" = "transfer",
  "Skip these rows"              = "skip"
)

CATEGORY_ROWS <- data.frame(
  key     = c("employee_pretax", "employee_roth", "employee_aftertax", "employer_match",
              "employer_nonelective", "employer_other", "rollover", "fee", "withdrawal",
              "dividend", "transfer"),
  type    = c(rep("contribution", 6), "rollover", "fee", "withdrawal", "dividend", "transfer"),
  source  = c("Employee pre-tax", "Employee Roth", "Employee after-tax", "Employer match",
              "Employer non-elective", "Employer", "Rollover", NA, NA, NA, NA),
  holding = c(rep("fund", 9), "cash", NA),
  stringsAsFactors = FALSE
)

# A first guess at what a row described by `label` is. Dividends and
# interest are skipped, because a total-return price already includes them.
guess_category <- function(label) {
  vapply(tolower(as.character(label)), function(x) {
    has <- function(p) grepl(p, x, perl = TRUE)
    if (has("total|balance|beginning|ending|pending|exchange|realloc|rebalanc|gain|loss|change in value|market value")) return("skip")
    if (has("reinvest|dividend|capital gain|interest")) return("skip")
    if (has("loan repay|repayment")) return("skip")
    if (has("roll ?over|roll-?in")) return("rollover")
    if (has("fee|admin|expense|charge|record ?keep|maintenance")) return("fee")
    if (has("withdraw|distribution|loan|hardship|payout|disburse")) return("withdrawal")
    if (has("match")) return("employer_match")
    if (has("non-?elective|profit.?shar|safe ?harbor|qnec|employer (basic|core)")) return("employer_nonelective")
    if (has("employer|company")) return("employer_other")
    if (has("roth")) return("employee_roth")
    if (has("after-?tax")) return("employee_aftertax")
    if (has("contribut|deferr|deposit|payroll|employee|elective|pre-?tax|salary|401")) return("employee_pretax")
    "skip"
  }, "", USE.NAMES = FALSE)
}

# One label per row from the chosen columns, such as "Contribution - Employer match",
# joined with a middle dot.
row_labels <- function(df, cols) {
  cols <- intersect(cols, names(df))
  if (!length(cols)) return(rep("Every row", nrow(df)))
  if (!nrow(df)) return(character())
  m <- do.call(cbind, lapply(df[cols], function(x) {
    x <- trimws(as.character(x))
    x[is.na(x)] <- ""
    x
  }))
  as.character(apply(m, 1, function(v) {
    v <- v[nzchar(v)]
    if (length(v)) paste(v, collapse = paste0(" ", MIDDOT, " ")) else "(blank)"
  }))
}

# Turns an imported table into r401k transactions.
#
# `categories` maps each label from row_labels() to a key in TX_CATEGORIES;
# labels it doesn't name are guessed. With `fix_signs`, a label whose amounts
# all have the wrong sign for its kind (fees listed as positive numbers, say)
# is flipped. Returns the transactions plus notes on what was dropped or changed.
convert_transactions <- function(df, date_col, amount_col, label_cols = character(),
                                 categories = NULL, date_format = NULL, fix_signs = TRUE,
                                 filter_col = NULL, filter_values = NULL) {
  if (!date_col %in% names(df)) stop("Choose the column with the date.", call. = FALSE)
  if (!amount_col %in% names(df)) stop("Choose the column with the amount.", call. = FALSE)
  notes <- character()
  if (!is.null(filter_col) && nzchar(filter_col) && filter_col %in% names(df)) {
    keep <- trimws(df[[filter_col]]) %in% filter_values
    if (sum(!keep)) notes <- c(notes, sprintf("Left out %d row(s) for other investments.", sum(!keep)))
    df <- df[keep, , drop = FALSE]
  }
  if (is.null(date_format) || is.na(date_format)) date_format <- detect_date_format(df[[date_col]])
  if (is.na(date_format)) stop("Couldn't read the dates in \"", date_col, "\". Pick the date format.", call. = FALSE)

  labels <- row_labels(df, label_cols)
  key <- if (length(categories)) unname(categories[labels]) else rep(NA_character_, length(labels))
  key[is.na(key)] <- guess_category(labels[is.na(key)])
  dates <- parse_dates(df[[date_col]], date_format)
  amounts <- parse_money(df[[amount_col]])

  drop_note <- function(mask, what) {
    if (any(mask)) notes <<- c(notes, sprintf("Left out %d row(s) %s.", sum(mask), what))
  }
  skip <- key == "skip"
  drop_note(skip, "you chose to skip")
  no_date <- !skip & is.na(dates)
  drop_note(no_date, "without a readable date, such as totals or notes")
  no_amount <- !skip & !no_date & is.na(amounts)
  drop_note(no_amount, "without a readable amount")
  zero <- !skip & !no_date & !no_amount & amounts == 0
  drop_note(zero, "with an amount of zero")
  keep <- !(skip | no_date | no_amount | zero)

  rows <- CATEGORY_ROWS[match(key[keep], CATEGORY_ROWS$key), ]
  amounts <- amounts[keep]
  labels <- labels[keep]
  flipped <- character()
  if (fix_signs && length(amounts)) {
    for (lab in unique(labels)) {
      i <- labels == lab
      type <- rows$type[i][1]
      wrong <- if (type %in% c("contribution", "rollover")) amounts[i] < 0
               else if (type %in% c("fee", "withdrawal")) amounts[i] > 0
               else rep(FALSE, sum(i))
      if (all(wrong)) {
        amounts[i] <- -amounts[i]
        flipped <- c(flipped, lab)
      }
    }
  }
  if (length(flipped)) {
    notes <- c(notes, sprintf("Flipped the sign of %s, so money in is positive and money out is negative.",
                              paste0("\"", flipped, "\"", collapse = ", ")))
  }
  tx <- data.frame(date = format(dates[keep]), type = rows$type, amount = round(amounts, 6),
                   source = rows$source, holding = rows$holding, stringsAsFactors = FALSE)
  tx <- tx[order(tx$date), , drop = FALSE]
  rownames(tx) <- NULL
  list(tx = tx, notes = notes, date_format = date_format)
}

# Daily prices from an imported table. Repeated dates with the same price are
# merged; repeated dates with different prices are an error.
convert_prices <- function(df, date_col, price_col, date_format = NULL) {
  if (!date_col %in% names(df)) stop("Choose the column with the date.", call. = FALSE)
  if (!price_col %in% names(df)) stop("Choose the column with the price.", call. = FALSE)
  if (is.null(date_format) || is.na(date_format)) date_format <- detect_date_format(df[[date_col]])
  if (is.na(date_format)) stop("Couldn't read the dates in \"", date_col, "\". Pick the date format.", call. = FALSE)
  dates <- parse_dates(df[[date_col]], date_format)
  prices <- parse_money(df[[price_col]])
  bad <- is.na(dates) | is.na(prices) | prices <= 0
  notes <- if (any(bad)) sprintf("Left out %d row(s) without a readable date and price.", sum(bad)) else character()
  nav <- unique(data.frame(date = dates[!bad], nav = prices[!bad]))
  if (!nrow(nav)) stop("No rows had both a date and a price.", call. = FALSE)
  clash <- unique(nav$date[duplicated(nav$date)])
  if (length(clash)) {
    stop(sprintf("Some dates have two different prices, for example %s.", format(clash[1])), call. = FALSE)
  }
  nav <- nav[order(nav$date), ]
  rownames(nav) <- NULL
  list(nav = nav, notes = notes, date_format = date_format)
}

convert_balances <- function(df, date_col, balance_col, date_format = NULL) {
  if (!date_col %in% names(df)) stop("Choose the column with the date.", call. = FALSE)
  if (!balance_col %in% names(df)) stop("Choose the column with the balance.", call. = FALSE)
  if (is.null(date_format) || is.na(date_format)) date_format <- detect_date_format(df[[date_col]])
  if (is.na(date_format)) stop("Couldn't read the dates in \"", date_col, "\". Pick the date format.", call. = FALSE)
  dates <- parse_dates(df[[date_col]], date_format)
  balances <- parse_money(df[[balance_col]])
  bad <- is.na(dates) | is.na(balances)
  out <- data.frame(date = format(dates[!bad]), balance = balances[!bad], stringsAsFactors = FALSE)
  out <- out[order(out$date), , drop = FALSE]
  rownames(out) <- NULL
  list(balances = out,
       notes = if (any(bad)) sprintf("Left out %d row(s) without a readable date and balance.", sum(bad)) else character(),
       date_format = date_format)
}

# Price days more than 4 calendar days apart, the longest normal market
# closure, between `from` and the last price.
price_gaps <- function(nav, from = min(nav$date)) {
  d <- nav$date[nav$date >= from - 7]
  if (length(d) < 2) return(data.frame(after = as.Date(character()), before = as.Date(character())))
  gap <- which(diff(d) > 4)
  data.frame(after = d[gap], before = d[gap + 1])
}

# A short id for file names, from a fund name.
fund_slug <- function(name) {
  s <- tolower(trimws(name %||% ""))
  s <- gsub("[^a-z0-9]+", "-", s)
  s <- gsub("^-+|-+$", "", s)
  s <- substr(s, 1, 40)
  if (nzchar(s)) s else "fund"
}
