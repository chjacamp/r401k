# Daily prices ----------------------------------------------------------------
#
# Prices live in data/nav_<fund_id>.csv with two columns: date, nav.
#
# `nav` must be a total-return series, meaning it doesn't drop when the fund
# pays a distribution. Collective trusts usually qualify as-is because they
# reinvest internally. For a mutual fund or ETF, use an adjusted close.

nav_path <- function(fund_id, data_dir = "data") {
  file.path(data_dir, sprintf("nav_%s.csv", fund_id))
}

read_nav <- function(fund_id, data_dir = "data") {
  path <- nav_path(fund_id, data_dir)
  if (!file.exists(path)) {
    stop("No price file at ", path, ". It needs two columns: date, nav.", call. = FALSE)
  }
  nav <- read.csv(path, stringsAsFactors = FALSE)
  if (!all(c("date", "nav") %in% names(nav))) {
    stop(path, " needs columns named date and nav.", call. = FALSE)
  }
  nav <- data.frame(date = as.Date(nav$date), nav = as.numeric(nav$nav))
  if (anyNA(nav$date) || anyNA(nav$nav) || any(nav$nav <= 0)) {
    stop(path, " has a blank date, a blank price, or a price at or below zero.", call. = FALSE)
  }
  if (any(duplicated(nav$date))) stop(path, " lists the same date twice.", call. = FALSE)
  nav[order(nav$date), ]
}

# Merge new prices into data/nav_<fund_id>.csv.
#
# `fetch` is any function(start, end) returning a data.frame with columns date
# and nav, so this works with whichever price source you're allowed to use.
# Only dates missing from the file are requested, plus the last week in case
# recent prices were corrected.
update_nav <- function(fund_id, fetch, first_needed = NULL, data_dir = "data") {
  path   <- nav_path(fund_id, data_dir)
  cached <- if (file.exists(path)) read_nav(fund_id, data_dir)
  if (is.null(first_needed)) first_needed <- min(read_transactions(data_dir)$date)

  covered <- !is.null(cached) && min(cached$date) <= first_needed
  from    <- if (covered) max(cached$date) - 7 else first_needed - 7
  fresh   <- fetch(from, Sys.Date())
  if (!all(c("date", "nav") %in% names(fresh))) {
    stop("`fetch` must return columns named date and nav.", call. = FALSE)
  }
  fresh <- data.frame(date = as.Date(fresh$date), nav = as.numeric(fresh$nav))

  old <- if (covered) cached[!cached$date %in% fresh$date, ] else fresh[0, ]
  out <- rbind(old, fresh)
  out <- out[order(out$date), ]
  dir.create(data_dir, showWarnings = FALSE)
  write.csv(out, path, row.names = FALSE)

  added <- if (is.null(cached)) nrow(out) else sum(!out$date %in% cached$date)
  message(sprintf("%s: %d new price(s), now through %s.", basename(path), added, max(out$date)))
  invisible(out)
}
