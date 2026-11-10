#' Read a daily price file
#'
#' Prices live in `<data_dir>/nav_<fund_id>.csv` with two columns, `date` and
#' `nav`. `nav` must be a total-return series, meaning it doesn't drop when the
#' fund pays a distribution. Collective trusts usually qualify as published,
#' because they reinvest internally. For a mutual fund or ETF, use an adjusted
#' close.
#'
#' @param data_dir Folder holding your `transactions.csv` and price file.
#' @param fund_id Id in the price file's name. If `NULL`, the folder's only
#'   `nav_*.csv` is used.
#' @return A data frame with columns `date` and `nav`, sorted by date.
#' @export
#' @examples
#' head(read_nav(demo_data_dir()))
read_nav <- function(data_dir, fund_id = NULL) {
  if (is.null(fund_id)) fund_id <- detect_fund_id(data_dir)
  path <- nav_path(data_dir, fund_id)
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

#' Add new prices to a price file
#'
#' Requests only the dates missing from `<data_dir>/nav_<fund_id>.csv`, plus
#' the last week in case recent prices were corrected, and merges them in.
#' `fetch` can wrap any price source you're allowed to use, including a file
#' you downloaded by hand. Check the terms of any website before automating it.
#'
#' @inheritParams read_nav
#' @param fetch A function of `(start, end)` returning a data frame with
#'   columns `date` and `nav`.
#' @param fund_id Id for the price file's name. Required when the folder has
#'   no price file yet.
#' @param first_needed Earliest date you need. Defaults to your first transaction.
#' @return The updated prices, invisibly.
#' @export
#' @examples
#' dir <- tempfile()
#' dir.create(dir)
#' file.copy(file.path(demo_data_dir(), "transactions.csv"), dir)
#' flat <- function(start, end) data.frame(date = seq(start, end, by = "day"), nav = 100)
#' prices <- update_nav(dir, flat, fund_id = "example")
#' range(prices$date)
update_nav <- function(data_dir, fetch, fund_id = NULL, first_needed = NULL) {
  if (is.null(fund_id)) fund_id <- detect_fund_id(data_dir)
  path   <- nav_path(data_dir, fund_id)
  cached <- if (file.exists(path)) read_nav(data_dir, fund_id)
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

nav_path <- function(data_dir, fund_id) {
  file.path(data_dir, sprintf("nav_%s.csv", fund_id))
}

detect_fund_id <- function(data_dir) {
  files <- list.files(data_dir, pattern = "^nav_.+\\.csv$")
  if (length(files) != 1) {
    stop(sprintf("Expected one nav_<fund_id>.csv in %s but found %d. Pass fund_id.",
                 data_dir, length(files)), call. = FALSE)
  }
  sub("^nav_(.+)\\.csv$", "\\1", files)
}
