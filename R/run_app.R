#' Run the 401(k) dashboard
#'
#' Rebuilds your account from the files in `data_dir` and returns the
#' dashboard as a Shiny app. With no arguments it runs on the bundled demo:
#' five years of made-up paychecks priced against the published daily NAV of
#' the Vanguard Target Retirement 2060 Trust Select.
#'
#' @inheritParams load_401k
#' @param fund_name Name shown in the app. Defaults to `fund_id`.
#' @param projection Starting values for the projection tab, from
#'   [projection_settings()].
#' @param ... Options passed to [shiny::shinyApp()], such as `port` or
#'   `launch.browser`.
#' @return A Shiny app object. It runs when printed, so calling `run_app()` at
#'   the console opens the dashboard. From a script, use
#'   `shiny::runApp(run_app())`.
#' @export
#' @examples
#' if (interactive()) {
#'   run_app()
#'   run_app("~/my401k", plan = plan_settings(match_cap = 0.01))
#' }
run_app <- function(data_dir = demo_data_dir(), fund_id = NULL, fund_name = NULL,
                    plan = plan_settings(), projection = projection_settings(), ...) {
  ctx <- app_context(data_dir, fund_id, fund_name, plan, projection)
  shiny::shinyApp(ui = app_ui(ctx), server = app_server(ctx), options = list(...))
}

#' Plan settings
#'
#' Describes your employer's contributions and fees. Used to infer your pay
#' from employer non-elective contributions and to project future balances.
#'
#' @param nonelective Employer contribution as a share of pay, made whether
#'   or not you contribute (0.03 is 3%).
#' @param match_rate Employer match per dollar you defer (1 is dollar for dollar).
#' @param match_cap Your deferrals are matched up to this share of pay.
#' @param monthly_fee Plan fee in dollars per month, used in projections.
#' @param fund_expense_ratio Fund expense ratio as a share (0.00045 is 0.045%).
#'   Used only to estimate expenses on the risk card; 0 hides that line.
#' @return A list of settings for [run_app()] and [load_401k()].
#' @export
#' @examples
#' plan_settings(nonelective = 0.03, match_cap = 0.01, monthly_fee = 5.25)
plan_settings <- function(nonelective = 0.03, match_rate = 1, match_cap = 0.04,
                          monthly_fee = 0, fund_expense_ratio = 0) {
  list(nonelective = nonelective, match_rate = match_rate, match_cap = match_cap,
       monthly_fee = monthly_fee, fund_expense_ratio = fund_expense_ratio)
}

#' Projection starting values
#'
#' Starting values for the projection tab's controls. All of them can be
#' changed in the app.
#'
#' @param annual_pay Annual pay in dollars. `NA` infers it from the last 12
#'   months of employer non-elective contributions.
#' @param your_rate Your contribution as a percent of pay. `NA` infers it from
#'   your most recent paycheck.
#' @param raise Annual raise, in percent.
#' @param rate_step Points to raise your contribution rate each year.
#' @param rate_cap Stop raising your rate at this percent.
#' @param expected_return Expected annual return, in percent.
#' @param return_range Points above and below `expected_return` to show.
#' @param inflation Annual inflation, in percent.
#' @param retirement_year Year the projection ends.
#' @param irs_limit This year's IRS limit on employee deferrals, in dollars.
#' @param irs_limit_growth Annual growth of that limit, in percent.
#' @return A list of settings for [run_app()].
#' @export
#' @examples
#' projection_settings(retirement_year = 2055, expected_return = 6)
projection_settings <- function(annual_pay = NA, your_rate = NA, raise = 3, rate_step = 0,
                                rate_cap = 15, expected_return = 7, return_range = 2,
                                inflation = 2.5, retirement_year = 2060,
                                irs_limit = 24500, irs_limit_growth = 2.5) {
  list(annual_pay = annual_pay, your_rate = your_rate, raise = raise, rate_step = rate_step,
       rate_cap = rate_cap, expected_return = expected_return, return_range = return_range,
       inflation = inflation, retirement_year = retirement_year, irs_limit = irs_limit,
       irs_limit_growth = irs_limit_growth)
}

#' Folder holding the demo data
#'
#' @return Path to the bundled demo `transactions.csv` and `nav_1685.csv`.
#' @export
#' @examples
#' list.files(demo_data_dir())
demo_data_dir <- function() {
  system.file("extdata", package = "r401k", mustWork = TRUE)
}

# Everything the UI and server need, computed once when the app is built.
app_context <- function(data_dir, fund_id, fund_name, plan, projection) {
  data_dir <- normalizePath(path.expand(data_dir), winslash = "/", mustWork = TRUE)
  D <- load_401k(data_dir, fund_id, plan)
  if (is.null(fund_name)) {
    is_demo <- identical(data_dir, normalizePath(demo_data_dir(), winslash = "/"))
    fund_name <- if (is_demo) "Vanguard Target Retirement 2060 Trust Select" else D$fund_id
  }
  first_known <- function(...) {
    for (x in list(...)) if (!is.na(x)) return(x)
  }
  list(
    D = D, data_dir = data_dir, fund_id = D$fund_id, fund_name = fund_name,
    plan = plan, projection = projection,
    first_date    = min(D$daily$date),
    last_nav_date = max(D$daily$date),
    data_through  = D$data_through,
    default_pay   = first_known(projection$annual_pay, infer_annual_pay(D, plan), 75000),
    default_rate  = first_known(projection$your_rate, infer_current_rate(D, plan) * 100, 6)
  )
}
