# Colors, formatting, and small UI pieces shared by ui.R and server.R -----------

INK <- list(primary = "#0b0b0b", secondary = "#52514e", muted = "#898781",
            grid = "#e1e0d9", axis = "#c3c2b7", surface = "#fcfcfb", page = "#f9f9f7")

# One color per kind of money, the same in every chart.
HUE <- list(you = "#2a78d6", you_light = "#86b6ef", employer = "#eb6834",
            employer_light = "#f4b08f", market = "#1baf7a", loss = "#e34948", start = "#c3c2b7")

FONT <- "system-ui, -apple-system, 'Segoe UI', sans-serif"

app_theme <- function() {
  bs_theme(version = 5, bg = INK$page, fg = INK$primary, primary = HUE$you,
           base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "sans-serif"))
}

wash <- function(hex, alpha) {
  v <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", v[1], v[2], v[3], alpha)
}

usd    <- function(x) scales::dollar(x, accuracy = 1, style_negative = "minus")
usd2   <- function(x) scales::dollar(x, accuracy = 0.01, style_negative = "minus")
signed <- function(x) paste0(ifelse(x >= 0, "+", ""), usd(x))
pct1   <- function(x) scales::percent(x, accuracy = 0.1)
pct2   <- function(x) if (is.na(x)) "n/a" else scales::percent(x, accuracy = 0.01, style_negative = "minus")
pct_s  <- function(x) if (is.na(x)) "n/a" else paste0(if (x >= 0) "+" else "", pct2(x))
long_date <- function(d) format(d, "%b %d, %Y")

tile <- function(label, value, note = NULL) {
  div(class = "tile",
      div(class = "tile-label", label),
      div(class = "tile-value", value),
      if (!is.null(note)) div(class = "tile-note", note))
}

chart_card <- function(title, subtitle, output) {
  card(class = "chart-card", full_screen = TRUE,
       card_header(div(class = "card-title-text", title),
                   div(class = "card-sub", subtitle)),
       card_body(output, fill = FALSE))
}

stat_list <- function(rows) {
  div(class = "stat-list", lapply(names(rows), function(k) {
    tagList(div(class = "k", k), div(class = "v", rows[[k]]))
  }))
}

style_plot <- function(p, y_format = "$~s", hover = "x unified", legend = TRUE, filename = "r401k-chart") {
  p |>
    layout(
      font = list(family = FONT, size = 12, color = INK$secondary),
      paper_bgcolor = INK$surface, plot_bgcolor = INK$surface,
      margin = list(l = 52, r = 20, t = 10, b = 32),
      hovermode = hover,
      hoverlabel = list(bgcolor = INK$surface, bordercolor = INK$grid,
                        font = list(family = FONT, color = INK$primary, size = 12)),
      xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, linecolor = INK$axis,
                   tickcolor = INK$axis, fixedrange = TRUE, showspikes = FALSE),
      yaxis = list(title = "", gridcolor = INK$grid, zeroline = TRUE, zerolinecolor = INK$axis,
                   tickformat = y_format, fixedrange = TRUE, linecolor = "rgba(0,0,0,0)"),
      showlegend = legend,
      legend = list(orientation = "h", x = 0, y = 1.13, xanchor = "left",
                    font = list(size = 12, color = INK$secondary))
    ) |>
    # Only the camera button, shown on hover, so charts can be saved as PNGs.
    config(modeBarButtons = list(list("toImage")), displaylogo = FALSE,
           toImageButtonOptions = list(format = "png", filename = filename, scale = 2))
}

data_marker <- function(p, when) {
  layout(p,
    shapes = list(list(type = "line", x0 = when, x1 = when, yref = "paper", y0 = 0, y1 = 1,
                       line = list(color = INK$axis, width = 1))),
    annotations = list(list(x = when, y = 0.02, yref = "paper", text = "transactions end",
                            showarrow = FALSE, xanchor = "right", yanchor = "bottom",
                            xshift = -4, font = list(size = 11, color = INK$secondary))))
}

APP_CSS <- sprintf("
  body { font-family: %s; }
  .period-row { display: flex; flex-wrap: wrap; align-items: center; gap: 12px; margin: 4px 0 14px; }
  .period-row .form-group { margin: 0; width: auto; }
  .period-row .shiny-options-group { display: flex; gap: 4px; flex-wrap: wrap; }
  .period-row .radio-inline { margin: 0; padding: 0; }
  .period-row input[type=radio] { position: absolute; opacity: 0; width: 0; height: 0; }
  .period-row .radio-inline span { display: inline-block; padding: 5px 14px; font-size: .875rem;
    border: 1px solid %s; border-radius: 6px; background: %s; color: %s; cursor: pointer; }
  .period-row input:checked + span { background: %s; border-color: %s; color: #fff; }
  .period-row input:focus-visible + span { outline: 2px solid %s; outline-offset: 1px; }
  .narrative { font-size: 1.05rem; color: %s; margin-bottom: 4px; max-width: 70rem; }
  .caveat { font-size: .85rem; color: %s; margin-bottom: 12px; }
  .tiles { display: grid; grid-template-columns: repeat(auto-fit, minmax(170px, 1fr)); gap: 10px; margin: 12px 0 16px; }
  .tile { background: %s; border: 1px solid rgba(11,11,11,.10); border-radius: 8px; padding: 12px 14px; }
  .tile-label { font-size: .8rem; color: %s; }
  .tile-value { font-size: 1.55rem; font-weight: 600; color: %s; line-height: 1.25; margin-top: 2px; }
  .tile-note { font-size: .78rem; color: %s; margin-top: 3px; }
  .chart-card { background: %s; border: 1px solid rgba(11,11,11,.10); box-shadow: none; }
  .chart-card .card-header { display: block; background: transparent; border-bottom: 0; padding-bottom: 0; }
  .card-title-text { font-weight: 600; color: %s; }
  .card-sub { font-size: .82rem; color: %s; }
  .stat-list { display: grid; grid-template-columns: 1fr auto; gap: 6px 16px; font-size: .9rem; }
  .stat-list .v { font-variant-numeric: tabular-nums; font-weight: 600; color: %s; text-align: right; }
  .stat-list .k { color: %s; }
  .method p, .method li { max-width: 62rem; }
  table.dataTable td, table.dataTable th { font-variant-numeric: tabular-nums; font-size: .86rem; white-space: nowrap; }
  .data-warning { border-left: 3px solid #fab219; background: #fffaf0; border-radius: 6px;
    padding: 10px 14px; margin: 8px 0 12px; font-size: .88rem; color: #0b0b0b; }
  .data-warning ul { margin: 4px 0 0; padding-left: 18px; }
", FONT, INK$grid, INK$surface, INK$secondary, INK$primary, INK$primary, HUE$you,
   INK$primary, INK$secondary, INK$surface, INK$secondary, INK$primary, INK$secondary,
   INK$surface, INK$primary, INK$secondary, INK$primary, INK$secondary)

# Extra styles for the web app's demo banner and Your data tab.
WEB_CSS <- sprintf("
  .loading { color: %s; padding: 40px 0; text-align: center; }
  .demo-banner { display: flex; flex-wrap: wrap; gap: 6px 12px; align-items: baseline;
    background: #eef5fd; border-left: 3px solid %s; border-radius: 6px; padding: 9px 14px;
    margin: 8px 0 12px; font-size: .9rem; }
  .demo-banner a { font-weight: 600; }
  .data-page { max-width: 68rem; margin: 6px auto 40px; }
  .data-page h4 { margin: 8px 0 6px; font-weight: 600; }
  .data-page h5 { font-weight: 600; }
  .step-help { color: %s; max-width: 52rem; }
  .field-help { color: %s; font-size: .82rem; margin: -4px 0 10px; }
  .step-alt { margin: -4px 0 14px; font-size: .9rem; }
  .wiz-steps { display: flex; flex-wrap: wrap; gap: 6px; margin: 4px 0 16px; }
  .wiz-step { border: 1px solid %s; background: %s; color: %s; border-radius: 999px;
    padding: 4px 12px 4px 4px; font-size: .84rem; display: inline-flex; align-items: center; gap: 6px; }
  .wiz-step .num { display: inline-grid; place-items: center; width: 22px; height: 22px; border-radius: 50%%;
    background: %s; color: %s; font-size: .75rem; font-weight: 600; }
  .wiz-step.done .num { background: %s; color: #fff; }
  .wiz-step.active { border-color: %s; color: %s; font-weight: 600; }
  .wiz-step.active .num { background: %s; color: #fff; }
  .wiz-nav { position: sticky; bottom: 0; display: flex; justify-content: space-between; gap: 10px;
    background: %s; border-top: 1px solid %s; padding: 12px 0; margin-top: 18px; z-index: 5; }
  .wiz-card { background: %s; border: 1px solid rgba(11,11,11,.10); box-shadow: none; margin-bottom: 14px; }
  .wiz-card .card-header { display: block; background: transparent; border-bottom: 0; padding-bottom: 0; }
  .start-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(240px, 1fr)); gap: 14px; margin: 14px 0; }
  .start-grid .card-body { display: flex; flex-direction: column; }
  .start-grid .card-body p { color: %s; font-size: .9rem; flex: 1; }
  .status-card { background: %s; border: 1px solid rgba(11,11,11,.10); border-radius: 8px; padding: 16px 18px; }
  .status-card p { margin-bottom: 10px; }
  .status-actions, .finish-actions, .table-tools { display: flex; flex-wrap: wrap; gap: 8px; margin: 10px 0; }
  .privacy-note { font-size: .86rem; color: %s; border-left: 3px solid %s; padding: 4px 12px; margin: 6px 0 18px; }
  .msg { border-radius: 6px; padding: 9px 14px; margin: 8px 0; font-size: .9rem; }
  .msg-ok { background: #effaf5; border-left: 3px solid %s; }
  .msg-warn { background: #fffaf0; border-left: 3px solid #fab219; }
  .msg-error { background: #fdf0f0; border-left: 3px solid %s; }
  .msg .totals { color: %s; font-size: .84rem; margin-top: 3px; }
  ul.notes { font-size: .86rem; color: %s; margin: 6px 0; }
  .cat-table { width: 100%%; border-collapse: collapse; font-size: .88rem; margin-bottom: 8px; }
  .cat-table th { font-weight: 600; color: %s; border-bottom: 1px solid %s; padding: 6px 8px; }
  .cat-table td { border-bottom: 1px solid %s; padding: 4px 8px; vertical-align: middle; }
  .cat-table td .form-group { margin: 0; }
  .cat-table .num { text-align: right; font-variant-numeric: tabular-nums; white-space: nowrap; }
  .cat-table td:last-child { min-width: 240px; }
  details > summary { cursor: pointer; color: %s; font-size: .9rem; margin: 6px 0; }
  .paste-box, .header-fix, .filter-box, .help-box { margin: 4px 0 12px; }
  .help-box li { font-size: .88rem; margin-bottom: 4px; max-width: 56rem; }
  .table-input .shiny-input-container { margin-bottom: 6px; }
", INK$secondary, HUE$you, INK$secondary, INK$secondary,
   INK$grid, INK$surface, INK$secondary, INK$grid, INK$secondary, HUE$market,
   HUE$you, INK$primary, HUE$you, INK$page, INK$grid, INK$surface, INK$secondary,
   INK$surface, INK$secondary, HUE$market, HUE$market, HUE$loss, INK$secondary, INK$secondary,
   INK$secondary, INK$grid, INK$grid, HUE$you)
