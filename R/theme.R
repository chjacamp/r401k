# Colors, formatting, and small UI pieces shared by ui.R and server.R -----------

INK <- list(primary = "#0b0b0b", secondary = "#52514e", muted = "#898781",
            grid = "#e1e0d9", axis = "#c3c2b7", surface = "#fcfcfb", page = "#f9f9f7")

# One color per kind of money, the same in every chart.
HUE <- list(you = "#2a78d6", you_light = "#86b6ef", employer = "#eb6834",
            employer_light = "#f4b08f", market = "#1baf7a", loss = "#e34948", start = "#c3c2b7")

FONT <- "system-ui, -apple-system, 'Segoe UI', sans-serif"

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
  .chart-card .card-header { background: transparent; border-bottom: 0; padding-bottom: 0; }
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
