# =============================================================================
# UFO Sightings Dashboard — Shiny App
# Converted from static-HTML generator to a deployable Shiny application.
#
# Required packages:
#   install.packages(c("shiny","bslib","readr","dplyr","tidyr","stringr","plotly","DT"))
#
# Usage:
#   shiny::runApp("app_shiny.R")          # local
#   rsconnect::deployApp(".")             # shinyapps.io
# =============================================================================

library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)
library(DT)

# =============================================================================
# CONFIGURATION
# =============================================================================
TOP_N       <- 10L
MAX_DUR_SEC <- 12600

COLORS <- c("#58a6ff","#3fb950","#f78166","#bc8cff","#ffa657",
            "#39d0d8","#56d364","#ff7b72","#d2a8ff","#e3b341")

STATE_NAMES <- c(
  AL="Alabama",AK="Alaska",AZ="Arizona",AR="Arkansas",CA="California",
  CO="Colorado",CT="Connecticut",DE="Delaware",FL="Florida",GA="Georgia",
  HI="Hawaii",ID="Idaho",IL="Illinois",IN="Indiana",IA="Iowa",KS="Kansas",
  KY="Kentucky",LA="Louisiana",ME="Maine",MD="Maryland",MA="Massachusetts",
  MI="Michigan",MN="Minnesota",MS="Mississippi",MO="Missouri",MT="Montana",
  NE="Nebraska",NV="Nevada",NH="New Hampshire",NJ="New Jersey",
  NM="New Mexico",NY="New York",NC="North Carolina",ND="North Dakota",
  OH="Ohio",OK="Oklahoma",OR="Oregon",PA="Pennsylvania",RI="Rhode Island",
  SC="South Carolina",SD="South Dakota",TN="Tennessee",TX="Texas",UT="Utah",
  VT="Vermont",VA="Virginia",WA="Washington",WV="West Virginia",
  WI="Wisconsin",WY="Wyoming",DC="District of Columbia",PR="Puerto Rico",
  GU="Guam",VI="U.S. Virgin Islands",AS="American Samoa",
  MP="N. Mariana Islands"
)

# =============================================================================
# DATA — loaded once at startup, shared across all sessions
# =============================================================================
df <- read_csv("us_ufo_sighting.csv", show_col_types = FALSE) |>
  mutate(State = str_to_upper(str_trim(State))) |>
  mutate(
    shape10 = str_to_lower(str_trim(as.character(shape10))),
    shape10 = if_else(is.na(shape10) | shape10 %in% c("", "na", "NA"),
                      "unknown", shape10),
    shape10 = str_to_sentence(shape10)
  ) |>
  filter(!is.na(duration_sec), duration_sec > 0, duration_sec <= MAX_DUR_SEC) |>
  mutate(duration_min = duration_sec / 60) |>
  filter(!is.na(Year)) |>
  mutate(Year = as.integer(Year))

required_cols <- c("Event_Date", "City", "Month", "Day", "Hour",
                   "State", "shape10", "duration_sec", "Year")
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0)
  stop("CSV is missing required columns: ", paste(missing_cols, collapse = ", "))

detail <- df |>
  group_by(State, Year, shape10, Hour) |>
  summarise(count = n(), sum_dur_min = sum(duration_min, na.rm = TRUE),
            .groups = "drop")

all_states <- sort(unique(df$State))
year_min   <- min(df$Year, na.rm = TRUE)
year_max   <- max(df$Year, na.rm = TRUE)

state_choices <- c(
  "All States / Territories" = "ALL",
  setNames(all_states,
    ifelse(all_states %in% names(STATE_NAMES), STATE_NAMES[all_states], all_states))
)

# =============================================================================
# THEMES
# =============================================================================
dark_theme <- bs_theme(
  bg = "#0d1117", fg = "#e6edf3", primary = "#58a6ff",
  "border-color" = "#30363d"
)
light_theme <- bs_theme(
  bg = "#cce8ff", fg = "#1f2328", primary = "#0969da",
  "border-color" = "#d0d7de"
)

pth <- function(dark) {
  if (dark)
    list(bg = "#161b22", paper = "#161b22", fc = "#8b949e",
         grid = "rgba(48,54,61,0.7)", tbg = "#1c2230", tb = "#30363d")
  else
    list(bg = "#e8f4ff", paper = "#e8f4ff", fc = "#57606a",
         grid = "rgba(208,215,222,0.7)", tbg = "#ffffff", tb = "#d0d7de")
}

CHART_FONT <- "Segoe UI, Helvetica Neue, Arial, sans-serif"

ax <- function(t, ...) {
  list(gridcolor = t$grid, showline = FALSE, zeroline = FALSE,
       tickfont = list(color = t$fc, size = 11, family = CHART_FONT), title = "",
       automargin = TRUE, ticklen = 5, tickcolor = "transparent", ...)
}

nodata_plot <- function(t) {
  plot_ly() |>
    layout(
      plot_bgcolor  = t$bg, paper_bgcolor = t$paper,
      annotations = list(list(
        text = "No data for selected filters", showarrow = FALSE,
        x = 0.5, y = 0.5, xref = "paper", yref = "paper",
        font = list(color = t$fc, size = 14, family = CHART_FONT)
      ))
    ) |>
    config(displayModeBar = FALSE)
}

base_layout <- function(p, t, show_legend = FALSE) {
  p |>
    layout(
      plot_bgcolor  = t$bg,
      paper_bgcolor = t$paper,
      font      = list(color = t$fc, size = 12,
                       family = "Segoe UI, Helvetica Neue, Arial, sans-serif"),
      margin    = list(l = 0, r = 10, t = 10, b = 0),
      showlegend = show_legend
    ) |>
    config(displayModeBar = FALSE)
}

# =============================================================================
# CUSTOM CSS
# =============================================================================
custom_css <- "
  body { font-family: 'Segoe UI', 'Helvetica Neue', Arial, sans-serif; }

  /* ── Filter bar ──────────────────────────────────────── */
  #filter-bar {
    position: sticky; top: 0; z-index: 100;
    background: #13181f;
    border-bottom: 1px solid var(--bs-border-color);
    padding: 8px 16px;
  }
  [data-app-theme='light'] #filter-bar { background: #b8dcf8; }
  .filter-inner {
    display: flex; align-items: center; gap: 10px; flex-wrap: wrap;
    max-width: 1400px; margin: 0 auto;
  }
  .filter-group { display: flex; align-items: center; gap: 6px; }
  .filter-group label {
    font-size: 0.75rem; color: var(--bs-secondary-color);
    margin-bottom: 0; white-space: nowrap;
  }
  .filter-group .form-group,
  .filter-group .shiny-input-container { margin-bottom: 0 !important; }
  .filter-group .form-control {
    padding: 3px 8px !important; font-size: 0.75rem !important; height: auto !important;
  }
  .filter-group .selectize-input {
    padding: 3px 8px !important; min-height: auto !important; font-size: 0.75rem !important;
  }
  #reset_btn {
    background: transparent !important; border-radius: 6px;
    padding: 3px 12px !important; font-size: 0.75rem !important; cursor: pointer;
    border: 1px solid var(--bs-primary) !important;
    color: var(--bs-primary) !important; box-shadow: none !important;
  }
  #theme_btn_el {
    background: transparent; border: 1px solid #f7c948; color: #f7c948;
    border-radius: 6px; padding: 3px 12px; font-size: 0.75rem; cursor: pointer;
    line-height: 1.5;
  }
  #theme_btn_el:hover { background: rgba(247,201,72,0.12); }
  [data-app-theme='light'] #theme_btn_el {
    border-color: #0969da; color: #0969da;
  }
  [data-app-theme='light'] #theme_btn_el:hover { background: rgba(9,105,218,0.12); }

  /* ── Header ──────────────────────────────────────────── */
  .dash-header { padding: 20px 16px 10px; max-width: 1400px; margin: 0 auto; }
  .dash-header h1 {
    font-family: 'Segoe UI', 'Helvetica Neue', Arial, sans-serif;
    font-size: 1.75rem; color: var(--bs-primary); letter-spacing: 0.04em;
  }
  .dash-header p { font-size: 0.75rem; color: var(--bs-secondary-color); margin-top: 4px; }

  /* ── Body ────────────────────────────────────────────── */
  .dash-body { max-width: 1400px; margin: 0 auto; padding: 14px 16px 40px; }

  /* ── KPI cards ───────────────────────────────────────── */
  .kpi-row {
    display: grid; grid-template-columns: repeat(4, 1fr);
    gap: 14px; margin-bottom: 14px;
  }
  .kpi-card {
    background: #161b22;
    border: 1px solid var(--bs-border-color);
    border-radius: 10px; padding: 20px;
  }
  [data-app-theme='light'] .kpi-card { background: #e8f4ff; }
  .kpi-value {
    font-family: 'Segoe UI', 'Helvetica Neue', Arial, sans-serif;
    font-size: 1.55rem; font-weight: 700; line-height: 1;
  }
  .kpi-label {
    font-size: 0.7rem; color: var(--bs-secondary-color);
    text-transform: uppercase; letter-spacing: 0.06em; margin-top: 10px;
  }
  .kpi-card:nth-child(1) .kpi-value { color: #58a6ff; }
  .kpi-card:nth-child(2) .kpi-value { color: #3fb950; }
  .kpi-card:nth-child(3) .kpi-value { color: #bc8cff; }
  .kpi-card:nth-child(4) .kpi-value { color: #ffa657; }

  /* ── Chart cards ─────────────────────────────────────── */
  .chart-card {
    background: #161b22;
    border: 1px solid var(--bs-border-color);
    border-radius: 10px; padding: 16px; margin-bottom: 14px;
  }
  [data-app-theme='light'] .chart-card { background: #e8f4ff; }
  .chart-title {
    font-size: 0.72rem; color: var(--bs-secondary-color);
    text-transform: uppercase; letter-spacing: 0.08em; margin-bottom: 10px;
  }
  .chart-2col {
    display: grid; grid-template-columns: repeat(2, 1fr);
    gap: 14px; margin-bottom: 14px;
  }

  /* ── Tab content ─────────────────────────────────────── */
  .tab-content, .tab-pane { background: transparent !important; }

  /* ── Tabs ────────────────────────────────────────────── */
  .nav-tabs { border-bottom-color: var(--bs-border-color) !important; }
  .nav-tabs .nav-link {
    color: var(--bs-secondary-color) !important;
    border: none !important; border-bottom: 2px solid transparent !important;
    font-size: 1rem; padding: 8px 20px; margin-bottom: -1px;
    background: transparent !important;
  }
  .nav-tabs .nav-link.active {
    color: var(--bs-primary) !important;
    border-bottom-color: var(--bs-primary) !important;
    background: transparent !important;
  }
  .nav-tabs .nav-link:hover { color: var(--bs-body-color) !important; }

  /* ── DT table ────────────────────────────────────────── */
  .dataTables_wrapper { color: var(--bs-body-color); }
  table.dataTable { color: var(--bs-body-color) !important;
                    border-color: var(--bs-border-color) !important; }
  table.dataTable thead th {
    background: #13181f !important;
    color: var(--bs-secondary-color) !important;
    border-bottom: 1px solid var(--bs-border-color) !important;
    font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.05em;
  }
  [data-app-theme='light'] table.dataTable thead th { background: #b8dcf8 !important; }
  table.dataTable tbody td {
    border-color: var(--bs-border-color) !important; font-size: 0.8rem;
    background: #161b22;
  }
  table.dataTable tbody tr:nth-child(even) td { background: rgba(255,255,255,0.02) !important; }
  [data-app-theme='light'] table.dataTable tbody td { background: #e8f4ff !important; }
  [data-app-theme='light'] table.dataTable tbody tr:nth-child(even) td { background: rgba(0,0,0,0.02) !important; }
  table.dataTable tbody tr:hover td { background: #1c2230 !important; }
  [data-app-theme='light'] table.dataTable tbody tr:hover td { background: #d4ecff !important; }
  .dataTables_info, .dataTables_paginate {
    color: var(--bs-secondary-color) !important; font-size: 0.75rem !important;
  }
  .paginate_button { background: transparent !important;
                     color: var(--bs-secondary-color) !important; }
  .paginate_button.current {
    background: var(--bs-primary) !important;
    color: #fff !important; border-radius: 4px;
  }

  /* ── Responsive ──────────────────────────────────────── */
  @media (max-width: 900px) {
    .kpi-row   { grid-template-columns: repeat(2, 1fr); }
    .chart-2col { grid-template-columns: 1fr; }
  }
  @media (max-width: 500px) {
    .kpi-row { grid-template-columns: 1fr 1fr; }
  }
"

# =============================================================================
# UI
# =============================================================================
ui <- page_fluid(
  theme = dark_theme,

  tags$head(
    tags$style(HTML(custom_css)),
    tags$script(HTML("
      document.documentElement.setAttribute('data-app-theme', 'dark');
      Shiny.addCustomMessageHandler('setThemeBtnLabel', function(label) {
        var btn = document.getElementById('theme_btn_el');
        if (btn) btn.innerHTML = label;
      });
      Shiny.addCustomMessageHandler('setThemeClass', function(theme) {
        document.documentElement.setAttribute('data-app-theme', theme);
      });
    "))
  ),

  # Header
  div(class = "dash-header",
    h1(HTML("&#x1F6F8; UFO Sightings Dashboard"))
  ),

  # Sticky filter bar
  div(id = "filter-bar",
    div(class = "filter-inner",
      div(class = "filter-group",
        tags$label("State/Territory"),
        selectInput("state_sel", NULL, choices = state_choices, width = "210px")
      ),
      div(class = "filter-group",
        tags$label("Year"),
        numericInput("year_from", NULL, value = year_min,
                     min = year_min, max = year_max, step = 1, width = "80px"),
        span("–", style = "color:#484f58"),
        numericInput("year_to", NULL, value = year_max,
                     min = year_min, max = year_max, step = 1, width = "80px"),
        actionButton("reset_btn", "Reset"),
        tags$button(id = "theme_btn_el",
          onclick = "Shiny.setInputValue('theme_toggle', Math.random())",
          HTML("&#9728; Bright Theme"))
      )
    )
  ),

  # Main body
  div(class = "dash-body",

    # KPI row
    div(class = "kpi-row",
      div(class = "kpi-card",
        div(class = "kpi-value", textOutput("kpi_total",    inline = TRUE)),
        div(class = "kpi-label", "Total Sightings")),
      div(class = "kpi-card",
        div(class = "kpi-value", textOutput("kpi_coverage", inline = TRUE)),
        div(class = "kpi-label", "Coverage")),
      div(class = "kpi-card",
        div(class = "kpi-value", textOutput("kpi_range",    inline = TRUE)),
        div(class = "kpi-label", "Year Range")),
      div(class = "kpi-card",
        div(class = "kpi-value", textOutput("kpi_shape",    inline = TRUE)),
        div(class = "kpi-label", "Top Shape"))
    ),

    tabsetPanel(type = "tabs",

      # ── Tab 1: Overview ──────────────────────────────────────────────────
      tabPanel("Overview",
        br(),
        div(class = "chart-card",
          div(class = "chart-title", "Sightings per Year"),
          plotlyOutput("chart_ts", height = "300px")
        ),
        div(class = "chart-2col",
          div(class = "chart-card",
            div(class = "chart-title", "Top Shapes by Count"),
            plotlyOutput("chart_shapes", height = "300px")
          ),
          div(class = "chart-card",
            div(class = "chart-title", "Sightings by Hour of Day"),
            plotlyOutput("chart_hour", height = "300px")
          ),
          div(class = "chart-card",
            div(class = "chart-title", "Avg Duration by Shape (minutes)"),
            plotlyOutput("chart_dur", height = "300px")
          ),
          div(class = "chart-card",
            div(class = "chart-title", "Year-over-Year % Change"),
            plotlyOutput("chart_yoy", height = "300px")
          )
        )
      ),

      # ── Tab 2: Analysis ──────────────────────────────────────────────────
      tabPanel("Analysis",
        br(),
        div(class = "chart-2col",
          div(class = "chart-card",
            div(class = "chart-title", "Top States / Territories by Count"),
            plotlyOutput("chart_states", height = "300px")
          ),
          div(class = "chart-card",
            div(class = "chart-title", "Shape Share (% of Sightings)"),
            plotlyOutput("chart_donut", height = "300px")
          )
        ),
        div(class = "chart-card",
          div(class = "chart-title", "Recent Sightings"),
          DTOutput("recent_table")
        )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  is_dark <- reactiveVal(TRUE)

  # ── Theme toggle ────────────────────────────────────────────────────────
  observeEvent(input$theme_toggle, {
    d <- !is_dark()
    is_dark(d)
    session$setCurrentTheme(if (d) dark_theme else light_theme)
    session$sendCustomMessage("setThemeBtnLabel",
      if (d) "&#9728; Bright Theme" else "&#9790; Dark Theme")
    session$sendCustomMessage("setThemeClass", if (d) "dark" else "light")
  }, ignoreInit = TRUE)

  # ── Reset filters ───────────────────────────────────────────────────────
  observeEvent(input$reset_btn, {
    updateSelectInput(session,  "state_sel", selected = "ALL")
    updateNumericInput(session, "year_from", value = year_min)
    updateNumericInput(session, "year_to",   value = year_max)
  })

  # ── Filtered data ───────────────────────────────────────────────────────
  filt_detail <- reactive({
    req(input$state_sel)
    yf <- if (is.null(input$year_from) || is.na(input$year_from)) year_min else as.integer(input$year_from)
    yt <- if (is.null(input$year_to)   || is.na(input$year_to))   year_max else as.integer(input$year_to)
    d  <- detail |> filter(Year >= yf, Year <= yt)
    if (input$state_sel != "ALL") d <- filter(d, State == input$state_sel)
    d
  })

  filt_recent <- reactive({
    req(input$state_sel)
    yf <- if (is.null(input$year_from) || is.na(input$year_from)) year_min else as.integer(input$year_from)
    yt <- if (is.null(input$year_to)   || is.na(input$year_to))   year_max else as.integer(input$year_to)
    d <- df |> filter(Year >= yf, Year <= yt)
    if (input$state_sel != "ALL") d <- filter(d, State == input$state_sel)
    d |>
      arrange(desc(Year), desc(Month), desc(Day)) |>
      head(400) |>
      select(Event_Date, City, State, shape10, duration_sec) |>
      mutate(across(everything(), ~ replace_na(as.character(.), "—")))
  })

  th <- reactive(pth(is_dark()))

  # ── KPIs ────────────────────────────────────────────────────────────────
  output$kpi_total <- renderText({
    format(sum(filt_detail()$count), big.mark = ",")
  })
  output$kpi_coverage <- renderText({
    st <- input$state_sel
    if (is.null(st) || st == "ALL") "U.S. National"
    else if (st %in% names(STATE_NAMES)) STATE_NAMES[[st]] else st
  })
  output$kpi_range <- renderText({
    d <- filt_detail()
    if (!nrow(d)) return("—")
    paste0(min(d$Year), "–", max(d$Year))
  })
  output$kpi_shape <- renderText({
    d <- filt_detail()
    if (!nrow(d)) return("—")
    d |> group_by(shape10) |> summarise(n = sum(count), .groups = "drop") |>
      slice_max(n, n = 1, with_ties = FALSE) |> pull(shape10)
  })

  # ── Chart 1: Time Series ─────────────────────────────────────────────────
  output$chart_ts <- renderPlotly({
    d <- filt_detail() |> group_by(Year) |> summarise(n = sum(count)) |> arrange(Year)
    t <- th()
    if (!nrow(d)) return(nodata_plot(t))
    tick_yrs <- unique(c(d$Year[1], d$Year[nrow(d)]))
    plot_ly(d, x = ~Year, y = ~n, type = "scatter", mode = "lines+markers",
      line   = list(color = COLORS[1], width = 2),
      marker = list(color = COLORS[1], size = 5),
      fill = "tozeroy", fillcolor = "rgba(88,166,255,0.12)",
      hovertemplate = "%{x}: %{y:,}<extra></extra>"
    ) |>
      layout(
        plot_bgcolor = t$bg, paper_bgcolor = t$paper,
        font = list(color = t$fc, size = 12, family = CHART_FONT), showlegend = FALSE,
        margin = list(l = 0, r = 10, t = 10, b = 0, pad = 8),
        xaxis = ax(t, tickvals = tick_yrs, ticktext = as.character(tick_yrs)),
        yaxis = ax(t)
      ) |>
      config(displayModeBar = FALSE)
  })

  # ── Chart 2: Top Shapes ──────────────────────────────────────────────────
  output$chart_shapes <- renderPlotly({
    d <- filt_detail() |>
      group_by(shape10) |> summarise(n = sum(count)) |>
      slice_max(n, n = TOP_N, with_ties = FALSE) |> arrange(n)
    t <- th()
    if (!nrow(d)) return(nodata_plot(t))
    plot_ly(d, x = ~n, y = ~shape10, type = "bar", orientation = "h",
      marker = list(color = COLORS[seq_len(nrow(d))]),
      hovertemplate = "%{y}: %{x:,}<extra></extra>"
    ) |>
      layout(
        plot_bgcolor = t$bg, paper_bgcolor = t$paper,
        font = list(color = t$fc, size = 12, family = CHART_FONT), showlegend = FALSE,
        margin = list(l = 0, r = 10, t = 10, b = 0, pad = 8),
        xaxis = ax(t),
        yaxis = ax(t, categoryorder = "total ascending")
      ) |>
      config(displayModeBar = FALSE)
  })

  # ── Chart 3: Hour of Day ─────────────────────────────────────────────────
  output$chart_hour <- renderPlotly({
    t <- th()
    fd <- filt_detail()
    if (!nrow(fd)) return(nodata_plot(t))
    d <- fd |>
      group_by(Hour) |> summarise(n = sum(count)) |>
      right_join(tibble(Hour = 0:23), by = "Hour") |>
      mutate(n = replace_na(n, 0)) |>
      arrange(Hour) |>
      mutate(label = case_when(
        Hour == 0  ~ "12am", Hour == 12 ~ "12pm",
        Hour < 12  ~ paste0(Hour, "am"),
        TRUE       ~ paste0(Hour - 12, "pm")
      ))
    plot_ly(d, x = ~label, y = ~n, type = "bar",
      marker = list(color = COLORS[5]),
      hovertemplate = "%{x}: %{y:,}<extra></extra>"
    ) |>
      layout(
        plot_bgcolor = t$bg, paper_bgcolor = t$paper,
        font = list(color = t$fc, size = 12, family = CHART_FONT), showlegend = FALSE,
        margin = list(l = 0, r = 10, t = 10, b = 0, pad = 8),
        xaxis = ax(t, tickangle = -45), yaxis = ax(t)
      ) |>
      config(displayModeBar = FALSE)
  })

  # ── Chart 4: Avg Duration by Shape ───────────────────────────────────────
  output$chart_dur <- renderPlotly({
    d <- filt_detail() |>
      group_by(shape10) |>
      summarise(avg_dur = sum(sum_dur_min) / sum(count)) |>
      slice_max(avg_dur, n = TOP_N, with_ties = FALSE) |>
      arrange(avg_dur)
    t <- th()
    if (!nrow(d)) return(nodata_plot(t))
    cat_order <- c(d$shape10[d$shape10 == "Other"],
                   d$shape10[d$shape10 != "Other"])
    plot_ly(d, x = ~round(avg_dur, 1), y = ~shape10, type = "bar", orientation = "h",
      marker = list(color = COLORS[4]),
      hovertemplate = "%{y}: %{x} min<extra></extra>"
    ) |>
      layout(
        plot_bgcolor = t$bg, paper_bgcolor = t$paper,
        font = list(color = t$fc, size = 12, family = CHART_FONT), showlegend = FALSE,
        margin = list(l = 0, r = 10, t = 10, b = 0, pad = 8),
        xaxis = ax(t), yaxis = ax(t, categoryorder = "array", categoryarray = cat_order)
      ) |>
      config(displayModeBar = FALSE)
  })

  # ── Chart 5: YoY % Change ────────────────────────────────────────────────
  output$chart_yoy <- renderPlotly({
    d <- filt_detail() |>
      group_by(Year) |> summarise(n = sum(count)) |>
      arrange(Year) |>
      mutate(pct = round((n - lag(n)) / lag(n) * 100, 1)) |>
      filter(!is.na(pct), is.finite(pct))
    t <- th()
    if (!nrow(d)) return(nodata_plot(t))
    plot_ly(d, x = ~Year, y = ~pct, type = "bar",
      marker = list(color = ifelse(d$pct >= 0, COLORS[2], COLORS[3])),
      hovertemplate = "%{x}: %{y}%<extra></extra>"
    ) |>
      layout(
        plot_bgcolor = t$bg, paper_bgcolor = t$paper,
        font = list(color = t$fc, size = 12, family = CHART_FONT), showlegend = FALSE,
        margin = list(l = 0, r = 10, t = 10, b = 0, pad = 8),
        xaxis = ax(t), yaxis = ax(t)
      ) |>
      config(displayModeBar = FALSE)
  })

  # ── Chart 6: Top States ──────────────────────────────────────────────────
  output$chart_states <- renderPlotly({
    d <- filt_detail() |>
      group_by(State) |> summarise(n = sum(count)) |>
      slice_max(n, n = TOP_N, with_ties = FALSE) |>
      mutate(name = if_else(State %in% names(STATE_NAMES),
                            STATE_NAMES[State], State)) |>
      arrange(n)
    t <- th()
    if (!nrow(d)) return(nodata_plot(t))
    plot_ly(d, x = ~n, y = ~name, type = "bar", orientation = "h",
      marker = list(color = COLORS[2]),
      hovertemplate = "%{y}: %{x:,}<extra></extra>"
    ) |>
      layout(
        plot_bgcolor = t$bg, paper_bgcolor = t$paper,
        font = list(color = t$fc, size = 12, family = CHART_FONT), showlegend = FALSE,
        margin = list(l = 0, r = 10, t = 10, b = 0, pad = 8),
        xaxis = ax(t), yaxis = ax(t, categoryorder = "total descending")
      ) |>
      config(displayModeBar = FALSE)
  })

  # ── Chart 7: Shape Donut ─────────────────────────────────────────────────
  output$chart_donut <- renderPlotly({
    d <- filt_detail() |>
      group_by(shape10) |> summarise(n = sum(count)) |>
      slice_max(n, n = TOP_N, with_ties = FALSE)
    t <- th()
    if (!nrow(d)) return(nodata_plot(t))
    plot_ly(d, labels = ~shape10, values = ~n, type = "pie", hole = 0.45,
      marker = list(
        colors = COLORS[seq_len(nrow(d))],
        line   = list(color = t$bg, width = 2)
      ),
      textinfo = "percent",
      textfont = list(color = "#ffffff", size = 10),
      hovertemplate = "%{label}: %{value:,} (%{percent})<extra></extra>"
    ) |>
      layout(
        plot_bgcolor = t$bg, paper_bgcolor = t$paper,
        font = list(color = t$fc, size = 12, family = CHART_FONT), showlegend = TRUE,
        margin = list(l = 0, r = 0, t = 10, b = 0, pad = 8),
        legend = list(font = list(color = t$fc, size = 11, family = CHART_FONT))
      ) |>
      config(displayModeBar = FALSE)
  })

  # ── Recent Sightings Table ───────────────────────────────────────────────
  output$recent_table <- renderDT({
    d <- filt_recent() |>
      mutate(duration_sec = if_else(
        duration_sec == "—", "—", paste0(duration_sec, "s")
      )) |>
      rename(Date = Event_Date, Shape = shape10, Duration = duration_sec)

    datatable(d,
      options = list(
        pageLength = 10,
        dom = "tip",
        columnDefs = list(list(className = "dt-left", targets = "_all")),
        language = list(
          paginate = list(previous = "❮ Prev", `next` = "Next ❯"),
          info     = "Showing _START_–_END_ of _TOTAL_ sightings"
        )
      ),
      rownames  = FALSE,
      selection = "none",
      class     = "compact"
    )
  })
}

# =============================================================================
shinyApp(ui, server)
