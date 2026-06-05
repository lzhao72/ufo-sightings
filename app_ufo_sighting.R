# ============================================================
# UFO Sightings Dashboard - Updated Version 2
# Fixes requested in "Please fix the following issues 2(1).docx"
# ============================================================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(readr)
library(lubridate)
library(scales)
library(usmap)
library(stringr)
library(bsicons)
library(rsconnect)

# ------------------------------------------------------------
# 1. Read and prepare data
# ------------------------------------------------------------

csv_path <- "us_ufo_sighting.csv"
cache_path <- "us_ufo_sighting_cache.rds"

load_ufo_data <- function(csv_file = csv_path, cache_file = cache_path) {
  cache_is_current <- file.exists(cache_file) &&
    file.info(cache_file)$mtime >= file.info(csv_file)$mtime

  if (cache_is_current) {
    return(readRDS(cache_file))
  }

  prepared_data <- read_csv(csv_file, show_col_types = FALSE) %>%
    rename_with(~ str_trim(.x)) %>%
    rename_with(tolower) %>%
    mutate(
      event_date = coalesce(
        ymd(event_date, quiet = TRUE),
        mdy(event_date, quiet = TRUE)
      ),
      year = as.numeric(year),
      month = as.numeric(month),
      hour = as.numeric(hour),
      duration_sec = as.numeric(duration_sec),
      state = toupper(as.character(state)),
      city = as.character(city),
      shape10 = as.character(shape10),
      shape10 = if_else(is.na(shape10) | shape10 == "", "other", shape10),
      shape10 = str_to_title(shape10)
    ) %>%
    filter(
      !is.na(year),
      !is.na(state),
      !is.na(shape10),
      year >= 1950
    )

  saveRDS(prepared_data, cache_file)
  prepared_data
}

ufo <- load_ufo_data()

# Filter values
state_choices <- sort(unique(ufo$state))
shape_choices <- sort(unique(ufo$shape10))
year_min <- min(ufo$year, na.rm = TRUE)
year_max <- max(ufo$year, na.rm = TRUE)

# Convert state abbreviation to state/territory name for KPI display.
state_lookup <- c(
  setNames(state.name, state.abb),
  "DC" = "District of Columbia",
  "PR" = "Puerto Rico",
  "GU" = "Guam",
  "VI" = "U.S. Virgin Islands",
  "AS" = "American Samoa",
  "MP" = "Northern Mariana Islands"
)

state_label <- function(state_code) {
  if_else(
    state_code %in% names(state_lookup),
    unname(state_lookup[state_code]),
    state_code
  )
}

# Fixed color palette for shape categories.
shape_palette <- c(
  "Light" = "#2F80ED",
  "Circle" = "#00A676",
  "Triangle" = "#F2994A",
  "Fireball" = "#EB5757",
  "Disk" = "#9B51E0",
  "Cigar" = "#8D6E63",
  "Other" = "#6C757D",
  "Sphere" = "#2D9CDB",
  "Oval" = "#F2C94C",
  "Formation" = "#27AE60"
)

extra_colors <- c(
  "#3366CC", "#DC3912", "#FF9900", "#109618", "#990099",
  "#0099C6", "#DD4477", "#66AA00", "#B82E2E", "#316395"
)

base_plot_theme <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", color = "#0F172A", size = base_size + 2),
      axis.title = element_text(face = "bold", color = "#334155", size = base_size),
      axis.text = element_text(color = "#475569", size = base_size - 1),
      axis.title.x = element_text(margin = margin(t = 28)),
      axis.title.y = element_text(margin = margin(r = 30)),
      legend.title = element_text(face = "bold", color = "#334155", size = base_size),
      legend.text = element_text(color = "#475569", size = base_size - 1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E2E8F0", linewidth = 0.35),
      plot.margin = margin(18, 28, 22, 28)
    )
}

polish_plotly <- function(
    plot,
    x_title,
    y_title,
    left = 96,
    right = 30,
    bottom = 92,
    top = 12,
    y_tick_standoff = 6) {
  plot %>%
    layout(
      font = list(family = "Inter", size = 11, color = "#334155"),
      margin = list(l = left, r = right, b = bottom, t = top),
      xaxis = list(
        automargin = TRUE,
        title = list(text = paste0("<b>", x_title, "</b>"), standoff = 30)
      ),
      yaxis = list(
        automargin = TRUE,
        ticklabelstandoff = y_tick_standoff,
        title = list(text = paste0("<b>", y_title, "</b>"), standoff = 26)
      )
    ) %>%
    config(displayModeBar = FALSE)
}

# ------------------------------------------------------------
# 2. User interface
# ------------------------------------------------------------

ui <- page_sidebar(

  title = NULL,

  theme = bs_theme(
    version = 5,
    bootswatch = "zephyr",
    base_font = font_google("Inter"),
    primary = "#2F80ED",
    secondary = "#64748B",
    success = "#27AE60",
    info = "#2D9CDB",
    warning = "#F2C94C",
    danger = "#EB5757"
  ),

  tags$head(
    tags$style(HTML("
      body {
        color: #0F172A;
        font-size: 12px;
        background: #F8FAFC;
      }
      .bslib-page-sidebar {
        gap: 12px;
        padding: 14px 16px;
      }
      .bslib-sidebar-layout {
        --bslib-sidebar-main-margin: 12px;
      }
      .bslib-sidebar-layout > .sidebar {
        background: linear-gradient(180deg, #08233F 0%, #0B3558 100%);
        color: white;
        border-right: 0;
        padding: 14px 12px;
      }
      .sidebar label,
      .sidebar h2,
      .sidebar h4,
      .sidebar p {
        color: white;
      }
      .sidebar h2 {
        font-size: 15px;
        line-height: 1.2;
        margin-bottom: 4px;
      }
      .sidebar h4 {
        font-size: 12px;
        line-height: 1.2;
        margin: 12px 0 8px 0;
      }
      .sidebar p,
      .sidebar label,
      .form-label,
      .control-label {
        font-size: 11px;
        line-height: 1.3;
      }
      .sidebar p {
        margin-bottom: 10px;
      }
      .sidebar hr {
        margin: 12px 0;
      }
      .sidebar .form-select,
      .sidebar .form-control {
        background-color: #0D3B63;
        color: white;
        border-color: #4B6B88;
        min-height: 30px;
        padding: 4px 8px;
        font-size: 11px;
      }
      .sidebar .irs--shiny .irs-bar,
      .sidebar .irs--shiny .irs-single {
        background: #2F80ED;
        border-color: #2F80ED;
      }
      .card {
        border: 0;
        border-radius: 8px;
        box-shadow: 0 3px 10px rgba(15, 23, 42, 0.07);
        margin-bottom: 12px;
      }
      .card-body {
        padding: 12px 14px 14px 14px;
      }
      .card-header {
        background: #FFFFFF;
        border-bottom: 1px solid #E2E8F0;
        padding: 10px 12px;
        font-size: 12px;
        line-height: 1.2;
        font-weight: 700;
        color: #0F172A;
      }
      .value-box {
        border-radius: 8px;
        box-shadow: 0 3px 10px rgba(15, 23, 42, 0.07);
      }
      .bslib-value-box,
      .value-box {
        min-height: 82px;
        background: #FFFFFF !important;
        color: #0F172A !important;
      }
      .bslib-value-box .value-box-area,
      .value-box .value-box-area {
        padding: 12px 14px !important;
      }
      .bslib-value-box .value-box-title,
      .value-box .value-box-title {
        color: #475569 !important;
        font-size: 11px;
        font-weight: 700;
        line-height: 1.15;
        min-height: 1.35em;
        text-transform: none;
        letter-spacing: 0;
      }
      .bslib-value-box .value-box-value,
      .value-box .value-box-value {
        color: #0F172A !important;
        font-size: clamp(1rem, 1.2vw, 1.35rem);
        font-weight: 800;
        line-height: 1.12;
        overflow-wrap: anywhere;
      }
      .bslib-value-box .value-box-showcase,
      .value-box .value-box-showcase {
        color: #2F80ED !important;
        opacity: 0.95;
      }
      .nav-tabs .nav-link {
        padding: 8px 12px;
        font-size: 12px;
        font-weight: 600;
      }
      .dataTables_wrapper {
        font-size: 10px;
      }
      #ufo_table .dataTables_wrapper {
        font-size: 10px !important;
      }
      table.dataTable {
        width: 100% !important;
      }
      table.dataTable tbody td {
        padding: 3px 4px;
        white-space: nowrap;
      }
      table.dataTable thead th {
        padding: 3px 4px;
        white-space: nowrap;
      }
      #ufo_table table.dataTable tbody td,
      #ufo_table table.dataTable thead th {
        font-size: 10px !important;
        padding: 1px 2px;
        line-height: 1.1;
      }
      #ufo_table table.dataTable thead th,
      #ufo_table table.dataTable thead th.sorting,
      #ufo_table table.dataTable thead th.sorting_asc,
      #ufo_table table.dataTable thead th.sorting_desc {
        font-size: 10px !important;
        font-weight: 600;
      }
      #ufo_table table.dataTable tbody td,
      #ufo_table table.dataTable tbody td.dt-center {
        font-size: 10px !important;
      }
      #map_state_disabled {
        background-color: #E9ECEF !important;
        color: #6C757D !important;
        cursor: not-allowed;
      }
      .dataTables_paginate {
        font-size: 10px !important;
        padding-top: 14px !important;
        transform: scale(0.82);
        transform-origin: right bottom;
      }
      .dataTables_info {
        font-size: 10px !important;
        padding-top: 16px !important;
      }
      #ufo_table .dataTables_paginate {
        font-size: 10px !important;
        padding-top: 8px !important;
        transform: scale(0.76);
        transform-origin: right bottom;
      }
      #ufo_table .dataTables_paginate .paginate_button {
        font-size: 10px !important;
        padding: 2px 4px !important;
      }
      #ufo_table .dataTables_info {
        font-size: 10px !important;
        padding-top: 10px !important;
      }
      .source-note {
        color: #475569;
        font-size: 10px;
        padding: 4px 2px 0 2px;
      }
      .sidebar-actions {
        display: grid;
        gap: 6px;
        margin-top: 10px;
      }
      .sidebar-actions .btn {
        min-height: 30px;
        padding: 5px 9px;
        font-size: 11px;
      }
      .sidebar-actions .btn,
      .sidebar-actions .form-control {
        width: 100%;
      }
      .overview-kpis {
        margin-bottom: 0;
      }
      .overview-kpis .bslib-layout-column-wrap {
        margin-bottom: 0;
      }
    "))
  ),

  sidebar = sidebar(
    width = 220,

    h2("UFO sightings dashboard"),
    p("Explore UFO sightings reported in the United States from 1950 onward."),
    hr(),

    h4("Global filters"),

    sliderInput(
      inputId = "year_range",
      label = "Year range",
      min = year_min,
      max = year_max,
      value = c(year_min, year_max),
      sep = ""
    ),

    selectInput(
      inputId = "state",
      label = "State/territory",
      choices = c("All" = "All", setNames(state_choices, state_label(state_choices))),
      selected = "All"
    ),

    selectInput(
      inputId = "shape",
      label = "Shape category",
      choices = c("All", shape_choices),
      selected = "All"
    ),

    hr(),

    div(
      class = "sidebar-actions",
      actionButton(
        inputId = "apply_filter",
        label = "Apply filters",
        icon = icon("filter"),
        class = "btn-primary"
      ),

      actionButton(
        inputId = "reset_filter",
        label = "Reset filters",
        icon = icon("rotate-left"),
        class = "btn-primary"
      ),

      downloadButton(
        outputId = "download_data",
        label = "Download filtered data",
        class = "btn-primary"
      )
    )
  ),

  navset_card_tab(

    # ========================================================
    # Overview tab
    # ========================================================
    nav_panel(
      "Overview",

      div(
        class = "overview-kpis",
        layout_column_wrap(
          width = 1/4,

          value_box(
            title = "Total sightings",
            value = textOutput("total_sightings"),
            showcase = bs_icon("binoculars"),
            theme = "primary"
          ),

          value_box(
            title = "States/territories represented",
            value = textOutput("total_states"),
            showcase = bs_icon("map"),
            theme = "success"
          ),

          value_box(
            title = "Most common shape",
            value = textOutput("common_shape"),
            showcase = bs_icon("circle"),
            theme = "info"
          ),

          value_box(
            title = "Median duration",
            value = textOutput("median_duration"),
            showcase = bs_icon("clock"),
            theme = "warning"
          )
        )
      ),

      layout_columns(
        card(
          card_header("Sightings over time"),
          plotlyOutput("year_plot", height = "440px")
        ),

        card(
          card_header("Sightings by shape"),
          plotlyOutput("shape_plot", height = "440px")
        ),

        col_widths = c(6, 6),
        gap = "12px"
      ),

      layout_columns(
        card(
          card_header("Top states/territories by sightings"),
          plotOutput("state_pie", height = "420px")
        ),

        card(
          card_header("U.S. map of sightings by state/territory"),
          plotOutput("state_map", height = "450px")
        ),

        card(
          card_header("Recent sightings"),
          DTOutput("ufo_table")
        ),

        col_widths = c(4, 4, 4),
        gap = "12px"
      ),

      div(
        class = "source-note",
        "Data source: NUFORC (National UFO Reporting Center). Dashboard uses cleaned U.S. sightings data from 1950 onward."
      )
    ),

    # ========================================================
    # Combined Shape and Duration tab
    # ========================================================
    nav_panel(
      "Shape and duration analysis",

      layout_columns(
        card(
          card_header("Shape distribution"),
          plotlyOutput("shape_dist", height = "440px")
        ),

        card(
          card_header("Shape trends over time"),
          plotlyOutput("shape_trend", height = "440px")
        ),

        col_widths = c(5, 7),
        gap = "12px"
      ),

      layout_columns(
        card(
          card_header("Duration histogram"),
          plotlyOutput("duration_hist", height = "440px")
        ),

        card(
          card_header("Duration by shape"),
          plotlyOutput("duration_box", height = "440px")
        ),

        col_widths = c(6, 6),
        gap = "12px"
      ),

      div(
        class = "source-note",
        "Data source: NUFORC (National UFO Reporting Center)."
      )
    )
  )
)

# ------------------------------------------------------------
# 3. Server
# ------------------------------------------------------------

server <- function(input, output, session) {

  applied_filters <- reactiveValues(
    year_range = c(year_min, year_max),
    state = "All",
    shape = "All"
  )

  observeEvent(input$apply_filter, {
    applied_filters$year_range <- input$year_range
    applied_filters$state <- input$state
    applied_filters$shape <- input$shape
  })

  observeEvent(input$reset_filter, {
    updateSliderInput(session, "year_range", value = c(year_min, year_max))
    updateSelectInput(session, "state", selected = "All")
    updateSelectInput(session, "shape", selected = "All")

    applied_filters$year_range <- c(year_min, year_max)
    applied_filters$state <- "All"
    applied_filters$shape <- "All"
  })

  filtered_data <- reactive({
    dat <- ufo %>%
      filter(
        year >= applied_filters$year_range[1],
        year <= applied_filters$year_range[2]
      )

    if (applied_filters$state != "All") {
      dat <- dat %>% filter(state == applied_filters$state)
    }

    if (applied_filters$shape != "All") {
      dat <- dat %>% filter(shape10 == applied_filters$shape)
    }

    dat
  })

  # Map ignores state filter so that the full national map remains visible.
  # It still respects year and shape filters.
  map_data <- reactive({
    dat <- ufo %>%
      filter(
        year >= applied_filters$year_range[1],
        year <= applied_filters$year_range[2],
        state %in% state.abb
      )

    if (applied_filters$shape != "All") {
      dat <- dat %>% filter(shape10 == applied_filters$shape)
    }

    dat
  })

  output$total_sightings <- renderText({
    comma(nrow(filtered_data()))
  })

  output$total_states <- renderText({
    if (applied_filters$state != "All") {
      selected <- applied_filters$state
      if (selected %in% names(state_lookup)) {
        return(state_lookup[[selected]])
      } else {
        return(selected)
      }
    }

    comma(n_distinct(filtered_data()$state))
  })

  output$common_shape <- renderText({
    dat <- filtered_data()
    if (nrow(dat) == 0) return("None")

    shape_summary <- dat %>%
      count(shape10, sort = TRUE) %>%
      mutate(pct = n / sum(n)) %>%
      slice(1)

    paste0(
      shape_summary$shape10,
      " (",
      percent(shape_summary$pct, accuracy = 0.1),
      ")"
    )
  })

  output$median_duration <- renderText({
    dat <- filtered_data()
    if (nrow(dat) == 0) return("None")

    paste0(comma(round(median(dat$duration_sec, na.rm = TRUE))), " sec")
  })

  output$year_plot <- renderPlotly({
    p <- filtered_data() %>%
      count(year) %>%
      ggplot(aes(x = year, y = n)) +
      geom_line(linewidth = 1, color = "#2F80ED") +
      geom_point(size = 1.3, color = "#2F80ED") +
      labs(x = "Year", y = "Number of sightings") +
      scale_y_continuous(labels = comma) +
      base_plot_theme()

    ggplotly(p, tooltip = c("x", "y")) %>%
      polish_plotly(
        x_title = "Year",
        y_title = "Number of sightings",
        bottom = 70
      )
  })

  output$shape_plot <- renderPlotly({
    plot_data <- filtered_data() %>%
      count(shape10, sort = TRUE) %>%
      slice_head(n = 10) %>%
      mutate(shape10 = reorder(shape10, n))

    displayed_shapes <- as.character(plot_data$shape10)
    missing_shapes <- setdiff(displayed_shapes, names(shape_palette))
    full_palette <- c(
      shape_palette,
      setNames(extra_colors[seq_along(missing_shapes)], missing_shapes)
    )

    p <- plot_data %>%
      ggplot(aes(x = n, y = shape10, fill = as.character(shape10))) +
      geom_col(width = 0.82, show.legend = FALSE) +
      labs(x = "Number of sightings", y = "Shape") +
      scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.08))) +
      scale_fill_manual(values = full_palette, guide = "none") +
      base_plot_theme() +
      theme(
        legend.position = "none",
        axis.text.y = element_text(margin = margin(r = 18))
      )

    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE) %>%
      polish_plotly(
        x_title = "Number of sightings",
        y_title = "Shape",
        left = 138,
        bottom = 70,
        y_tick_standoff = 24
      )
  })

  output$state_pie <- renderPlot({
    pie_data <- filtered_data() %>%
      count(state, sort = TRUE) %>%
      slice_head(n = 10) %>%
      mutate(
        pct = n / sum(n)
      )

    validate(
      need(nrow(pie_data) > 0, "No state/territory data available for the selected filters.")
    )

    pie_data <- pie_data %>%
      mutate(
        state_name = state_label(state),
        state_name = reorder(state_name, n),
        label_text = paste0(comma(n), "  (", percent(pct, accuracy = 0.1), ")")
      )

    ggplot(pie_data, aes(x = n, y = state_name, fill = state_name)) +
      geom_col(width = 0.72, show.legend = FALSE) +
      geom_text(
        aes(label = label_text),
        hjust = -0.05,
        size = 4,
        color = "#334155",
        fontface = "bold"
      ) +
      scale_fill_brewer(palette = "Paired") +
      scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.22))) +
      labs(x = "Sightings", y = NULL) +
      coord_cartesian(clip = "off") +
      base_plot_theme(base_size = 10) +
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 10, margin = margin(r = 12)),
        axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 10, margin = margin(t = 26)),
        axis.title.y = element_text(margin = margin(r = 30)),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(18, 64, 24, 32)
      )
  })

  output$state_map <- renderPlot({
    state_counts <- map_data() %>%
      count(state, name = "sightings") %>%
      filter(state %in% state.abb) %>%
      as.data.frame()

    validate(
      need(nrow(state_counts) > 0, "No map data available for the selected filters.")
    )

    # Robust map fix:
    # usmap::plot_usmap() requires a data frame with a column named "state".
    # The values must be two-letter state abbreviations, such as CA, TX, NY.
    usmap::plot_usmap(
      data = state_counts,
      values = "sightings",
      regions = "states",
      color = "white"
    ) +
      scale_fill_continuous(
        low = "#DCEBFF",
        high = "#2F80ED",
        name = "Sightings",
        labels = comma,
        guide = guide_colorbar(
          barheight = unit(82, "pt"),
          barwidth = unit(12, "pt")
        )
      ) +
      theme(
        legend.position = c(0.93, 0.25),
        legend.justification = c(1, 0),
        legend.background = element_rect(fill = "white", color = NA),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.title = element_text(face = "bold", color = "#334155", size = 12),
        legend.text = element_text(color = "#475569", size = 11),
        panel.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(8, 12, 8, 12)
      )
  })

  output$shape_dist <- renderPlotly({
    plot_data <- filtered_data() %>%
      count(shape10, sort = TRUE) %>%
      mutate(shape10 = reorder(shape10, n))

    displayed_shapes <- as.character(plot_data$shape10)
    missing_shapes <- setdiff(displayed_shapes, names(shape_palette))
    full_palette <- c(
      shape_palette,
      setNames(extra_colors[seq_along(missing_shapes)], missing_shapes)
    )

    p <- plot_data %>%
      ggplot(aes(x = n, y = shape10, fill = as.character(shape10))) +
      geom_col(show.legend = FALSE) +
      labs(x = "Number of sightings", y = "Shape") +
      scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.08))) +
      scale_fill_manual(values = full_palette, guide = "none") +
      base_plot_theme() +
      theme(
        legend.position = "none",
        axis.text.y = element_text(margin = margin(r = 10))
      )

    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE) %>%
      polish_plotly(
        x_title = "Number of sightings",
        y_title = "Shape",
        left = 138,
        y_tick_standoff = 14
      )
  })

  output$shape_trend <- renderPlotly({
    p <- filtered_data() %>%
      count(year, shape10) %>%
      ggplot(aes(x = year, y = n, color = shape10)) +
      geom_line(linewidth = 0.8) +
      labs(x = "Year", y = "Number of sightings", color = "Shape") +
      scale_y_continuous(labels = comma) +
      scale_color_manual(values = shape_palette) +
      base_plot_theme(base_size = 10)

    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      polish_plotly(
        x_title = "Year",
        y_title = "Number of sightings",
        right = 118
      )
  })

  output$duration_hist <- renderPlotly({
    p <- filtered_data() %>%
      filter(!is.na(duration_sec)) %>%
      ggplot(aes(x = duration_sec)) +
      geom_histogram(bins = 40, fill = "#27AE60", color = "white") +
      labs(x = "Duration in seconds", y = "Number of sightings") +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = comma) +
      base_plot_theme()

    ggplotly(p, tooltip = c("x", "y")) %>%
      polish_plotly(
        x_title = "Duration in seconds",
        y_title = "Number of sightings"
      )
  })

  output$duration_box <- renderPlotly({
    # Sort shape labels alphabetically, but place "Other" last.
    shape_order <- filtered_data() %>%
      filter(!is.na(shape10), !is.na(duration_sec)) %>%
      distinct(shape10) %>%
      pull(shape10) %>%
      sort()

    shape_order <- c(setdiff(shape_order, "Other"), "Other")

    p <- filtered_data() %>%
      filter(!is.na(shape10), !is.na(duration_sec)) %>%
      mutate(
        shape10 = factor(shape10, levels = rev(shape_order))
      ) %>%
      ggplot(
        aes(
          x = shape10,
          y = duration_sec,
          fill = shape10
        )
      ) +
      geom_boxplot(outlier.alpha = 0.55, outlier.size = 1.2, show.legend = FALSE) +
      coord_flip() +
      labs(x = "Shape", y = "Duration in seconds") +
      scale_y_continuous(labels = comma) +
      scale_fill_manual(values = shape_palette, guide = "none") +
      base_plot_theme() +
      theme(
        legend.position = "none"
      )

    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        showlegend = FALSE
      ) %>%
      polish_plotly(
        x_title = "Duration in seconds",
        y_title = "Shape",
        left = 122,
        bottom = 96
      )
  })


  output$ufo_table <- renderDT({
    filtered_data() %>%
      transmute(
        Date = format(event_date, "%Y-%m-%d"),
        State = state_label(state),
        City = city,
        Shape = shape10,
        `Duration<br>seconds` = duration_sec
      ) %>%
      arrange(desc(Date)) %>%
      datatable(
        options = list(
          pageLength = 10,
          scrollX = FALSE,
          autoWidth = FALSE,
          dom = "tip",
          columnDefs = list(
            list(width = "17%", targets = 0),
            list(width = "26%", targets = 1),
            list(width = "22%", targets = 2),
            list(width = "15%", targets = 3),
            list(width = "20%", targets = 4),
            list(className = "dt-center", targets = "_all")
          )
        ),
        rownames = FALSE,
        escape = FALSE
      )
  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("ufo_filtered_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
}

shinyApp(ui = ui, server = server)


