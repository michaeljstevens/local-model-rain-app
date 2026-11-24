# Precipitation Tracker
# Fetches real precipitation data from the Open-Meteo archive API.
# Run with: shiny::runApp()          (from this directory, which contains app.R)
#   or: shiny::runApp("/Users/michaelstevens/precip_tracker")

library(shiny)
library(bslib)
library(ggplot2)
library(lubridate)
library(dplyr)
library(jsonlite)
library(DT)

# --- Helpers -------------------------------------------------------------------

fetch_precip <- function(lat, lon, start, end) {
  url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive",
    "?latitude=", lat,
    "&longitude=", lon,
    "&start_date=", format(start, "%Y-%m-%d"),
    "&end_date=", format(end, "%Y-%m-%d"),
    "&daily=precipitation_sum,precipitation_hours",
    "&timezone=auto"
  )
  raw <- fromJSON(url)
  if (!is.null(raw$error)) {
    stop("API error: ", paste(raw$error, collapse = " "))
  }
  data.frame(
    date   = as.Date(raw$daily$time),
    precip = raw$daily$precipitation_sum,
    hours  = raw$daily$precipitation_hours
  )
}

# Roll up daily data to the requested granularity
roll_up <- function(d, level) {
  d <- d |> filter(!is.na(precip))
  if (nrow(d) == 0) return(d)
  d <- d |> arrange(date)
  period <- switch(level,
                   day     = floor_date(d$date, "day"),
                   week    = floor_date(d$date, "week"),
                   month   = floor_date(d$date, "month"),
                   quarter = floor_date(d$date, "quarter"),
                   year    = floor_date(d$date, "year"))
  d$period <- period
  d |>
    group_by(period) |>
    summarise(
      events = sum(precip > 0.01),   # must run before `precip` is shadowed
      precip = sum(precip, na.rm = TRUE),
      hours  = sum(hours, na.rm = TRUE),
      .groups = "drop"
    ) |>
    select(period, precip, hours, events)
}

# Null-coalescing helper
`%||%` <- function(x, y) {
  if (is.null(x)) return(y)
  if (is.character(x) && length(x) == 1 && !is.na(x) && x == "") return(y)
  x
}

# --- Preset file (read at build time) -------------------------------------------
# The dropdown is populated from disk when the UI is built, so it starts with
# any saved presets — no startup updateSelectInput is needed. Sending an
# update* to the preset selectize on the first flush (before the selectize
# widget has finished initialising) crashes a real browser; reading the CSV
# here sidesteps that entirely.
preset_file <- file.path(getwd(), "presets.csv")

load_presets <- function() {
  if (!file.exists(preset_file)) return(data.frame(
    name = character(), lat = numeric(), lon = numeric(),
    start = character(), end = character()))
  read.csv(preset_file, stringsAsFactors = FALSE)
}

initial_presets <- load_presets()
initial_preset_choices <- if (nrow(initial_presets) == 0) list("") else initial_presets$name

# --- UI -------------------------------------------------------------------------
ui <- page_fluid(
  title = "Precipitation Tracker",
  theme = bs_theme(
    primary   = "#2e6fb5",
    secondary = "#8fa6bd",
    success   = "#3d8b6d"
  ),

  layout_sidebar(
    sidebar = div(
      class = "p-3",

      card(
        card_title("Location"),
        card_body(
          numericInput("lat", "Latitude", value = 40.71, step = 0.01),
          numericInput("lon", "Longitude", value = -74.01, step = 0.01),
          textInput("place_name", "Place name (optional)",
                    placeholder = "e.g. New York")
        )
      ),
      class = "mb-3",

      card(
        card_title("Date range"),
        card_body(
          dateRangeInput("date_range", "Range",
                         start = Sys.Date() - 365,
                         end   = Sys.Date()),
          div(class = "mb-2",
              actionButton("fetch_btn", "Fetch data",
                           icon = icon("download"), width = "100%"))
        )
      ),
      class = "mb-3",

      card(
        card_title("Saved locations"),
        card_body(
          # dropdownParent = "body" lets the selectize dropdown render
          # relative to <body>, so it overflows the sidebar container
          # instead of being clipped by its overflow. Choices come from
          # disk at build time, so the dropdown starts populated.
          selectizeInput("preset", "Load preset", choices = initial_preset_choices,
                         selected = "",
                         options = list(dropdownParent = "body")),
          div(class = "mb-2",
              actionButton("save_btn", "Save current",
                           icon = icon("bookmark"), width = "100%"))
        )
      ),
      class = "mb-3",

      card(
        card_title("Roll-up"),
        card_body(
          # dropdownParent = "body" lets the selectize dropdown render
          # relative to <body>, so it overflows the sidebar container
          # instead of being clipped by its overflow.
          selectizeInput("rollup", "Granularity",
                         choices = c(
                           "Daily"     = "day",
                           "Weekly"    = "week",
                           "Monthly"   = "month",
                           "Quarterly" = "quarter",
                           "Yearly"    = "year"
                         ),
                         selected = "day",
                         options = list(dropdownParent = "body"))
        )
      ),
      class = "mb-3",

      div(class = "small text-muted",
          textOutput("data_status"))
    ),

    # --- Main panel ---
    card(
      card_body(
        div(
          class = "row g-3",
          div(class = "col-3", card(card_body(
            div(class = "small text-muted", "Total precip"),
            div(class = "fs-3 fw-semibold", textOutput("vb_total"))
          ))),
          div(class = "col-3", card(card_body(
            div(class = "small text-muted", "Wet hours"),
            div(class = "fs-3 fw-semibold", textOutput("vb_hours"))
          ))),
          div(class = "col-3", card(card_body(
            div(class = "small text-muted", "Max period"),
            div(class = "fs-3 fw-semibold", textOutput("vb_max")),
            div(class = "small text-muted", textOutput("vb_maxdate"))
          ))),
          div(class = "col-3", card(card_body(
            div(class = "small text-muted", "Periods"),
            div(class = "fs-3 fw-semibold", textOutput("vb_n"))
          )))
        )
      )
    ),
    plotOutput("precip_plot", height = 420),
    card(
      card_title("Records"),
      card_body(DTOutput("dt_records"))
    ),
    # Let the page scroll at natural height instead of locking to the
    # viewport: a fixed-height, scrollable sidebar clips the selectize
    # dropdown, forcing a full-container scroll to see its options.
    fillable = FALSE
  )
)

# --- Server ----------------------------------------------------------------------
server <- function(input, output, session) {

  recs   <- reactiveVal(NULL)
  status <- reactiveVal("No data loaded yet. Set a location and press Fetch.")
  place  <- reactiveVal("")

  # --- Saved presets (persisted to CSV) -------------------------------------
  # presets.csv is read at build time (initial_presets above), so the dropdown
  # starts populated. This reactive just tracks the set so a save can refresh
  # the dropdown — no startup update is sent.
  presets <- reactiveVal(initial_presets)

  update_preset_choices <- function() {
    p <- presets()
    choices <- if (nrow(p) == 0) list("")
              else p$name
    updateSelectInput(session, "preset", choices = choices, selected = "")
  }

  # --- Save current location + range ----------------------------------------
  observeEvent(input$save_btn, {
    lat <- input$lat
    lon <- input$lon
    dr  <- input$date_range
    if (is.null(lat) || is.null(lon) || is.na(lat) || is.na(lon) ||
        is.null(dr) || length(dr) != 2) {
      toast("Set a location and date range first", type = "warning")
      return(invisible())
    }
    name <- input$place_name
    if (is.null(name) || trimws(name) == "") {
      name <- paste0(round(lat, 2), ",", round(lon, 2))
    }
    # Drop any existing row with the same name, then append
    p <- presets()
    p <- p[!(p$name %in% name), ]
    p <- rbind(p, data.frame(
      name  = name,
      lat   = lat,
      lon   = lon,
      start = format(dr[1], "%Y-%m-%d"),
      end   = format(dr[2], "%Y-%m-%d"),
      stringsAsFactors = FALSE
    ))
    p <- p[order(p$name), ]
    write.csv(p, preset_file, row.names = FALSE)
    presets(p)
    update_preset_choices()
    toast(sprintf("Saved '%s'", name), type = "success")
  })

  # --- Load a preset ----------------------------------------------------------
  observeEvent(input$preset, {
    nm <- input$preset
    if (is.null(nm) || nm == "") return(invisible())
    p <- presets()
    row <- p[p$name == nm, ]
    if (nrow(row) == 0) return(invisible())
    updateNumericInput(session, "lat", value = row$lat)
    updateNumericInput(session, "lon", value = row$lon)
    updateTextInput(session, "place_name", value = nm)
    updateDateRangeInput(session, "date_range",
                         start = as.Date(row$start),
                         end   = as.Date(row$end))
    toast(sprintf("Loaded '%s' — press Fetch to load data", nm),
          type = "info")
  })

  # --- Fetch data -------------------------------------------------------------
  observeEvent(input$fetch_btn, {
    lat <- input$lat
    lon <- input$lon
    dr  <- input$date_range

    if (is.null(lat) || is.null(lon) || is.na(lat) || is.na(lon)) {
      toast("Enter valid coordinates", type = "warning")
      return(invisible())
    }
    if (is.null(dr) || length(dr) != 2) {
      toast("Select a date range", type = "warning")
      return(invisible())
    }

    status("Fetching from Open-Meteo…")

    res <- tryCatch(
      fetch_precip(lat, lon, dr[1], dr[2]),
      error = function(e) {
        status(paste("Error:", conditionMessage(e)))
        NULL
      }
    )

    if (is.null(res)) {
      toast("Fetch failed — check coordinates and range", type = "danger")
      return(invisible())
    }

    recs(res)
    place(input$place_name %||% paste0(lat, ", ", lon))

    status(sprintf("%d days | %d wet days | %.1f mm total | %s",
                   nrow(res), sum(res$precip > 0.01, na.rm = TRUE),
                   sum(res$precip, na.rm = TRUE), place()))
    toast(sprintf("Loaded %d days of data", nrow(res)), type = "success")
  })

  # --- Aggregated data ----------------------------------------------------------
  agg <- reactive({
    d <- recs()
    if (is.null(d) || nrow(d) == 0) return(NULL)
    roll_up(d, input$rollup)
  })

  # --- Stats ---------------------------------------------------------------------
  output$vb_total <- renderText({
    a <- agg()
    if (is.null(a)) return("—")
    sprintf("%.1f mm", sum(a$precip, na.rm = TRUE))
  })
  output$vb_hours <- renderText({
    a <- agg()
    if (is.null(a)) return("—")
    sprintf("%.0f hrs", sum(a$hours, na.rm = TRUE))
  })
  output$vb_max <- renderText({
    a <- agg()
    if (is.null(a)) return("—")
    sprintf("%.1f mm", max(a$precip))
  })
  output$vb_maxdate <- renderText({
    a <- agg()
    if (is.null(a)) return("")
    p <- max(a$period)
    switch(input$rollup,
           day     = format(p, "%b %d, %Y"),
           week    = paste("wk of", format(p, "%b %d")),
           month   = format(p, "%b %Y"),
           quarter = sprintf("Q%d %d", quarter(p), year(p)),
           year    = format(p, "%Y"))
  })
  output$vb_n <- renderText({
    a <- agg()
    if (is.null(a)) return("—")
    nrow(a)
  })

  # --- Plot -----------------------------------------------------------------------
  output$precip_plot <- renderPlot({
    a <- agg()
    if (is.null(a) || nrow(a) == 0) {
      plot.new(); text(0.5, 0.5, "No data — fetch from the sidebar")
      return(invisible())
    }

    p <- ggplot(a, aes(x = period, y = precip))

    if (input$rollup %in% c("quarter", "year")) {
      p <- p +
        geom_col(fill = "#2e6fb5", width = 0.8) +
        geom_point(color = "#2e6fb5", size = 2)
    } else {
      p <- p +
        geom_col(fill = "#2e6fb5", width = 0.8, alpha = 0.85) +
        geom_line(color = "#2e6fb5", linewidth = 0.6) +
        geom_point(color = "#2e6fb5", size = 1.5)
    }

    p +
      labs(x = NULL, y = "Precipitation (mm)",
           title = paste0("Precipitation — ", place(), " (", input$rollup, ")")) +
      theme_minimal()
  })

  # --- Table -----------------------------------------------------------------------
  output$dt_records <- renderDT(
    agg(),
    options = list(pageLength = 15, dom = "tp", lengthChange = FALSE),
    rownames = FALSE
  )

  # --- Status -----------------------------------------------------------------------
  output$data_status <- renderText(status())
}

shinyApp(ui, server)
