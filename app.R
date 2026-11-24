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

# For date ranges of <= 92 days the API returns one JSON object; longer
# ranges come back as a list of yearly objects — `dailys` normalises both.
fetch_precip <- function(lat, lon, start, end) {
  daily <- paste(
    "precipitation_sum,precipitation_hours",
    "temperature_2m_max,temperature_2m_min,temperature_2m_mean",
    "apparent_temperature_mean",
    "relative_humidity_2m_mean",
    "wind_speed_10m_mean,wind_speed_10m_max,wind_gusts_10m_max",
    "pressure_msl_mean,cloud_cover_mean,weather_code",
    sep = ","
  )
  url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive",
    "?latitude=", lat,
    "&longitude=", lon,
    "&start_date=", format(start, "%Y-%m-%d"),
    "&end_date=", format(end, "%Y-%m-%d"),
    "&daily=", daily,
    "&timezone=auto"
  )
  raw <- fromJSON(url)
  if (is.list(raw) && !is.null(raw$error)) {
    stop("API error: ", paste(raw$error, collapse = " "))
  }
  # The API returns one object for ranges of <= 92 days and a list of
  # yearly objects for longer ones — normalise to a single daily table.
  daily_df <- if (is.list(raw$daily)) raw$daily
              else do.call(rbind, lapply(as.list(raw), function(x) x$daily))
  data.frame(
    date        = as.Date(daily_df$time),
    precip      = daily_df$precipitation_sum,
    hours       = daily_df$precipitation_hours,
    temp_max    = daily_df$temperature_2m_max,
    temp_min    = daily_df$temperature_2m_min,
    temp_mean   = daily_df$temperature_2m_mean,
    apparent    = daily_df$apparent_temperature_mean,
    humidity    = daily_df$relative_humidity_2m_mean,
    wind_mean   = daily_df$wind_speed_10m_mean,
    wind_max    = daily_df$wind_speed_10m_max,
    gust_max    = daily_df$wind_gusts_10m_max,
    pressure    = daily_df$pressure_msl_mean,
    cloud       = daily_df$cloud_cover_mean,
    weather_code = daily_df$weather_code
  )
}

# Hourly data for a date range, same endpoint as fetch_precip(). The API
# returns one object for <= 92 days and a list of yearly objects for longer
# ranges — `hourlys` normalises both.
fetch_hourly <- function(lat, lon, start, end) {
  hourly <- paste(
    "temperature_2m,precipitation,relative_humidity_2m",
    "pressure_msl,cloud_cover,weather_code,wind_speed_10m,wind_gusts_10m",
    sep = ","
  )
  url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive",
    "?latitude=", lat,
    "&longitude=", lon,
    "&start_date=", format(start, "%Y-%m-%d"),
    "&end_date=", format(end, "%Y-%m-%d"),
    "&hourly=", hourly,
    "&timezone=auto"
  )
  raw <- fromJSON(url)
  if (is.list(raw) && !is.null(raw$error)) {
    stop("API error: ", paste(raw$error, collapse = " "))
  }
  hourly_df <- if (is.list(raw$hourly)) raw$hourly
               else do.call(rbind, lapply(as.list(raw), function(x) x$hourly))
  data.frame(
    date       = as.POSIXct(hourly_df$time, tz = "UTC"),
    temp       = hourly_df$temperature_2m,
    precip     = hourly_df$precipitation,
    humidity   = hourly_df$relative_humidity_2m,
    pressure   = hourly_df$pressure_msl,
    cloud      = hourly_df$cloud_cover,
    weather_code = hourly_df$weather_code,
    wind_mean  = hourly_df$wind_speed_10m,
    gust_max   = hourly_df$wind_gusts_10m
  )
}

# Hourly forecast: hourly= variables on the forecast endpoint. Returns the
# same frame shape as fetch_hourly() (POSIXct `date` column).
fetch_forecast_hourly <- function(lat, lon, days = 7) {
  hourly <- paste(
    "temperature_2m,precipitation,relative_humidity_2m",
    "pressure_msl,cloud_cover,weather_code,wind_speed_10m,wind_gusts_10m",
    sep = ","
  )
  url <- paste0(
    "https://api.open-meteo.com/v1/forecast",
    "?latitude=", lat,
    "&longitude=", lon,
    "&forecast_days=", days,
    "&hourly=", hourly,
    "&timezone=auto"
  )
  raw <- fromJSON(url)
  if (!is.null(raw$error)) {
    stop("API error: ", paste(raw$error, collapse = " "))
  }
  data.frame(
    date       = as.POSIXct(raw$hourly$time, tz = "UTC"),
    temp       = raw$hourly$temperature_2m,
    precip     = raw$hourly$precipitation,
    humidity   = raw$hourly$relative_humidity_2m,
    pressure   = raw$hourly$pressure_msl,
    cloud      = raw$hourly$cloud_cover,
    weather_code = raw$hourly$weather_code,
    wind_mean  = raw$hourly$wind_speed_10m,
    gust_max   = raw$hourly$wind_gusts_10m
  )
}

# Forecast: same daily variables as fetch_precip(), but from the forecast
# endpoint (api.open-meteo.com) with forecast_days (1-16) instead of a
# start/end date range. Returns an identical data.frame shape.
fetch_forecast <- function(lat, lon, days = 7) {
  daily <- paste(
    "precipitation_sum,precipitation_hours",
    "temperature_2m_max,temperature_2m_min,temperature_2m_mean",
    "apparent_temperature_mean",
    "relative_humidity_2m_mean",
    "wind_speed_10m_mean,wind_speed_10m_max,wind_gusts_10m_max",
    "pressure_msl_mean,cloud_cover_mean,weather_code",
    sep = ","
  )
  url <- paste0(
    "https://api.open-meteo.com/v1/forecast",
    "?latitude=", lat,
    "&longitude=", lon,
    "&forecast_days=", days,
    "&daily=", daily,
    "&timezone=auto"
  )
  raw <- fromJSON(url)
  if (!is.null(raw$error)) {
    stop("API error: ", paste(raw$error, collapse = " "))
  }
  data.frame(
    date        = as.Date(raw$daily$time),
    precip      = raw$daily$precipitation_sum,
    hours       = raw$daily$precipitation_hours,
    temp_max    = raw$daily$temperature_2m_max,
    temp_min    = raw$daily$temperature_2m_min,
    temp_mean   = raw$daily$temperature_2m_mean,
    apparent    = raw$daily$apparent_temperature_mean,
    humidity    = raw$daily$relative_humidity_2m_mean,
    wind_mean   = raw$daily$wind_speed_10m_mean,
    wind_max    = raw$daily$wind_speed_10m_max,
    gust_max    = raw$daily$wind_gusts_10m_max,
    pressure    = raw$daily$pressure_msl_mean,
    cloud       = raw$daily$cloud_cover_mean,
    weather_code = raw$daily$weather_code
  )
}

# Plot variables: name -> list of display label and unit. Each plots on its own
# y-axis since the units differ, so the picker shows one at a time.
plot_vars <- list(
  precip    = list(label = "Precipitation (mm)",  unit = "mm"),
  temp_mean = list(label = "Mean temp (°C)",       unit = "°C"),
  temp_max  = list(label = "Max temp (°C)",        unit = "°C"),
  temp_min  = list(label = "Min temp (°C)",        unit = "°C"),
  apparent  = list(label = "Feels-like (°C)",      unit = "°C"),
  humidity  = list(label = "Humidity (%)",         unit = "%"),
  wind_mean = list(label = "Wind (km/h)",          unit = "km/h"),
  wind_max  = list(label = "Max wind (km/h)",      unit = "km/h"),
  gust_max  = list(label = "Gusts (km/h)",         unit = "km/h"),
  pressure  = list(label = "Pressure (hPa)",       unit = "hPa"),
  cloud     = list(label = "Cloud cover (%)",      unit = "%")
)

# Roll up data to the requested granularity. Daily input (Date column)
# groups by calendar period. Hourly input (POSIXct column) is first reduced
# to daily via roll_up_hourly_daily(), except at the "hour" level where
# each hour is its own period.
roll_up <- function(d, level) {
  d <- d |> filter(!is.na(precip))
  if (nrow(d) == 0) return(d)
  d <- d |> arrange(date)
  if (is.POSIXct(d$date)) {
    if (level == "hour") {
      return(d |>
        select(date, precip, temp, humidity, pressure, cloud,
               weather_code, wind_mean, gust_max) |>
        mutate(temp_max = temp, temp_min = temp, temp_mean = temp,
               wind_max = wind_mean, apparent = NA_real_,
               hours  = as.integer(precip > 0.1),
               events = as.integer(precip > 0.01)) |>
        select(period = date, precip, hours, events, temp_max, temp_min,
               temp_mean, apparent, humidity, wind_mean, wind_max,
               gust_max, pressure, cloud, weather_code))
    }
    d <- roll_up_hourly_daily(d)
  }
  period <- switch(level,
                   hour    = d$date,
                   day     = floor_date(d$date, "day"),
                   week    = floor_date(d$date, "week"),
                   month   = floor_date(d$date, "month"),
                   quarter = floor_date(d$date, "quarter"),
                   year    = floor_date(d$date, "year"))
  d$period <- period
  d |>
    group_by(period) |>
    summarise(
      events     = sum(precip > 0.01),   # must run before `precip` is shadowed
      precip     = sum(precip, na.rm = TRUE),
      hours      = sum(hours, na.rm = TRUE),
      temp_max   = max(temp_max,   na.rm = TRUE),
      temp_min   = min(temp_min,   na.rm = TRUE),
      temp_mean  = mean(temp_mean, na.rm = TRUE),
      apparent   = mean(apparent,  na.rm = TRUE),
      humidity   = mean(humidity,  na.rm = TRUE),
      wind_mean  = mean(wind_mean, na.rm = TRUE),
      wind_max   = max(wind_max,   na.rm = TRUE),
      gust_max   = max(gust_max,   na.rm = TRUE),
      pressure   = mean(pressure,  na.rm = TRUE),
      cloud      = mean(cloud,     na.rm = TRUE),
      weather_code = mode_num(weather_code),
      .groups = "drop"
    ) |>
    select(period, precip, hours, events, temp_max, temp_min, temp_mean,
           apparent, humidity, wind_mean, wind_max, gust_max, pressure,
           cloud, weather_code)
}

# Reduce hourly data to a daily frame shaped like fetch_precip()'s output
# (minus `apparent`, which the API doesn't serve hourly), so roll_up() and
# the rest of the pipeline work unchanged.
roll_up_hourly_daily <- function(h) {
  h <- h |> filter(!is.na(precip))
  if (nrow(h) == 0) return(h)
  h$day <- as.Date(h$date)
  h |>
    group_by(day) |>
    summarise(
      date       = first(day),
      events     = sum(precip > 0.01),
      precip     = sum(precip, na.rm = TRUE),
      hours      = sum(precip > 0.1),
      temp_max   = max(temp,     na.rm = TRUE),
      temp_min   = min(temp,     na.rm = TRUE),
      temp_mean  = mean(temp,    na.rm = TRUE),
      apparent   = NA_real_,
      humidity   = mean(humidity,   na.rm = TRUE),
      wind_mean  = mean(wind_mean,  na.rm = TRUE),
      wind_max   = max(wind_mean,   na.rm = TRUE),
      gust_max   = max(gust_max,    na.rm = TRUE),
      pressure   = mean(pressure,   na.rm = TRUE),
      cloud      = mean(cloud,      na.rm = TRUE),
      weather_code = mode_num(weather_code),
      .groups = "drop"
    ) |>
    select(-day)
}

# Most common integer value (used for the roll-up weather code)
mode_num <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  tab <- table(x)
  as.numeric(names(tab)[which.max(tab)])
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
          selectizeInput("source", "Data source",
                         choices = c(
                           "Archive" = "archive",
                           "Hourly"  = "hourly"
                         ),
                         selected = "archive",
                         options = list(dropdownParent = "body")),
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
        card_title("Forecast"),
        card_body(
          selectizeInput("forecast_source", "Resolution",
                         choices = c(
                           "Daily"  = "daily",
                           "Hourly" = "hourly"
                         ),
                         selected = "daily",
                         options = list(dropdownParent = "body")),
          numericInput("forecast_days", "Days ahead", value = 7,
                       min = 1, max = 16, step = 1),
          div(class = "mb-2",
              actionButton("fetch_forecast_btn", "Fetch forecast",
                           icon = icon("cloud-sun"), width = "100%"))
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
                           "Hourly"    = "hour",
                           "Daily"     = "day",
                           "Weekly"    = "week",
                           "Monthly"   = "month",
                           "Quarterly" = "quarter",
                           "Yearly"    = "year"
                         ),
                         selected = "day",
                         options = list(dropdownParent = "body")),
          div(class = "mb-1"),
          selectizeInput("plot_var", "Plot variable",
                         choices = names(plot_vars),
                         selected = "precip",
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
            div(class = "small text-muted", textOutput("vb1_label")),
            div(class = "fs-3 fw-semibold", textOutput("vb_total"))
          ))),
          div(class = "col-3", card(card_body(
            div(class = "small text-muted", "Max period"),
            div(class = "fs-3 fw-semibold", textOutput("vb_max")),
            div(class = "small text-muted", textOutput("vb_maxdate"))
          ))),
          div(class = "col-3", card(card_body(
            div(class = "small text-muted", "Min period"),
            div(class = "fs-3 fw-semibold", textOutput("vb_min")),
            div(class = "small text-muted", textOutput("vb_mindate"))
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

  recs    <- reactiveVal(NULL)
  src     <- reactiveVal("archive")   # "archive" | "hourly" | "forecast"
  status  <- reactiveVal("No data loaded yet. Set a location and press Fetch.")
  place   <- reactiveVal("")

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

    hourly_source <- input$source == "hourly"
    res <- tryCatch(
      if (hourly_source) fetch_hourly(lat, lon, dr[1], dr[2])
      else               fetch_precip(lat, lon, dr[1], dr[2]),
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
    src(if (hourly_source) "hourly" else "archive")
    place(input$place_name %||% paste0(lat, ", ", lon))

    if (hourly_source) {
      # The data is one row per hour; roll_up() reduces it to days.
      d <- roll_up_hourly_daily(res)
      status(sprintf("%d hours (%d days) | %d wet days | %.1f mm total | %s",
                     nrow(res), nrow(d), sum(d$precip > 0.01, na.rm = TRUE),
                     sum(d$precip, na.rm = TRUE), place()))
      toast(sprintf("Loaded %d hours of data", nrow(res)), type = "success")
    } else {
      status(sprintf("%d days | %d wet days | %.1f mm total | %s",
                     nrow(res), sum(res$precip > 0.01, na.rm = TRUE),
                     sum(res$precip, na.rm = TRUE), place()))
      toast(sprintf("Loaded %d days of data", nrow(res)), type = "success")
    }
  })

  # --- Fetch forecast -----------------------------------------------------------
  observeEvent(input$fetch_forecast_btn, {
    lat <- input$lat
    lon <- input$lon
    days <- input$forecast_days

    if (is.null(lat) || is.null(lon) || is.na(lat) || is.na(lon)) {
      toast("Enter valid coordinates", type = "warning")
      return(invisible())
    }
    if (is.null(days) || is.na(days)) {
      toast("Set the number of forecast days", type = "warning")
      return(invisible())
    }

    status("Fetching forecast from Open-Meteo…")

    hourly_forecast <- input$forecast_source == "hourly"
    res <- tryCatch(
      if (hourly_forecast) fetch_forecast_hourly(lat, lon, days)
      else                 fetch_forecast(lat, lon, days),
      error = function(e) {
        status(paste("Error:", conditionMessage(e)))
        NULL
      }
    )

    if (is.null(res)) {
      toast("Forecast fetch failed — check coordinates", type = "danger")
      return(invisible())
    }

    recs(res)
    # "hourly" lets the Hourly granularity show per-hour periods
    src(if (hourly_forecast) "hourly" else "forecast")
    place(input$place_name %||% paste0(lat, ", ", lon))

    if (hourly_forecast) {
      status(sprintf("Forecast: %d hours (%d days) | %.1f mm total | %s",
                     nrow(res), days, sum(res$precip, na.rm = TRUE), place()))
      toast(sprintf("Loaded %d-hour forecast", nrow(res)), type = "success")
    } else {
      status(sprintf("Forecast: %d days | %.1f mm total | %s",
                     nrow(res), sum(res$precip, na.rm = TRUE), place()))
      toast(sprintf("Loaded %d-day forecast", nrow(res)), type = "success")
    }
  })

  # --- Aggregated data ----------------------------------------------------------
  # "hour" is only meaningful for hourly data; for archive/forecast data it
  # falls back to daily grouping.
  lvl <- reactive({
    if (input$rollup == "hour") {
      if (src() == "hourly") "hour" else "day"
    } else input$rollup
  })

  agg <- reactive({
    d <- recs()
    if (is.null(d) || nrow(d) == 0) return(NULL)
    roll_up(d, lvl())
  })

  # --- Stats (follow the selected plot variable) ---------------------------------
  # Per-variable summary: which aggregate (sum vs mean) and the unit.
  summary_word <- function(v) if (v == "precip") "Total" else "Mean"
  var_sum <- function(v, a) switch(v,
    precip = sum(a$precip, na.rm = TRUE),
    temp_mean = mean(a$temp_mean, na.rm = TRUE),
    temp_max = mean(a$temp_max, na.rm = TRUE),
    temp_min = mean(a$temp_min, na.rm = TRUE),
    apparent = mean(a$apparent, na.rm = TRUE),
    humidity = mean(a$humidity, na.rm = TRUE),
    wind_mean = mean(a$wind_mean, na.rm = TRUE),
    wind_max = max(a$wind_max, na.rm = TRUE),
    gust_max = max(a$gust_max, na.rm = TRUE),
    pressure = mean(a$pressure, na.rm = TRUE),
    cloud = mean(a$cloud, na.rm = TRUE))
  var_unit <- function(v) plot_vars[[v]]$unit
  fmt <- function(x, unit) {
    if (is.na(x)) return("—")
    sprintf("%.1f %s", x, unit)
  }
  fmt_period <- function(p) switch(lvl(),
    hour    = format(p, "%b %d, %Y %H:%M"),
    day     = format(p, "%b %d, %Y"),
    week    = paste("wk of", format(p, "%b %d")),
    month   = format(p, "%b %Y"),
    quarter = sprintf("Q%d %d", quarter(p), year(p)),
    year    = format(p, "%Y"))

  output$vb1_label <- renderText({
    v <- input$plot_var
    paste0(summary_word(v), " ", plot_vars[[v]]$label)
  })
  output$vb_total <- renderText({
    a <- agg()
    if (is.null(a)) return("—")
    fmt(var_sum(input$plot_var, a), var_unit(input$plot_var))
  })
  output$vb_max <- renderText({
    a <- agg()
    if (is.null(a)) return("—")
    v <- input$plot_var
    fmt(max(a[[v]], na.rm = TRUE), var_unit(v))
  })
  output$vb_maxdate <- renderText({
    a <- agg()
    if (is.null(a)) return("")
    v <- input$plot_var
    fmt_period(a$period[which.max(a[[v]])])
  })
  output$vb_min <- renderText({
    a <- agg()
    if (is.null(a)) return("—")
    v <- input$plot_var
    fmt(min(a[[v]], na.rm = TRUE), var_unit(v))
  })
  output$vb_mindate <- renderText({
    a <- agg()
    if (is.null(a)) return("")
    v <- input$plot_var
    fmt_period(a$period[which.min(a[[v]])])
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

    varname <- input$plot_var
    meta <- plot_vars[[varname]]
    a <- a |> mutate(plot_val = .data[[varname]])

    # Precipitation and wind look best as bars for coarser periods; continuous
    # variables like temperature read better as lines.
    bar_vars <- c("precip", "wind_max", "gust_max")
    use_bar <- varname %in% bar_vars && lvl() %in% c("quarter", "year")

    p <- ggplot(a, aes(x = period, y = plot_val))
    p <- if (use_bar) {
      p + geom_col(fill = "#2e6fb5", width = 0.8) +
        geom_point(color = "#2e6fb5", size = 2)
    } else {
      p + geom_line(color = "#2e6fb5", linewidth = 0.7) +
        geom_point(color = "#2e6fb5", size = 1.5)
    }

    p +
      labs(x = NULL, y = meta$label,
           title = paste0(meta$label, " — ", place(), " (", lvl(), ")")) +
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
