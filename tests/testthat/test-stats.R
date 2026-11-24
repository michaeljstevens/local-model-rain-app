# Tests for the server-side stat cards: the per-variable summary
# (var_sum: sum for precip, mean/max for everything else), the unit
# formatting (fmt), and the period labels (fmt_period).
#
# The cards are driven by input$plot_var + input$rollup over the fetched
# daily data, so we inject a deterministic daily frame into app_env$recs
# before the server starts and assert on the rendered text outputs. The
# expected values are computed through the real roll_up() with the same
# formatting the server uses, so the tests pin behavior, not arithmetic.

pv_make_full_daily <- function(start, n, seed = 7) {
  set.seed(seed)
  data.frame(
    date         = seq(as.Date(start), by = 1, length.out = n),
    precip       = runif(n, 0, 10),
    hours        = sample(0:14, n, replace = TRUE),
    temp_max     = runif(n, 5, 30),
    temp_min     = runif(n, -5, 15),
    temp_mean    = runif(n, 0, 25),
    apparent     = runif(n, -2, 28),
    humidity     = runif(n, 20, 100),
    wind_mean    = runif(n, 0, 40),
    wind_max     = runif(n, 5, 80),
    gust_max     = runif(n, 10, 120),
    pressure     = runif(n, 980, 1040),
    cloud        = runif(n, 0, 100),
    weather_code = sample(0:9, n, replace = TRUE)
  )
}

# Same formatting the server uses (copy — the server's own closure holds
# the originals; we re-derive expected strings with identical logic).
pv_var_sum <- function(v, a) switch(v,
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
pv_fmt <- function(x, unit) if (is.na(x)) "—" else sprintf("%.1f %s", x, unit)
pv_fmt_period <- function(p, lvl) switch(lvl,
  day     = format(p, "%b %d, %Y"),
  week    = paste("wk of", format(p, "%b %d")),
  month   = format(p, "%b %Y"),
  quarter = sprintf("Q%d %d", lubridate::quarter(p), lubridate::year(p)),
  year    = format(p, "%Y"))

# Clone the app's server with toast() no-oped (needs an active session) and
# drive it with a mock session. The server's first statement creates a
# *local* `recs <- reactiveVal(NULL)`, which would shadow any `recs` defined
# in a parent env — so we patch the cloned body to initialise `recs` from
# `app_env$pv_injected`, which this test sets beforehand. (The server
# function's enclosure is app_env, so the injected frame must live there.)
run_pv_stats <- function(recs_df, verify) {
  pv_ts <- function(input, output, session) {}
  bl <- as.list(body(app_env$server))
  is_recs_init <- vapply(bl[-1], function(x)
    is.call(x) && identical(x[[1]], as.name("<-")) &&
    identical(x[[2]], quote(recs)) &&
    identical(x[[3]], quote(reactiveVal(NULL))), logical(1))
  if (!any(is_recs_init)) stop("recs init not found in server body")
  bl[[which(is_recs_init)[1] + 1]] <- quote(recs <- reactiveVal(app_env$pv_injected))
  body(pv_ts) <- as.call(bl)
  pv_env <- new.env(parent = app_env)
  environment(pv_ts) <- pv_env
  pv_env$toast <- function(...) invisible(NULL)
  app_env$pv_injected <- recs_df
  on.exit(rm("pv_injected", envir = app_env), add = TRUE)
  pv_sess <- shiny::MockShinySession$new()
  on.exit(withr::defer(pv_sess$close()), add = TRUE)
  shiny::testServer(pv_ts, {
    pv_sess$setInputs(rollup = "month", plot_var = "precip")
    pv_sess$elapse(0.1)
    pv_sess$flushReact()
    verify(pv_sess)
  }, session = pv_sess)
}

test_that("stat cards show total, max/min with dates, and period count", {
  pv_d <- pv_make_full_daily("2026-01-01", 41)  # Jan 1 - Feb 10
  run_pv_stats(pv_d, function(pv_ms) {
    a <- roll_up(pv_d, "month")
    expect_equal(pv_ms$output$vb1_label, "Total Precipitation (mm)")
    expect_equal(pv_ms$output$vb_total,
                 pv_fmt(sum(a$precip, na.rm = TRUE), "mm"))
    expect_equal(pv_ms$output$vb_max,
                 pv_fmt(max(a$precip, na.rm = TRUE), "mm"))
    expect_equal(pv_ms$output$vb_maxdate,
                 pv_fmt_period(a$period[which.max(a$precip)], "month"))
    expect_equal(pv_ms$output$vb_min,
                 pv_fmt(min(a$precip, na.rm = TRUE), "mm"))
    expect_equal(pv_ms$output$vb_mindate,
                 pv_fmt_period(a$period[which.min(a$precip)], "month"))
    expect_equal(pv_ms$output$vb_n, as.character(nrow(a)))
    # Two monthly periods
    expect_equal(pv_fmt_period(a$period, "month"),
                 c("Jan 2026", "Feb 2026"))
  })
})

test_that("temp_mean cards show the mean, not the sum", {
  pv_d <- pv_make_full_daily("2026-01-01", 41)
  run_pv_stats(pv_d, function(pv_ms) {
    pv_ms$setInputs(plot_var = "temp_mean")
    pv_ms$elapse(0.1)
    pv_ms$flushReact()
    a <- roll_up(pv_d, "month")
    expect_equal(pv_ms$output$vb1_label, "Mean Mean temp (°C)")
    expect_equal(pv_ms$output$vb_total,
                 pv_fmt(mean(a$temp_mean, na.rm = TRUE), "°C"))
    expect_equal(pv_ms$output$vb_max,
                 pv_fmt(max(a$temp_mean, na.rm = TRUE), "°C"))
    expect_equal(pv_ms$output$vb_min,
                 pv_fmt(min(a$temp_mean, na.rm = TRUE), "°C"))
  })
})

test_that("wind_max cards use the max-of-periods aggregate", {
  pv_d <- pv_make_full_daily("2026-01-01", 41)
  run_pv_stats(pv_d, function(pv_ms) {
    pv_ms$setInputs(plot_var = "wind_max")
    pv_ms$elapse(0.1)
    pv_ms$flushReact()
    a <- roll_up(pv_d, "month")
    expect_equal(pv_ms$output$vb1_label, "Mean Max wind (km/h)")
    # var_sum takes max(a$wind_max) — the max across monthly periods,
    # which equals the overall max here
    expect_equal(pv_ms$output$vb_total,
                 pv_fmt(max(a$wind_max, na.rm = TRUE), "km/h"))
  })
})

test_that("fmt_period renders each granularity correctly", {
  pv_d <- pv_make_full_daily("2026-01-01", 41)
  # Build one frame of known period starts per granularity
  runs <- list(
    list(lvl = "day",     sel = c(2, 41)),   # Jan 2, Feb 10
    list(lvl = "week",    sel = c(2, 29)),  # wk of Jan 2, wk of Feb 8
    list(lvl = "month",   sel = c(1, 32)),  # Jan, Feb
    list(lvl = "quarter", sel = c(1, 32)),  # Q1
    list(lvl = "year",    sel = c(1, 32))   # 2026
  )
  for (r in runs) {
    run_pv_stats(pv_d, function(pv_ms) {
      a <- roll_up(pv_d, r$lvl)
      pv_ms$setInputs(rollup = r$lvl)
      pv_ms$elapse(0.1)
      pv_ms$flushReact()
      # max/min dates come from the same fmt_period as the server's
      maxdate <- pv_fmt_period(a$period[which.max(a$precip)], r$lvl)
      mindate <- pv_fmt_period(a$period[which.min(a$precip)], r$lvl)
      expect_equal(pv_ms$output$vb_maxdate, maxdate)
      expect_equal(pv_ms$output$vb_mindate, mindate)
    })
  }
  # Spot-check the label shapes directly
  expect_equal(pv_fmt_period(as.Date("2026-01-02"), "day"), "Jan 02, 2026")
  expect_equal(pv_fmt_period(as.Date("2026-01-02"), "week"), "wk of Jan 02")
  expect_equal(pv_fmt_period(as.Date("2026-01-02"), "month"), "Jan 2026")
  expect_equal(pv_fmt_period(as.Date("2026-04-15"), "quarter"), "Q2 2026")
  expect_equal(pv_fmt_period(as.Date("2026-04-15"), "year"), "2026")
})

test_that("cards show em-dash placeholders when no data is loaded", {
  run_pv_stats(NULL, function(pv_ms) {
    expect_equal(pv_ms$output$vb_total, "—")
    expect_equal(pv_ms$output$vb_max, "—")
    expect_equal(pv_ms$output$vb_min, "—")
    expect_equal(pv_ms$output$vb_n, "—")
    expect_equal(pv_ms$output$vb_maxdate, "")
    expect_equal(pv_ms$output$vb_mindate, "")
  })
})


test_that("precip_convert and precip_fmt convert mm to inches", {
  expect_equal(precip_convert(25.4, "mm"), 25.4)
  expect_equal(precip_convert(c(25.4, 50.8), "in"), c(1, 2))
  expect_equal(precip_fmt(12.34, "mm"), "12.3 mm")
  expect_equal(precip_fmt(25.4, "in"), "1.00 in")
  expect_equal(precip_fmt(12.7, "in"), "0.50 in")
})

test_that("precip stat cards follow the inches toggle", {
  pv_d <- pv_make_full_daily("2026-01-01", 41)
  run_pv_stats(pv_d, function(pv_ms) {
    a <- roll_up(pv_d, "month")
    # Default is mm
    expect_equal(pv_ms$output$vb1_label, "Total Precipitation (mm)")
    pv_ms$setInputs(precip_units = "in")
    pv_ms$elapse(0.1)
    pv_ms$flushReact()
    expect_equal(pv_ms$output$vb1_label, "Total Precipitation (in)")
    expect_equal(pv_ms$output$vb_total,
                 sprintf("%.2f in", sum(a$precip) / 25.4))
    expect_equal(pv_ms$output$vb_max,
                 sprintf("%.2f in", max(a$precip) / 25.4))
    expect_equal(pv_ms$output$vb_min,
                 sprintf("%.2f in", min(a$precip) / 25.4))
    # Back to mm
    pv_ms$setInputs(precip_units = "mm")
    pv_ms$elapse(0.1)
    pv_ms$flushReact()
    expect_equal(pv_ms$output$vb_total,
                 sprintf("%.1f mm", sum(a$precip)))
  })
})

test_that("non-precip variables ignore the units toggle", {
  pv_d <- pv_make_full_daily("2026-01-01", 41)
  run_pv_stats(pv_d, function(pv_ms) {
    pv_ms$setInputs(plot_var = "temp_mean", precip_units = "in")
    pv_ms$elapse(0.1)
    pv_ms$flushReact()
    a <- roll_up(pv_d, "month")
    expect_equal(pv_ms$output$vb1_label, "Mean Mean temp (°C)")
    expect_equal(pv_ms$output$vb_total,
                 sprintf("%.1f °C", mean(a$temp_mean)))
  })
})
