# Tests for the hourly data path: fetch_hourly() (mocked), the hourly ->
# daily reduction (roll_up_hourly_daily()), and roll_up() at the "hour"
# level.

mock_hourly <- function(payload, days = 3) {
  local({
    stub <- new.env()
    stub$fromJSON <- function(url, ...) payload
    testthat::with_mocked_bindings(
      fromJSON = stub$fromJSON,
      .package = "jsonlite",
      code = fetch_hourly(40.71, -74.01, as.Date("2026-01-01"), as.Date("2026-01-03"))
    )
  })
}

make_hourly_payload <- function(n = 72) {
  times <- seq(as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
               by = "hour", length.out = n)
  list(
    hourly = list(
      time = format(times, "%Y-%m-%d %H:%M"),
      temperature_2m = seq(0, n - 1) %% 10 + 1,
      precipitation = c(rep(0, 6), 2.0, rep(0, n - 7)),
      relative_humidity_2m = rep(50, n),
      pressure_msl = rep(1010.0, n),
      cloud_cover = rep(30, n),
      weather_code = rep(1, n),
      wind_speed_10m = rep(8.0, n),
      wind_gusts_10m = rep(16.0, n)
    )
  )
}

test_that("fetch_hourly parses payload into a data frame", {
  d <- mock_hourly(make_hourly_payload(48))
  expect_true(is.data.frame(d))
  expect_equal(nrow(d), 48)
  expect_named(d, c("date", "temp", "precip", "humidity", "pressure",
                    "cloud", "weather_code", "wind_mean", "gust_max"))
  expect_true(is.POSIXct(d$date))
  expect_equal(d$date[1], as.POSIXct("2026-01-01 00:00:00", tz = "UTC"))
})

test_that("fetch_hourly surfaces API errors as a stop()", {
  expect_error(mock_hourly(list(error = TRUE, reason = "bad coordinates")),
               "API error")
})

test_that("fetch_hourly builds correct URL", {
  captured_url <- NULL
  stub <- new.env()
  stub$fromJSON <- function(url, ...) {
    captured_url <<- url
    list(hourly = list(time = "2026-01-01 00:00:00",
                       temperature_2m = 5, precipitation = 0,
                       relative_humidity_2m = 50, pressure_msl = 1010,
                       cloud_cover = 30, weather_code = 1,
                       wind_speed_10m = 8, wind_gusts_10m = 16))
  }
  testthat::with_mocked_bindings(
    fromJSON = stub$fromJSON,
    .package = "jsonlite",
    code = fetch_hourly(48.85, 2.35, as.Date("2025-06-01"), as.Date("2025-06-03"))
  )
  expect_match(captured_url, "archive-api.open-meteo.com/v1/archive")
  expect_match(captured_url, "latitude=48.85")
  expect_match(captured_url, "longitude=2.35")
  expect_match(captured_url, "start_date=2025-06-01")
  expect_match(captured_url, "end_date=2025-06-03")
  expect_match(captured_url, "temperature_2m")
  expect_match(captured_url, "wind_gusts_10m")
  expect_no_match(captured_url, "daily=")
})

# A small hourly frame: 2 days x 3 hours (kept small; shape is what matters)
make_hourly_df <- function() {
  data.frame(
    date = as.POSIXct(c("2026-01-01 00:00:00", "2026-01-01 01:00:00",
                       "2026-01-01 02:00:00", "2026-01-02 00:00:00",
                       "2026-01-02 01:00:00", "2026-01-02 02:00:00"),
                       tz = "UTC"),
    temp       = c(1, 2, 3, 4, 5, 6),
    precip     = c(0, 1, 0, 2, 0, 0),
    humidity   = rep(50, 6),
    pressure   = rep(1010, 6),
    cloud      = rep(30, 6),
    weather_code = rep(1, 6),
    wind_mean  = rep(8, 6),
    gust_max   = rep(16, 6)
  )
}

test_that("roll_up_hourly_daily reduces hourly data to a daily frame", {
  d <- roll_up_hourly_daily(make_hourly_df())
  expect_equal(nrow(d), 2)
  expect_true(all(sapply(d$date, is.Date)))
  expect_equal(names(d), c("date", "events", "precip", "hours", "temp_max",
                           "temp_min", "temp_mean", "apparent", "humidity",
                           "wind_mean", "wind_max", "gust_max", "pressure",
                           "cloud", "weather_code"))
  expect_equal(d$precip, c(1, 2))
  expect_equal(d$hours, c(1, 1))
  expect_equal(d$events, c(1, 1))
  expect_equal(d$temp_max, c(3, 6))
  expect_equal(d$temp_min, c(1, 4))
  expect_equal(d$temp_mean, c(2, 5))
  expect_true(all(is.na(d$apparent)))
  expect_equal(d$wind_max, c(8, 8))
})

test_that("roll_up at 'hour' level returns one row per hour", {
  h <- make_hourly_df()
  a <- roll_up(h, "hour")
  expect_equal(nrow(a), 6)
  expect_true(is.POSIXct(a$period))
  expect_equal(a$precip, h$precip)
  expect_equal(a$temp_max, h$temp)
  expect_equal(a$wind_max, h$wind_mean)
  expect_true(all(is.na(a$apparent)))
})

test_that("roll_up on hourly data at daily levels matches the daily frame", {
  h <- make_hourly_df()
  for (lvl in c("day", "week", "month")) {
    a <- roll_up(h, lvl)
    d <- roll_up_hourly_daily(h) |> roll_up(lvl)
    expect_equal(a$precip, d$precip, label = lvl)
    expect_equal(a$temp_max, d$temp_max, label = lvl)
  }
})

test_that("roll_up on hourly data falls back gracefully for coarse levels", {
  h <- make_hourly_df()
  a <- roll_up(h, "year")
  expect_equal(nrow(a), 1)
  expect_equal(a$precip, 3)
})
