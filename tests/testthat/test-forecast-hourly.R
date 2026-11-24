# Tests for fetch_forecast_hourly(). fromJSON is mocked, so no network.

mock_forecast_hourly <- function(payload, days = 7) {
  local({
    stub <- new.env()
    stub$fromJSON <- function(url, ...) payload
    testthat::with_mocked_bindings(
      fromJSON = stub$fromJSON,
      .package = "jsonlite",
      code = fetch_forecast_hourly(40.71, -74.01, days)
    )
  })
}

make_forecast_hourly_payload <- function(n = 24) {
  list(
    hourly = list(
      time = format(seq(as.POSIXct("2026-11-24 00:00:00", tz = "UTC"),
                       by = "hour", length.out = n), "%Y-%m-%d %H:%M"),
      temperature_2m = rep(5.5, n),
      precipitation = c(rep(0, n - 1), 1.5),
      relative_humidity_2m = rep(70, n),
      pressure_msl = rep(1015.0, n),
      cloud_cover = rep(60, n),
      weather_code = rep(3, n),
      wind_speed_10m = rep(12.0, n),
      wind_gusts_10m = rep(24.0, n)
    )
  )
}

test_that("fetch_forecast_hourly parses payload into a data frame", {
  d <- mock_forecast_hourly(make_forecast_hourly_payload(12))
  expect_true(is.data.frame(d))
  expect_equal(nrow(d), 12)
  expect_named(d, c("date", "temp", "precip", "humidity", "pressure",
                    "cloud", "weather_code", "wind_mean", "gust_max"))
  expect_true(is.POSIXct(d$date))
  expect_equal(d$date[1], as.POSIXct("2026-11-24 00:00:00", tz = "UTC"))
  expect_equal(d$precip, c(rep(0, 11), 1.5))
})

test_that("fetch_forecast_hourly surfaces API errors as a stop()", {
  expect_error(mock_forecast_hourly(list(error = TRUE, reason = "bad coordinates")),
               "API error")
})

test_that("fetch_forecast_hourly builds correct URL", {
  captured_url <- NULL
  stub <- new.env()
  stub$fromJSON <- function(url, ...) {
    captured_url <<- url
    list(hourly = list(time = "2026-11-24 00:00:00",
                       temperature_2m = 5, precipitation = 0,
                       relative_humidity_2m = 70, pressure_msl = 1015,
                       cloud_cover = 60, weather_code = 3,
                       wind_speed_10m = 12, wind_gusts_10m = 24))
  }
  testthat::with_mocked_bindings(
    fromJSON = stub$fromJSON,
    .package = "jsonlite",
    code = fetch_forecast_hourly(48.85, 2.35, 3)
  )
  expect_match(captured_url, "api.open-meteo.com/v1/forecast")
  expect_match(captured_url, "latitude=48.85")
  expect_match(captured_url, "longitude=2.35")
  expect_match(captured_url, "forecast_days=3")
  expect_match(captured_url, "temperature_2m")
  expect_match(captured_url, "wind_gusts_10m")
  expect_no_match(captured_url, "daily=")
})

test_that("fetch_forecast_hourly output works through roll_up()", {
  d <- mock_forecast_hourly(make_forecast_hourly_payload(48))
  # at the hour level: one row per hour
  a <- roll_up(d, "hour")
  expect_equal(nrow(a), 48)
  expect_true(is.POSIXct(a$period))
  # at the day level: reduced to daily first
  b <- roll_up(d, "day")
  expect_equal(sum(b$precip), 1.5)
})
