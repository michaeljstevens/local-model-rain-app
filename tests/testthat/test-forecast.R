# Tests for fetch_forecast(). fromJSON is mocked, so no network is needed.

mock_forecast <- function(payload, days = 7) {
  local({
    stub <- new.env()
    stub$fromJSON <- function(url, ...) payload
    testthat::with_mocked_bindings(
      fromJSON = stub$fromJSON,
      .package = "jsonlite",
      code = fetch_forecast(40.71, -74.01, days)
    )
  })
}

make_forecast_payload <- function(n = 7) {
  list(
    daily = list(
      time = as.Date("2026-11-24") + seq_len(n) - 1,
      precipitation_sum = rep(0.5, n),
      precipitation_hours = rep(1, n),
      temperature_2m_max = rep(12.0, n),
      temperature_2m_min = rep(4.0, n),
      temperature_2m_mean = rep(8.0, n),
      apparent_temperature_mean = rep(6.0, n),
      relative_humidity_2m_mean = rep(65, n),
      wind_speed_10m_mean = rep(10.0, n),
      wind_speed_10m_max = rep(22.0, n),
      wind_gusts_10m_max = rep(34.0, n),
      pressure_msl_mean = rep(1012.0, n),
      cloud_cover_mean = rep(40, n),
      weather_code = rep(3, n)
    )
  )
}

test_that("fetch_forecast parses daily payload into a data frame", {
  d <- mock_forecast(make_forecast_payload(5))
  expect_true(is.data.frame(d))
  expect_equal(nrow(d), 5)
  expect_equal(ncol(d), 14)
  expect_true(all(sapply(d$date, is.Date)))
  expect_equal(d$date, as.Date("2026-11-24") + 0:4)
  expect_equal(d$precip, rep(0.5, 5))
})

test_that("fetch_forecast returns all 14 columns with correct names", {
  d <- mock_forecast(make_forecast_payload(2))
  expect_equal(ncol(d), 14)
  expect_named(d, c("date", "precip", "hours",
                    "temp_max", "temp_min", "temp_mean",
                    "apparent", "humidity",
                    "wind_mean", "wind_max", "gust_max",
                    "pressure", "cloud", "weather_code"))
})

test_that("fetch_forecast surfaces API errors as a stop()", {
  expect_error(mock_forecast(list(error = TRUE, reason = "bad coordinates")),
               "API error")
})

test_that("fetch_forecast builds correct URL with forecast_days", {
  captured_url <- NULL
  stub <- new.env()
  stub$fromJSON <- function(url, ...) {
    captured_url <<- url
    list(daily = list(time = "2026-11-24",
                      precipitation_sum = 1, precipitation_hours = 0,
                      temperature_2m_max = 10, temperature_2m_min = 5,
                      temperature_2m_mean = 7.5,
                      apparent_temperature_mean = 6,
                      relative_humidity_2m_mean = 70,
                      wind_speed_10m_mean = 5, wind_speed_10m_max = 15,
                      wind_gusts_10m_max = 25,
                      pressure_msl_mean = 1013, cloud_cover_mean = 40,
                      weather_code = 2))
  }
  testthat::with_mocked_bindings(
    fromJSON = stub$fromJSON,
    .package = "jsonlite",
    code = fetch_forecast(48.85, 2.35, 10)
  )
  expect_match(captured_url, "api.open-meteo.com/v1/forecast")
  expect_match(captured_url, "latitude=48.85")
  expect_match(captured_url, "longitude=2.35")
  expect_match(captured_url, "forecast_days=10")
  expect_match(captured_url, "timezone=auto")
  expect_match(captured_url, "precipitation_sum")
  expect_match(captured_url, "weather_code")
  expect_no_match(captured_url, "start_date")
})

test_that("fetch_forecast output is compatible with roll_up()", {
  d <- mock_forecast(make_forecast_payload(7))
  a <- roll_up(d, "week")
  expect_true(is.data.frame(a))
  expect_true(nrow(a) >= 1)
  expect_true("precip" %in% names(a))
})
