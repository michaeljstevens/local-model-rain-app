# Tests for fetch_precip(). We mock fromJSON so no network is needed.

mock_fetch <- function(payload) {
  local({
    stub <- new.env()
    stub$fromJSON <- function(url, ...) payload
    testthat::with_mocked_bindings(
      fromJSON = stub$fromJSON,
      .package = "jsonlite",
      code = fetch_precip(40.71, -74.01, as.Date("2026-01-01"), as.Date("2026-01-03"))
    )
  })
}

test_that("fetch_precip parses daily payload into a data frame", {
  payload <- list(
    daily = list(
      time = c("2026-01-01", "2026-01-02", "2026-01-03"),
      precipitation_sum = c(1.0, 2.5, 0.0),
      precipitation_hours = c(3, 1, 0),
      temperature_2m_max = c(15, 16, 14),
      temperature_2m_min = c(8, 9, 7),
      temperature_2m_mean = c(11.5, 12.5, 10.5),
      apparent_temperature_mean = c(10, 11, 9),
      relative_humidity_2m_mean = c(60, 65, 55),
      wind_speed_10m_mean = c(10, 12, 8),
      wind_speed_10m_max = c(20, 25, 18),
      wind_gusts_10m_max = c(30, 35, 28),
      pressure_msl_mean = c(1013, 1012, 1014),
      cloud_cover_mean = c(50, 60, 40),
      weather_code = c(1, 2, 0)
    )
  )
  d <- mock_fetch(payload)
  expect_true(is.data.frame(d))
  expect_equal(nrow(d), 3)
  expect_equal(ncol(d), 14)
  expect_true(all(sapply(d$date, is.Date)))
  expect_equal(d$precip, c(1.0, 2.5, 0.0))
  expect_equal(d$hours, c(3, 1, 0))
})

test_that("fetch_precip surfaces API errors as a stop()", {
  payload <- list(error = TRUE, reason = "bad coordinates")
  expect_error(mock_fetch(payload), "API error")
})

test_that("fetch_precip returns all 14 columns with correct names", {
  payload <- list(
    daily = list(
      time = c("2026-01-01", "2026-01-02"),
      precipitation_sum = c(1.0, 2.0),
      precipitation_hours = c(1, 2),
      temperature_2m_max = c(15.0, 16.0),
      temperature_2m_min = c(8.0, 9.0),
      temperature_2m_mean = c(11.5, 12.5),
      apparent_temperature_mean = c(10.0, 11.0),
      relative_humidity_2m_mean = c(60.0, 65.0),
      wind_speed_10m_mean = c(10.0, 12.0),
      wind_speed_10m_max = c(20.0, 25.0),
      wind_gusts_10m_max = c(30.0, 35.0),
      pressure_msl_mean = c(1013.0, 1012.0),
      cloud_cover_mean = c(50.0, 60.0),
      weather_code = c(1, 2)
    )
  )
  d <- mock_fetch(payload)
  expect_equal(ncol(d), 14)
  expect_named(d, c("date", "precip", "hours",
                    "temp_max", "temp_min", "temp_mean",
                    "apparent", "humidity",
                    "wind_mean", "wind_max", "gust_max",
                    "pressure", "cloud", "weather_code"))
  expect_equal(d$precip, c(1.0, 2.0))
  expect_equal(d$temp_max, c(15.0, 16.0))
  expect_equal(d$temp_min, c(8.0, 9.0))
  expect_equal(d$temp_mean, c(11.5, 12.5))
  expect_equal(d$apparent, c(10.0, 11.0))
  expect_equal(d$humidity, c(60.0, 65.0))
  expect_equal(d$wind_mean, c(10.0, 12.0))
  expect_equal(d$wind_max, c(20.0, 25.0))
  expect_equal(d$gust_max, c(30.0, 35.0))
  expect_equal(d$pressure, c(1013.0, 1012.0))
  expect_equal(d$cloud, c(50.0, 60.0))
  expect_equal(d$weather_code, c(1, 2))
})

test_that("fetch_precip preserves NA values from the API", {
  # fromJSON converts the API's `null` to NA; simulate that here. (Raw NULLs
  # in a vector are dropped by data.frame(), so they can't be exercised
  # through the mocked payload.)
  payload <- list(
    daily = list(
      time = c("2026-01-01", "2026-01-02"),
      precipitation_sum = c(1.0, NA),
      precipitation_hours = c(1, NA),
      temperature_2m_max = c(15.0, NA),
      temperature_2m_min = c(8.0, NA),
      temperature_2m_mean = c(11.5, NA),
      apparent_temperature_mean = c(10.0, NA),
      relative_humidity_2m_mean = c(60.0, NA),
      wind_speed_10m_mean = c(10.0, NA),
      wind_speed_10m_max = c(20.0, NA),
      wind_gusts_10m_max = c(30.0, NA),
      pressure_msl_mean = c(1013.0, NA),
      cloud_cover_mean = c(50.0, NA),
      weather_code = c(1, NA)
    )
  )
  d <- mock_fetch(payload)
  expect_equal(nrow(d), 2)
  expect_true(is.na(d$precip[2]))
  expect_true(is.na(d$temp_max[2]))
  expect_true(is.na(d$weather_code[2]))
  # First row should be intact
  expect_equal(d$precip[1], 1.0)
  expect_equal(d$temp_max[1], 15.0)
})

test_that("fetch_precip builds correct URL with date range", {
  captured_url <- NULL
  stub <- new.env()
  stub$fromJSON <- function(url, ...) {
    captured_url <<- url
    list(daily = list(time = "2026-01-01",
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
    code = fetch_precip(48.85, 2.35, as.Date("2025-06-01"), as.Date("2025-06-30"))
  )
  expect_match(captured_url, "latitude=48.85")
  expect_match(captured_url, "longitude=2.35")
  expect_match(captured_url, "start_date=2025-06-01")
  expect_match(captured_url, "end_date=2025-06-30")
  expect_match(captured_url, "timezone=auto")
  expect_match(captured_url, "precipitation_sum")
  expect_match(captured_url, "weather_code")
})
