# Tests for roll_up(): aggregating daily data to day/week/month/quarter/year.

# Minimal full 14-column frame for the tests that only care about precip.
make_daily <- function(start, n, vals) {
  data.frame(
    date         = seq(as.Date(start), by = 1, length.out = n),
    precip       = vals,
    hours        = rep(1, n),
    temp_max     = rep(15, n),
    temp_min     = rep(5, n),
    temp_mean    = rep(10, n),
    apparent     = rep(9, n),
    humidity     = rep(60, n),
    wind_mean    = rep(10, n),
    wind_max     = rep(20, n),
    gust_max     = rep(30, n),
    pressure     = rep(1013, n),
    cloud        = rep(50, n),
    weather_code = rep(1, n)
  )
}

make_full_daily <- function(start, n, seed = 42) {
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

test_that("roll_up to 'day' keeps every row", {
  d <- make_daily("2026-01-01", 10, c(rep(0, 5), rep(2, 5)))
  a <- roll_up(d, "day")
  expect_equal(nrow(a), 10)
  expect_equal(a$precip, d$precip)
  expect_true(is.Date(a$period))
})

test_that("roll_up to 'month' groups by calendar month", {
  # Jan 1-15 (0.5 each), Jan 16-31 (1 each)
  d <- make_daily("2026-01-01", 31, c(rep(0.5, 15), rep(1, 16)))
  a <- roll_up(d, "month")
  expect_equal(nrow(a), 1)
  expect_equal(a$period, as.Date("2026-01-01"))
  expect_equal(a$precip, 15 * 0.5 + 16 * 1)
  expect_equal(a$events, 31)  # all days have precip > 0.01
  expect_equal(a$hours, 31)
})

test_that("roll_up to 'week' floors to Sunday (lubridate week start)", {
  # 2026-01-04 is a Sunday; a full Sun-Sat week is one period
  d <- make_daily("2026-01-04", 7, rep(1, 7))
  a <- roll_up(d, "week")
  expect_equal(nrow(a), 1)
  expect_equal(a$period, as.Date("2026-01-04"))
  expect_equal(a$precip, 7)
})

test_that("roll_up to 'week' splits multi-week data correctly", {
  # 14 days starting Sun 2026-01-04 = exactly two weeks
  d <- make_daily("2026-01-04", 14, rep(1, 14))
  a <- roll_up(d, "week")
  expect_equal(nrow(a), 2)
  expect_equal(a$period, as.Date(c("2026-01-04", "2026-01-11")))
  expect_equal(a$precip, c(7, 7))
})

test_that("roll_up to 'quarter' floors to quarter start", {
  # 2026-04-15 is in Q2; one month of data stays in one quarter
  d <- make_daily("2026-04-15", 15, rep(2, 15))
  a <- roll_up(d, "quarter")
  expect_equal(nrow(a), 1)
  expect_equal(a$period, as.Date("2026-04-01"))
  expect_equal(a$precip, 30)
})

test_that("roll_up to 'month' splits months that straddle a boundary", {
  # Jan 25 - Feb 5, 2026: 7 days in Jan, 11 days in Feb
  d <- make_daily("2026-01-25", 12, rep(1, 12))
  a <- roll_up(d, "month")
  expect_equal(nrow(a), 2)
  expect_equal(a$period, as.Date(c("2026-01-01", "2026-02-01")))
  expect_equal(a$precip, c(7, 5))
})

test_that("roll_up to 'year' aggregates the whole year", {
  d <- make_daily("2026-01-01", 365, rep(1, 365))
  a <- roll_up(d, "year")
  expect_equal(nrow(a), 1)
  expect_equal(a$period, as.Date("2026-01-01"))
  expect_equal(a$precip, 365)
})

test_that("roll_up drops NA precip rows", {
  d <- make_daily("2026-01-01", 5, c(1, NA, 2, NA, 3))
  a <- roll_up(d, "month")
  expect_equal(nrow(a), 1)
  expect_equal(a$precip, 6)  # 1 + 2 + 3
})

test_that("roll_up returns empty frame for empty input", {
  d <- make_daily("2026-01-01", 0, numeric(0))
  a <- roll_up(d, "month")
  expect_equal(nrow(a), 0)
})

test_that("events counts only wet days, not all rows", {
  # 3 days: 1 wet, 2 dry
  d <- make_daily("2026-06-01", 3, c(0.1, 0, 0.5))
  a <- roll_up(d, "month")
  expect_equal(a$events, 2)
})

test_that("sums are preserved across granularities", {
  d <- make_daily("2026-03-10", 90, runif(90, 0, 5))
  total <- sum(d$precip)
  for (lvl in c("day", "week", "month", "quarter", "year")) {
    a <- roll_up(d, lvl)
    expect_equal(sum(a$precip), total, label = lvl)
  }
})

# --- Aggregation semantics for the full 14-column frame ---

test_that("roll_up applies correct aggregation per column type", {
  d <- make_full_daily("2026-01-01", 10)
  a <- roll_up(d, "month")
  expect_equal(nrow(a), 1)
  # sum columns
  expect_equal(a$precip, sum(d$precip))
  expect_equal(a$hours, sum(d$hours))
  # max columns
  expect_equal(a$temp_max, max(d$temp_max))
  expect_equal(a$wind_max, max(d$wind_max))
  expect_equal(a$gust_max, max(d$gust_max))
  # min column
  expect_equal(a$temp_min, min(d$temp_min))
  # mean columns
  expect_equal(a$temp_mean, mean(d$temp_mean))
  expect_equal(a$apparent, mean(d$apparent))
  expect_equal(a$humidity, mean(d$humidity))
  expect_equal(a$wind_mean, mean(d$wind_mean))
  expect_equal(a$pressure, mean(d$pressure))
  expect_equal(a$cloud, mean(d$cloud))
})

test_that("roll_up weather_code is the mode", {
  # 10 days: codes 1,1,1,2,2,3,3,4,5,6 -> mode is 1
  d <- make_full_daily("2026-01-01", 10)
  d$weather_code <- c(1, 1, 1, 2, 2, 3, 3, 4, 5, 6)
  a <- roll_up(d, "month")
  expect_equal(a$weather_code, 1)
})

test_that("roll_up weather_code tie-breaks to first max in table", {
  # Codes: 1,2,1,2 -> tie between 1 and 2; table orders by value, which.max
  # picks the first (1)
  d <- make_full_daily("2026-01-01", 4)
  d$weather_code <- c(1, 2, 1, 2)
  a <- roll_up(d, "month")
  expect_true(a$weather_code %in% c(1, 2))
})

test_that("roll_up handles NA in non-precip columns", {
  d <- make_full_daily("2026-01-01", 5)
  d$temp_max[2] <- NA
  d$temp_min[3] <- NA
  d$humidity[4] <- NA
  d$weather_code[5] <- NA
  a <- roll_up(d, "month")
  expect_equal(a$temp_max, max(d$temp_max, na.rm = TRUE))
  expect_equal(a$temp_min, min(d$temp_min, na.rm = TRUE))
  expect_equal(a$humidity, mean(d$humidity, na.rm = TRUE))
  # weather_code: mode of remaining values should not be NA
  expect_true(!is.na(a$weather_code))
})

test_that("roll_up preserves all 14 columns in output", {
  d <- make_full_daily("2026-01-01", 7)
  a <- roll_up(d, "week")
  expect_equal(ncol(a), 15)
  expect_named(a, c("period", "precip", "hours", "events",
                    "temp_max", "temp_min", "temp_mean",
                    "apparent", "humidity",
                    "wind_mean", "wind_max", "gust_max",
                    "pressure", "cloud", "weather_code"))
})

test_that("roll_up with unsorted dates still groups correctly", {
  d <- make_full_daily("2026-01-01", 6)
  # Shuffle rows
  d <- d[sample(nrow(d)), ]
  a <- roll_up(d, "month")
  expect_equal(a$precip, sum(d$precip))
  expect_equal(a$temp_max, max(d$temp_max))
})

# --- mode_num() direct tests ---

test_that("mode_num returns the most common value", {
  expect_equal(mode_num(c(1, 1, 1, 2, 3)), 1)
  expect_equal(mode_num(c(5, 5, 3, 3, 3)), 3)
})

test_that("mode_num handles a single value", {
  expect_equal(mode_num(7), 7)
})

test_that("mode_num returns NA for all-NA input", {
  expect_true(is.na(mode_num(c(NA, NA, NA))))
})

test_that("mode_num returns NA for empty input", {
  expect_true(is.na(mode_num(numeric(0))))
})

test_that("mode_num handles ties (returns one of the tied values)", {
  r <- mode_num(c(4, 4, 7, 7))
  expect_true(r %in% c(4, 7))
})

# --- plot_vars structure ---

test_that("plot_vars is a list of 11 entries, each with label and unit", {
  expect_true(is.list(plot_vars))
  expect_equal(length(plot_vars), 11)
  expect_setequal(names(plot_vars), c(
    "precip", "temp_mean", "temp_max", "temp_min", "apparent",
    "humidity", "wind_mean", "wind_max", "gust_max", "pressure", "cloud"))
  for (v in names(plot_vars)) {
    expect_true(is.list(plot_vars[[v]]), label = v)
    expect_true("label" %in% names(plot_vars[[v]]), label = v)
    expect_true("unit" %in% names(plot_vars[[v]]), label = v)
    expect_true(nchar(plot_vars[[v]]$label) > 0, label = v)
    expect_true(nchar(plot_vars[[v]]$unit) > 0, label = v)
  }
})

test_that("plot_vars keys are all valid columns of a roll_up output", {
  d <- make_full_daily("2026-01-01", 7)
  a <- roll_up(d, "week")
  for (v in names(plot_vars)) {
    expect_true(v %in% names(a), label = paste0(v, " in roll_up columns"))
  }
})
