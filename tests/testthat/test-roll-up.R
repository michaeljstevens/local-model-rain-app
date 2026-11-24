# Tests for roll_up(): aggregating daily data to day/week/month/quarter/year.

make_daily <- function(start, n, vals) {
  data.frame(
    date   = seq(as.Date(start), by = 1, length.out = n),
    precip = vals,
    hours  = rep(1, n)
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
