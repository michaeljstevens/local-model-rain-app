# Tests for fetch_precip(). We mock fromJSON so no network is needed.

mock_fetch <- function(payload) {
  local({
    # Temporarily mask jsonlite::fromJSON with a stub returning `payload`
    stub <- new.env()
    stub$fromJSON <- function(url, ...) payload
    withr::with_envvar(character(), {
      testthat::with_mocked_bindings(
        fromJSON = stub$fromJSON,
        .package = "jsonlite",
        code = fetch_precip(40.71, -74.01, as.Date("2026-01-01"), as.Date("2026-01-03"))
      )
    })
  })
}

test_that("fetch_precip parses daily payload into a data frame", {
  payload <- list(
    daily = list(
      time = c("2026-01-01", "2026-01-02", "2026-01-03"),
      precipitation_sum = c(1.0, 2.5, 0.0),
      precipitation_hours = c(3, 1, 0)
    )
  )
  d <- mock_fetch(payload)
  expect_true(is.data.frame(d))
  expect_equal(nrow(d), 3)
  expect_named(d, c("date", "precip", "hours"))
  expect_true(all(sapply(d$date, is.Date)))
  expect_equal(d$precip, c(1.0, 2.5, 0.0))
  expect_equal(d$hours, c(3, 1, 0))
})

test_that("fetch_precip surfaces API errors as a stop()", {
  payload <- list(error = TRUE, reason = "bad coordinates")
  expect_error(mock_fetch(payload), "API error")
})
