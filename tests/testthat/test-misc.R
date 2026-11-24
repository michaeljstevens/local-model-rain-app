# Tests for the %||% helper and a whole-app build smoke test.

test_that("%||% returns left when non-null", {
  expect_equal(5 %||% 10, 5)
  expect_equal("a" %||% "b", "a")
})

test_that("%||% falls back on NULL and empty string", {
  expect_equal(NULL %||% 10, 10)
  expect_equal("" %||% "b", "b")
  # NA is not NULL and not "", so it passes through unchanged
  expect_true(is.na(NA_character_ %||% "b"))
})

test_that("app.R sources ui and server, and builds a valid shinyApp", {
  expect_true(!is.null(app_env$ui))
  expect_true(is.function(app_env$server))
  app <- shiny::shinyApp(app_env$ui, app_env$server)
  expect_s3_class(app, "shiny.appobj")
})
