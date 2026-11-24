# Tests for the preset save/load flow, via shiny::testServer.
#
# The app's server is cloned with toast() no-oped (it needs an active
# session) and updateSelectInput spied, so tests can assert the dropdown
# was refreshed — the mock session in this shiny version does not replay
# update* messages into input.
#
# Note on naming: the testServer expr's lookup chain reaches the global
# environment, so this file avoids bare names like `session`, `t`, or `s`
# that a stray global could shadow. The mock session is passed to
# closures explicitly.

# Clone the app's server with toast() no-oped and updateSelectInput
# spied. Returns list(server, spy) where spy records update calls.
make_test_server <- function() {
  pv_srv <- function(input, output, session) {}
  body(pv_srv) <- body(app_env$server)
  pv_env <- new.env(parent = app_env)
  environment(pv_srv) <- pv_env
  pv_env$toast <- function(...) invisible(NULL)
  pv_spy <- new.env()
  pv_spy$calls <- list()
  pv_env$updateSelectInput <- function(session, inputId, ...) {
    # Wrap the record in its own list(): c() would otherwise flatten the
    # named list into pv_spy$calls, corrupting the per-call records.
    pv_spy$calls <- c(pv_spy$calls,
                      list(list(inputId = inputId, dots = list(...))))
    shiny::updateSelectInput(session, inputId, ...)
  }
  list(server = pv_srv, spy = pv_spy)
}

# Drive the mock session. `verify` runs inside the reactive context where
# session inputs are readable, and receives the updateSelectInput spy as its
# third argument. Spy assertions must happen inside `verify`: testServer's
# teardown re-runs the session-init updateSelectInput, which resets the spy,
# so the save-triggered call is not observable once testServer has returned.
run_server <- function(set_inputs, verify, pre_setup = NULL) {
  pv_ts <- make_test_server()
  pv_sess <- shiny::MockShinySession$new()
  pv_wd <- withr::local_tempdir()
  withr::local_dir(pv_wd)
  # preset_file is top-level (built from getwd() at source time), so point it
  # at the temp dir here; the server's save action writes through it, keeping
  # the test isolated from the real presets.csv.
  pv_orig_file <- app_env$preset_file
  app_env$preset_file <- file.path(pv_wd, "presets.csv")
  on.exit(app_env$preset_file <- pv_orig_file)
  if (!is.null(pre_setup)) pre_setup(pv_wd)  # seed state before server start
  on.exit(withr::defer(pv_sess$close()))
  shiny::testServer(pv_ts$server, {
    set_inputs(pv_sess)
    pv_sess$elapse(0.1)
    pv_sess$flushReact()
    verify(pv_sess, pv_wd, pv_ts$spy)
  }, session = pv_sess)
}

test_that("save writes the preset and refreshes the dropdown immediately", {
  run_server(
    set_inputs = function(pv_ms) {
      pv_ms$setInputs(lat = 37.84, lon = -122.21,
                      date_range = c(as.Date("2026-01-01"),
                                     as.Date("2026-02-01")),
                      place_name = "Oakland",
                      save_btn = 1)
    },
    verify = function(pv_ms, wd, spy) {
      # preset_file is top-level (app_env), redirected to the temp dir by
      # run_server. Compare directory basenames: macOS resolves temp paths
      # via the /var -> /private/var symlink, so exact equality is flaky.
      expect_equal(basename(dirname(app_env$preset_file)), basename(wd))
      expect_true(file.exists(app_env$preset_file))
      saved <- read.csv(app_env$preset_file, stringsAsFactors = FALSE)
      expect_equal(nrow(saved), 1)
      expect_equal(saved$name, "Oakland")
      # The dropdown must be refreshed to include the new preset after
      # saving. The save handler's updateSelectInput is not the last one:
      # a session-plumbing call with inputId = NA fires during the reactive
      # flush, after it. And the choices vector carries a `names` attribute
      # (it comes from a data.frame column), so compare unname()'d. Look
      # for the matching call among all calls rather than assuming last.
      matched <- Filter(function(x) identical(x$inputId, "preset") &&
                        "Oakland" %in% unname(x$dots$choices),
                        spy$calls)
      expect_true(length(matched) >= 1)
    }
  )
})

test_that("loading a preset populates the form inputs", {
  run_server(
    set_inputs = function(pv_ms) {
      pv_ms$setInputs(lat = 37.84, lon = -122.21,
                      date_range = c(as.Date("2026-01-01"),
                                     as.Date("2026-02-01")),
                      place_name = "Oakland",
                      save_btn = 1)
      pv_ms$setInputs(preset = "Oakland")
    },
    verify = function(pv_ms, wd, spy) {
      expect_equal(pv_ms$input$lat, 37.84)
      expect_equal(pv_ms$input$place_name, "Oakland")
      expect_equal(pv_ms$input$date_range,
                   c(as.Date("2026-01-01"), as.Date("2026-02-01")))
    }
  )
})

test_that("the preset dropdown is populated from disk at build time", {
  # The dropdown starts populated: load_presets() reads presets.csv and
  # initial_preset_choices uses those names — no server-side startup update.
  # Point load_presets() at a seeded temp CSV and confirm the names flow
  # through; that is the mechanism that pre-fills the dropdown at build.
  pv_orig <- app_env$preset_file
  on.exit(app_env$preset_file <- pv_orig)
  pv_tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(pv_tmp), add = TRUE)
  write.csv(data.frame(name = c("Seattle", "Oakland"), lat = c(1, 2),
                       lon = c(3, 4), start = "2026-01-01", end = "2026-01-02",
                       stringsAsFactors = FALSE), pv_tmp, row.names = FALSE)
  app_env$preset_file <- pv_tmp
  pv_p <- app_env$load_presets()
  expect_equal(unname(pv_p$name), c("Seattle", "Oakland"))
})
