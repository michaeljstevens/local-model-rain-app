# Source the app in the test environment so we can reach its helper functions.
# This only defines functions and the (unused in tests) ui/server objects.
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(ggplot2)
  library(lubridate); library(dplyr); library(jsonlite); library(DT)
})

app_env <- new.env()
sys.source("../../app.R", envir = app_env, keep.source = TRUE)

# Attach the pieces the tests need
fetch_precip          <- app_env$fetch_precip
fetch_forecast        <- app_env$fetch_forecast
fetch_forecast_hourly <- app_env$fetch_forecast_hourly
fetch_hourly        <- app_env$fetch_hourly
roll_up             <- app_env$roll_up
roll_up_hourly_daily <- app_env$roll_up_hourly_daily
mode_num     <- app_env$mode_num
plot_vars    <- app_env$plot_vars
`%||%`       <- app_env$`%||%`
