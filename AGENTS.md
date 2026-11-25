# precip_tracker

A Shiny app that fetches historical weather data from the Open-Meteo archive
API and lets the user explore it at different granularities. All UI + server
code lives in `app.R`.

## Running

```r
shiny::runApp("precip_tracker")   # or shiny::runApp() from this directory
```

## Data source — Open-Meteo archive API

Endpoint: `https://archive-api.open-meteo.com/v1/archive`

All variables are requested in a **single** call via the `daily=` param
(see `fetch_precip()` in `app.R`). The exact variables fetched and their
in-app column names:

| API variable             | App column   |
|--------------------------|--------------|
| `precipitation_sum`      | `precip`     |
| `precipitation_hours`    | `hours`      |
| `temperature_2m_max`     | `temp_max`   |
| `temperature_2m_min`     | `temp_min`   |
| `temperature_2m_mean`    | `temp_mean`  |
| `apparent_temperature_mean` | `apparent` |
| `relative_humidity_2m_mean` | `humidity` |
| `wind_speed_10m_mean`    | `wind_mean`  |
| `wind_speed_10m_max`     | `wind_max`   |
| `wind_gusts_10m_max`     | `gust_max`   |
| `pressure_msl_mean`      | `pressure`   |
| `cloud_cover_mean`       | `cloud`      |
| `weather_code`           | `weather_code` |

A second endpoint serves **forecasts**: `https://api.open-meteo.com/v1/forecast`
(see `fetch_forecast()` and `fetch_forecast_hourly()` in `app.R`). Same
`daily=` variables and return shape as `fetch_precip()`, but takes
`forecast_days=1..16` instead of a date range. The sidebar's "Forecast" card
has a Daily/Hourly resolution picker: daily uses `fetch_forecast()`, hourly
uses `fetch_forecast_hourly()` (`hourly=` variables, same shape as
`fetch_hourly()`). Both feed the same `recs()` pipeline, so roll-up, stats,
plot, and table work unchanged.

**Hourly data**: the archive endpoint also serves `hourly=` variables
(see `fetch_hourly()` in `app.R`). It returns one object for ranges of
<= 92 days and a **list of yearly objects** for longer ranges — both
fetchers normalise that. The hourly frame has a POSIXct `date` column;
`roll_up()` detects that and (a) at the `"hour"` level uses each hour as its
own period, (b) otherwise reduces to daily via `roll_up_hourly_daily()`
first, then groups normally. `apparent` is NA in hourly-derived data (the
API has no hourly apparent temperature). The sidebar's **Granularity**
picker (`input$rollup`) is the single control: it also decides which
fetcher the handlers use — `rollup == "hour"` fetches hourly variables
(`fetch_hourly` / `fetch_forecast_hourly`), anything else fetches daily.
The server's `lvl` reactive is just `input$rollup`.

To add a new variable: add it to the `daily=` string in `fetch_precip()`,
add a column to the returned `data.frame`, add it to `roll_up()`'s
`summarise()` + `select()`, and (if plottable) add an entry to the
`plot_vars` list. The stat cards and plot are driven by `input$plot_var`,
so no UI changes are needed for a new variable.

`plot_vars` (in `app.R`) maps column name -> `list(label=, unit=)`. Each
variable plots on its own y-axis because units differ.

**Rainfall units**: the Roll-up card's "Rainfall units" radio
(`input$precip_units`, `"mm"` default | `"in"`) is display-only. Fetch and
`roll_up()` always work in mm; the server's `agg()` reactive converts
`precip` via `precip_convert()`, and `var_unit()`/`var_label()`/`fmt()`
switch to "in" (2 decimals). Status messages that report a precip total are
stored as a `function(units)` in `status()` so they follow the toggle.

## Roll-up semantics (`roll_up()`)

- sum: `precip`, `hours`
- max: `temp_max`, `wind_max`, `gust_max`
- min: `temp_min`
- mean: `temp_mean`, `apparent`, `humidity`, `wind_mean`, `pressure`, `cloud`
- mode: `weather_code` (via the `mode_num()` helper)

## Testing

This is an app, not a package, so `testthat::test_local()` does not apply.
Run the suite from the `tests/` directory:

```sh
cd tests && Rscript testthat.R
```

- `tests/testthat/helper.R` sources `app.R` into an isolated env and attaches
  `fetch_precip`, `fetch_forecast`, `fetch_forecast_hourly`,
  `fetch_hourly`, `roll_up`, `roll_up_hourly_daily`, and `%||%`. If you add
  a helper the tests should use, attach it there too.
- `test-stats.R` clones `app_env$server` and patches its body so the
  server-local `recs` reactiveVal is initialised from `app_env$pv_injected`.
  If you rename the server's `recs` line, the patch's pattern match in
  `run_pv_stats()` will fail loudly.
- Test files: `test-fetch.R`, `test-forecast.R`, `test-forecast-hourly.R`,
  `test-hourly.R`, `test-roll-up.R`, `test-stats.R`, `test-presets.R`,
  `test-misc.R`.
- `fetch_precip` tests hit the **live** API, so a network connection is
  required.

## Gotchas

- **Do not use `__` as a leading identifier.** `mutate(__y = ...)` is a
  parse-time syntax error in R (`__` is a reserved prefix). Name columns
  without the `__` prefix (e.g. `plot_val`).
- **Don't `library()` a package you don't use.** An unused `library()` for a
  package that isn't installed breaks app startup (this bit us with `tidyr`).
- **Standalone `Rscript` on this machine has a broken Homebrew R 4.3.2
  install** (openblas dylib mismatch) and aborts. Verify code with the live
  R session (`executeCode`) instead of `Rscript` from the shell.
- **Verify before declaring done.** Parse-check and, where possible, run the
  real functions against the live API before reporting a change as complete.
- `presets.csv` is created next to `app.R` on first save and is git-ignored
  (per-user runtime data).
