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

To add a new variable: add it to the `daily=` string in `fetch_precip()`,
add a column to the returned `data.frame`, add it to `roll_up()`'s
`summarise()` + `select()`, and (if plottable) add an entry to the
`plot_vars` list. The stat cards and plot are driven by `input$plot_var`,
so no UI changes are needed for a new variable.

`plot_vars` (in `app.R`) maps column name -> `list(label=, unit=)`. Each
variable plots on its own y-axis because units differ.

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
  `fetch_precip`, `roll_up`, and `%||%`. If you add a helper the tests should
  use, attach it there too.
- Test files: `test-fetch.R`, `test-roll-up.R`, `test-presets.R`,
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
