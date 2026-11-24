# Precipitation Tracker

A [Shiny](https://shiny.posit.co/) web app that fetches real historical
weather data from the [Open-Meteo archive API](https://open-meteo.com/)
and lets you explore it at different time granularities.

## Features

- **Fetch real data** — enter a latitude/longitude (and optional place name)
  and a date range, then pull a full daily weather summary from Open-Meteo in
  a single API call: precipitation sum and wet hours, temperature
  (max/min/mean), apparent ("feels-like") temperature, relative humidity,
  wind speed (mean and max), gusts, sea-level pressure, cloud cover, and
  weather code.
- **Roll up** — aggregate the daily records to daily, weekly, monthly,
  quarterly, or yearly periods. Each variable is summarized sensibly: sums
  for precipitation and wet hours, max for high/peak values (temp max, wind,
  gusts), min for temp min, means for the rest, and the mode for the weather
  code.
- **Switch the variable** — a "Plot variable" dropdown lets you point the
  chart and the stat cards at any fetched variable. Each plots on its own
  axis (units differ), and the stat cards report the right aggregate
  (total vs. mean) and unit for the selection.
- **Rainfall units** — a toggle switches precipitation between millimetres
  (default) and inches across the stat cards, chart, table, and status line.
- **Summary stats** — a stat card for the selected variable (total/mean),
  plus its max and min periods with dates, and the number of periods.
- **Chart** — a ggplot2 chart of the selected variable by period: bars for
  precipitation/wind at quarterly and yearly granularity, a line otherwise.
- **Records table** — an interactive [DT](https://rstudio.github.io/DT/)
  data table of the aggregated periods (all variables).
- **Saved locations** — save the current location + date range as a named
  preset (persisted to `presets.csv`) and reload it later from the
  "Load preset" dropdown, which is populated from disk at startup.

## Requirements

An R installation and the following packages:

- `shiny`
- `bslib`
- `ggplot2`
- `lubridate`
- `dplyr`
- `jsonlite`
- `DT`

Install any that are missing with:

```r
install.packages(c("shiny", "bslib", "ggplot2", "lubridate", "dplyr", "jsonlite", "DT"))
```

## Running the app

From the directory containing `app.R`:

```r
shiny::runApp()
```

or, from anywhere:

```r
shiny::runApp("/path/to/precip_tracker")
```

You can also use the **Run App** button in RStudio. The app serves on a local
port and opens in your browser.

> Note: `presets.csv` is created next to `app.R` the first time you save a
> preset. It is git-ignored because it is per-user runtime data.

## Project structure

```
.
├┤ app.R          # The Shiny app (UI + server)
├┤ .gitignore
├┤ README.md
└┤ tests/
   └┤ testthat.R          # Test entry point
      └┤ testthat/
         └┤ helper.R          # Sources app.R into a test environment
            └┤ test-fetch.R
            └┤ test-misc.R
            └┤ test-presets.R
            └┤ test-roll-up.R
```

## Testing

This is a Shiny **app**, not an R package, so `testthat::test_local()` does
not apply. Run the suite with its entry point from the `tests/` directory:

```sh
cd tests && Rscript testthat.R
```
