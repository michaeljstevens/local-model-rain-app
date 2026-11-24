# Precipitation Tracker

A [Shiny](https://shiny.posit.co/) web app that fetches real historical
precipitation data from the [Open-Meteo archive API](https://open-meteo.com/)
and lets you explore it at different time granularities.

## Features

- **Fetch real data** — enter a latitude/longitude (and optional place name)
  and a date range, then pull daily precipitation sums and wet hours from
  Open-Meteo.
- **Roll up** — aggregate the daily records to daily, weekly, monthly,
  quarterly, or yearly periods.
- **Summary stats** — total precipitation, total wet hours, the wettest
  period, and the number of periods, shown as stat cards.
- **Chart** — a ggplot2 bar + line/point chart of precipitation by period.
- **Records table** — an interactive [DT](https://rstudio.github.io/DT/)
  data table of the aggregated periods.
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
