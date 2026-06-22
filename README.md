# NBA RAPM Analysis

A research dashboard combining custom Regularized Adjusted Plus/Minus (RAPM)
estimates with NBA awards data, Basketball-Reference advanced stats, and
external impact metrics from nbarapm.com. The end product is an interactive
Shiny app for exploring player value across multiple measurement systems.

---

## Repository Structure

```
nba-rapm-analysis/
│
├── app/
│   └── app.R                   # Shiny app (single-file)
│
├── scripts/
│   ├── stint_to_rapm.R         # Builds RAPM estimates from stint data
│   ├── scrape_awards.qmd       # Scrapes MVP/All-NBA/All-Defense data
│   ├── scrape_advanced_data.qmd# Scrapes BBref per-team advanced stats
│   └── backup_nbarapm_data.qmd # Downloads nbarapm.com API data to CSV
│
└── data/
    ├── stints/                 # Stint-level play-by-play input data
    │   └── stints_YYYY_YY.csv  # One file per season (2016-17 to 2024-25)
    ├── rapm/                   # RAPM model outputs and reference files
    │   ├── rapm_YYYY_YY.csv    # One file per season
    │   ├── game_dates.csv      # Game date reference used by stint_to_rapm.R
    │   └── player_names.csv    # Player ID → name lookup used by stint_to_rapm.R
    ├── awards/
    │   ├── awards_mvp_voting.csv
    │   ├── awards_all_nba.csv
    │   └── awards_all_defense.csv
    ├── advanced_stats.csv      # BBref per-player advanced stats (2016-2025)
    └── nbarapm_backup/         # Local cache of nbarapm.com API data
        ├── mamba.csv
        ├── raptor.csv
        ├── darko.csv
        └── lebron.csv
```

---

## Prerequisites

### R version
R 4.2 or later is recommended.

### Required R packages

Install everything in one block:

```r
install.packages(c(
  # Shiny app
  "shiny", "bslib", "DT", "tidyverse", "scales", "ggrepel", "jsonlite",
  # Scraping scripts
  "polite", "rvest", "janitor", "glue",
  # RAPM model
  "Matrix", "glmnet"
))
```

### Quarto
The scraping scripts are Quarto (`.qmd`) documents. Install Quarto from
[quarto.org](https://quarto.org/docs/get-started/) if you do not already have
it. You can also run the R code chunks directly inside RStudio without
rendering the full document.

---

## Running the App from Scratch

Follow these steps in order. Steps 1 and 2 only need to be re-run when you
want to refresh the underlying data.

### Step 1 — Produce the RAPM estimates

> Skip this step if the `data/rapm/rapm_YYYY_YY.csv` files are already present
> in the repository.

Open `scripts/stint_to_rapm.R` in RStudio and run:

```r
# Prepare lineup data and fit RAPM models for all seasons
walk(2016:2024, prepare_season_data)
walk(2016:2024, fit_season_rapm)
```

**Input:** `data/stints/stints_YYYY_YY.csv`, `data/rapm/game_dates.csv`, and `data/rapm/player_names.csv`  
**Output:** `data/rapm/rapm_YYYY_YY.csv` (one file per season)

This is the most computationally intensive step. Each season takes a few
minutes depending on hardware; all nine seasons together takes roughly
20–40 minutes.

### Step 2 — Scrape supporting data

Run each of the following Quarto documents by opening them in RStudio and
clicking **Render**, or by running the code chunks interactively.

#### Awards data (MVP voting, All-NBA, All-Defensive teams)

```
scripts/scrape_awards.qmd
```

**Output:** `data/awards/awards_mvp_voting.csv`, `awards_all_nba.csv`,
`awards_all_defense.csv`

Covers seasons 2016–2025. Scrapes Basketball-Reference.com using a polite
crawl (respects `robots.txt`). Takes roughly 2–5 minutes.

#### BBref advanced stats

```
scripts/scrape_advanced_data.qmd
```

**Output:** `data/advanced_stats.csv`

Pulls PER, WS, BPM, VORP, and related stats from every team page on
Basketball-Reference for 2016–2025. Takes roughly 5–15 minutes due to
per-team page requests.

#### nbarapm.com backup (optional but recommended)

```
scripts/backup_nbarapm_data.qmd
```

**Output:** `data/nbarapm_backup/mamba.csv`, `raptor.csv`, `darko.csv`,
`lebron.csv`

Fetches the full history of MAMBA, RAPTOR, DARKO, and LEBRON metrics from
the nbarapm.com JSON API and saves them locally. Takes under a minute.
This step is optional because the app will call the API live at startup;
the local files are used as a fallback if the API becomes unavailable.

### Step 3 — Launch the Shiny app

In the R console, with your working directory set to the repository root:

```r
shiny::runApp("app")
```

Or open `app/app.R` in RStudio and click the **Run App** button.

The app loads all data files at startup and then runs entirely in-browser.
Initial load takes a few seconds while it reads CSVs and calls the
nbarapm.com API (or falls back to the local backup).

---

## App Tabs

| Tab | Description |
|---|---|
| **Home** | Overview of the app and data sources |
| **Awards** | Browse MVP voting and All-NBA / All-Defensive team results by season, with selectable columns |
| **RAPM Search** | Search any player's RAPM across seasons |
| **New Lineups Per Day** | Visualise how many novel lineup combinations appeared each day of a season |
| **RAPM Recognition** | How often top-RAPM players received MVP votes or All-NBA / All-Defensive selections |
| **Metrics Explorer** | Scatter plot and table comparing any two metrics across all data sources |
| **Impact Metrics** | Per-player historical trend lines for MAMBA, RAPTOR, DARKO, and LEBRON |

---

## Data Sources

| Source | Access method |
|---|---|
| Stint-level play-by-play | Provided in `data/stints/` (pre-processed) |
| RAPM estimates | Computed by `scripts/stint_to_rapm.R` |
| MVP / All-NBA / All-Defense | Scraped from [Basketball-Reference.com](https://www.basketball-reference.com) |
| Advanced stats (PER, WS, BPM, VORP, …) | Scraped from [Basketball-Reference.com](https://www.basketball-reference.com) |
| MAMBA, RAPTOR, DARKO, LEBRON | [nbarapm.com](https://www.nbarapm.com) JSON API |
