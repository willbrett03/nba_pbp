library(shiny)
library(bslib)
library(DT)
library(tidyverse)
library(scales)
library(ggrepel)
library(jsonlite)

# ── Data paths ────────────────────────────────────────────────────────────────
# runApp() sets getwd() to the app folder, so ".." is the Basketball Project root
DATA_DIR <- normalizePath(file.path(getwd(), ".."), mustWork = FALSE)
if (!file.exists(file.path(DATA_DIR, "awards_mvp_voting.csv"))) {
  DATA_DIR <- getwd()
}
RAPM_DIR <- file.path(DATA_DIR, "dr_south_code")

# ── Awards data ───────────────────────────────────────────────────────────────
mvp_data <- read_csv(file.path(DATA_DIR, "awards_mvp_voting.csv"),  show_col_types = FALSE)

nba_data <- read_csv(file.path(DATA_DIR, "awards_all_nba.csv"), show_col_types = FALSE) |>
  mutate(nba_team = case_when(
    nba_team %in% c("1st", "1st Team") ~ "1st Team",
    nba_team %in% c("2nd", "2nd Team") ~ "2nd Team",
    nba_team %in% c("3rd", "3rd Team") ~ "3rd Team",
    TRUE ~ nba_team   # "Other Votes" and anything else passes through
  ))

def_data <- read_csv(file.path(DATA_DIR, "awards_all_defense.csv"), show_col_types = FALSE) |>
  mutate(defense_team = case_when(
    defense_team %in% c("1st", "1st Team") ~ "1st Team",
    defense_team %in% c("2nd", "2nd Team") ~ "2nd Team",
    TRUE ~ defense_team
  ))

award_seasons <- sort(unique(mvp_data$season))

# ── RAPM data ─────────────────────────────────────────────────────────────────
rapm_files <- list.files(RAPM_DIR, pattern = "^rapm_\\d{4}_\\d{2}\\.csv$", full.names = TRUE)

rapm_data <- map_dfr(rapm_files, function(f) {
  df <- read_csv(f, show_col_types = FALSE)
  # season col is the start year (2016 = 2016-17); add a display label
  df |> mutate(
    season_label = paste0(season, "-", substr(as.character(season + 1), 3, 4))
  )
}) |>
  arrange(season, RAPM_rank)

rapm_seasons <- rapm_data |>
  distinct(season, season_label) |>
  arrange(season)

# Named vector for season selector: "2016-17" -> 2016
rapm_season_choices <- setNames(rapm_seasons$season, rapm_seasons$season_label)

all_players <- sort(unique(rapm_data$player_name))

# ── Cross-dataset joins (RAPM season = awards season - 1) ─────────────────────
# awards season 2024 = 2023-24 NBA season = RAPM start year 2023

mvp_rapm <- mvp_data |>
  mutate(rapm_season = season - 1) |>
  inner_join(
    rapm_data |> select(rapm_season = season, player_name, RAPM, RAPM_rank),
    by = c("rapm_season", "player" = "player_name")
  ) |>
  mutate(
    season_label = paste0(season - 1, "-", substr(as.character(season), 3, 4))
  )

# Top-N RAPM players joined to All-NBA, All-Defensive, and MVP vote status
rapm_recognition_base <- rapm_data |>
  mutate(awards_season = season + 1) |>
  filter(awards_season %in% unique(mvp_data$season)) |>
  left_join(
    nba_data |> select(awards_season = season, player, nba_team),
    by = c("awards_season", "player_name" = "player")
  ) |>
  left_join(
    def_data |>
      filter(defense_team %in% c("1st Team", "2nd Team")) |>
      select(awards_season = season, player, defense_team),
    by = c("awards_season", "player_name" = "player")
  ) |>
  left_join(
    mvp_data |> select(awards_season = season, player, mvp_share = share, mvp_rank = rank),
    by = c("awards_season", "player_name" = "player")
  ) |>
  mutate(
    season_label    = paste0(season, "-", substr(as.character(season + 1), 3, 4)),
    all_nba_team    = case_when(
      nba_team == "1st Team"  ~ "1st Team",
      nba_team == "2nd Team"  ~ "2nd Team",
      nba_team == "3rd Team"  ~ "3rd Team",
      TRUE                    ~ "Not Selected"
    ),
    all_def_team    = case_when(
      defense_team == "1st Team" ~ "1st Team",
      defense_team == "2nd Team" ~ "2nd Team",
      TRUE                       ~ "Not Selected"
    ),
    got_mvp_votes   = !is.na(mvp_share)
  ) |>
  # One row per player-season: keep best tier earned across any duplicate join rows
  group_by(season, season_label, player_name, RAPM, RAPM_rank, mvp_share, mvp_rank, got_mvp_votes) |>
  summarise(
    all_nba_team = case_when(
      any(all_nba_team == "1st Team") ~ "1st Team",
      any(all_nba_team == "2nd Team") ~ "2nd Team",
      any(all_nba_team == "3rd Team") ~ "3rd Team",
      TRUE                            ~ "Not Selected"
    ),
    all_def_team = case_when(
      any(all_def_team == "1st Team") ~ "1st Team",
      any(all_def_team == "2nd Team") ~ "2nd Team",
      TRUE                            ~ "Not Selected"
    ),
    .groups = "drop"
  ) |>
  mutate(got_all_defense = all_def_team != "Not Selected")

corr_seasons <- sort(unique(mvp_rapm$season))
corr_season_choices <- setNames(
  corr_seasons,
  paste0(corr_seasons - 1, "-", substr(as.character(corr_seasons), 3, 4))
)

# ── Lineup novelty data (precomputed for all seasons) ─────────────────────────
stint_season_tag <- function(y) sprintf("%d_%02d", y, (y + 1) %% 100)

new_lineups_all <- map_dfr(2016:2024, function(yr) {
  path <- file.path(RAPM_DIR, sprintf("stints_%s.csv", stint_season_tag(yr)))
  if (!file.exists(path)) return(NULL)

  stints <- read_csv(path, col_types = cols(
    game_id   = col_character(),
    game_date = col_date(),
    .default  = col_guess()
  ), show_col_types = FALSE) |>
    mutate(season = yr)

  bind_rows(
    stints |> select(game_date, season, lineup = home_lineup),
    stints |> select(game_date, season, lineup = away_lineup)
  ) |>
    drop_na(game_date, lineup) |>
    group_by(lineup) |>
    slice_min(game_date, n = 1, with_ties = FALSE) |>
    ungroup() |>
    count(game_date, season, name = "new_lineups") |>
    arrange(game_date)
})

lineup_seasons <- sort(unique(new_lineups_all$season))
# Season display labels for selector: 2016 -> "2016-17"
lineup_season_choices <- setNames(
  lineup_seasons,
  paste0(lineup_seasons, "-", substr(as.character(lineup_seasons + 1), 3, 4))
)

# ── nbarapm.com external metrics (live API, loaded once at startup) ───────────
# Year convention: nbarapm 'year'/'season' = END year of season
#   e.g. year=2017 => 2016-17 season
# Our RAPM 'season' = START year (2016 => 2016-17)
# So we cover nbarapm years 2017:2025 to match our 2016-2024 RAPM range.
NBARAPM_YEARS  <- 2017:2025
BACKUP_DIR     <- file.path(DATA_DIR, "nbarapm_backup")

# Try the live API first; fall back to the local CSV backup if the API fails.
safe_ext_load <- function(url, backup_file, transform_fn) {
  api_result <- tryCatch({
    fromJSON(url, simplifyDataFrame = TRUE) |>
      as_tibble() |>
      transform_fn()
  }, error = function(e) {
    message("External API load failed (", url, "): ", conditionMessage(e))
    NULL
  })

  if (!is.null(api_result) && nrow(api_result) > 0) return(api_result)

  # API failed — try the local backup
  backup_path <- file.path(BACKUP_DIR, paste0(backup_file, ".csv"))
  if (file.exists(backup_path)) {
    message("Using local backup for ", backup_file, ": ", backup_path)
    tryCatch(
      read_csv(backup_path, show_col_types = FALSE) |> transform_fn(),
      error = function(e) { message("Backup read failed: ", conditionMessage(e)); tibble() }
    )
  } else {
    message("No backup found at: ", backup_path)
    tibble()
  }
}

ext_mamba <- safe_ext_load(
  "https://www.nbarapm.com/load/mamba", "mamba",
  function(d) {
    d |>
      filter(year %in% NBARAPM_YEARS) |>
      mutate(
        player_name  = str_to_title(player_name),
        season_label = paste0(year - 1, "-", substr(as.character(year), 3, 4))
      ) |>
      # jsonlite preserves hyphens; rename "O-MAMBA" -> "O_MAMBA" etc.
      rename_with(~ str_replace_all(.x, "-", "_")) |>
      select(player_name, nba_id, year, season_label,
             MAMBA, O_MAMBA, D_MAMBA,
             MAMBA_rank, O_MAMBA_rank, D_MAMBA_rank)
  }
)

ext_raptor <- safe_ext_load(
  "https://www.nbarapm.com/load/raptor", "raptor",
  function(d) {
    d |>
      filter(season %in% NBARAPM_YEARS) |>
      mutate(
        player_name  = str_to_title(player_name),
        nba_id       = suppressWarnings(as.integer(nba_id)),
        season_label = paste0(season - 1, "-", substr(as.character(season), 3, 4))
      ) |>
      select(player_name, nba_id, year = season, season_label,
             RAPTOR = raptor_total, O_RAPTOR = raptor_offense, D_RAPTOR = raptor_defense,
             RAPTOR_rank = raptor_rank, O_RAPTOR_rank = o_raptor_rank, D_RAPTOR_rank = d_raptor_rank)
  }
)

ext_darko <- safe_ext_load(
  "https://www.nbarapm.com/load/DARKO", "darko",
  function(d) {
    d |>
      filter(season %in% NBARAPM_YEARS) |>
      mutate(
        player_name  = str_to_title(player_name),
        season_label = paste0(season - 1, "-", substr(as.character(season), 3, 4))
      ) |>
      select(player_name, nba_id, year = season, season_label,
             DARKO = dpm, O_DARKO = o_dpm, D_DARKO = d_dpm,
             DARKO_rank = dpm_rank, O_DARKO_rank = o_dpm_rank, D_DARKO_rank = d_dpm_rank)
  }
)

ext_lebron <- safe_ext_load(
  "https://www.nbarapm.com/load/lebron", "lebron",
  function(d) {
    d |>
      filter(year %in% NBARAPM_YEARS) |>
      mutate(
        player_name  = str_to_title(player_name),
        season_label = paste0(year - 1, "-", substr(as.character(year), 3, 4))
      ) |>
      # jsonlite preserves hyphens; rename "O-LEBRON" -> "O_LEBRON" etc.
      rename_with(~ str_replace_all(.x, "-", "_")) |>
      select(player_name, nba_id, year, season_label,
             LEBRON, O_LEBRON, D_LEBRON,
             LEBRON_rank = LEBRON_Rank, O_LEBRON_rank = O_LEBRON_Rank, D_LEBRON_rank = D_LEBRON_Rank)
  }
)

# Metric colour palette (consistent across plot and legend)
EXT_COLOURS <- c(
  MAMBA  = "#E41A1C",
  RAPTOR = "#377EB8",
  DARKO  = "#4DAF4A",
  LEBRON = "#FF7F00"
)

# ── Combined metrics dataset ──────────────────────────────────────────────────
# Joins: our RAPM + nbarapm API (keyed by nba_id) + BBref advanced (keyed by name)
# Season alignment (all use "start year" = rapm_season):
#   our RAPM season   = start year  (2016 = 2016-17)
#   nbarapm year      = end year    → rapm_season = nbarapm_year - 1
#   BBref season col  = end year    → rapm_season = bbref_season  - 1

# Name normaliser: strips accents + title-cases for BBref joins
strip_accents <- function(x) iconv(x, to = "ASCII//TRANSLIT")
norm_name     <- function(x) str_to_title(strip_accents(x)) |> str_squish()

# BBref advanced stats — loads if scraped, otherwise returns empty skeleton
adv_path <- file.path(DATA_DIR, "advanced_stats.csv")
bbref_adv <- if (file.exists(adv_path)) {
  message("Loading BBref advanced stats from ", adv_path)
  read_csv(adv_path, show_col_types = FALSE) |>
    mutate(
      rapm_season = as.integer(season) - 1L,
      name_key    = norm_name(Player),
      MP          = suppressWarnings(as.numeric(MP))
    ) |>
    # Traded players appear on multiple team pages; keep the stint with most MP
    group_by(name_key, rapm_season) |>
    slice_max(MP, n = 1, with_ties = FALSE) |>
    ungroup() |>
    mutate(across(c(PER, OWS, DWS, WS, OBPM, DBPM, BPM, VORP),
                  ~ suppressWarnings(as.numeric(.x)))) |>
    select(name_key, rapm_season, PER, OWS, DWS, WS, OBPM, DBPM, BPM, VORP)
} else {
  message("advanced_stats.csv not found — BBref columns will be empty")
  tibble(name_key = character(), rapm_season = integer(),
         PER = numeric(), OWS = numeric(), DWS = numeric(), WS = numeric(),
         OBPM = numeric(), DBPM = numeric(), BPM = numeric(), VORP = numeric())
}
bbref_available <- nrow(bbref_adv) > 0

# Helper: add rapm_season to an ext_* tibble and keep only join + value cols
mk_nbarapm_join <- function(df, ...) {
  if (nrow(df) == 0) {
    return(tibble(nba_id = integer(), rapm_season = integer()))
  }
  df |>
    mutate(rapm_season = as.integer(year) - 1L) |>
    select(nba_id, rapm_season, ...)
}

mamba_j  <- mk_nbarapm_join(ext_mamba,  MAMBA, O_MAMBA, D_MAMBA)
raptor_j <- mk_nbarapm_join(ext_raptor, RAPTOR, O_RAPTOR, D_RAPTOR)
darko_j  <- mk_nbarapm_join(ext_darko,  DARKO, O_DARKO, D_DARKO)
lebron_j <- mk_nbarapm_join(ext_lebron, LEBRON, O_LEBRON, D_LEBRON)

combined_metrics <- rapm_data |>
  select(
    player_name,
    nba_id      = player_id,
    rapm_season = season,
    season_label,
    RAPM,
    RAPM_rank
  ) |>
  left_join(mamba_j,  by = c("nba_id", "rapm_season")) |>
  left_join(raptor_j, by = c("nba_id", "rapm_season")) |>
  left_join(darko_j,  by = c("nba_id", "rapm_season")) |>
  left_join(lebron_j, by = c("nba_id", "rapm_season")) |>
  mutate(name_key = norm_name(player_name)) |>
  left_join(bbref_adv, by = c("name_key", "rapm_season")) |>
  select(-name_key) |>
  arrange(rapm_season, RAPM_rank)

# Named metric choices for scatter axis selectors (grouped by source)
COMP_METRIC_CHOICES <- c(
  list("Our RAPM" = c("RAPM (ours)" = "RAPM")),
  if (bbref_available) list("BBref Advanced" = c(
    "PER" = "PER", "WS" = "WS", "BPM" = "BPM", "VORP" = "VORP",
    "OBPM" = "OBPM", "DBPM" = "DBPM", "OWS" = "OWS", "DWS" = "DWS"
  )) else NULL,
  list(
    "MAMBA (nbarapm)"     = c("MAMBA"    = "MAMBA",  "O-MAMBA"  = "O_MAMBA",  "D-MAMBA"  = "D_MAMBA"),
    "RAPTOR (538)"        = c("RAPTOR"   = "RAPTOR", "O-RAPTOR" = "O_RAPTOR", "D-RAPTOR" = "D_RAPTOR"),
    "DARKO"               = c("DARKO"    = "DARKO",  "O-DARKO"  = "O_DARKO",  "D-DARKO"  = "D_DARKO"),
    "LEBRON (BBall-Index)"= c("LEBRON"   = "LEBRON", "O-LEBRON" = "O_LEBRON", "D-LEBRON" = "D_LEBRON")
  )
)

# Flat lookup: column name -> short display label (no "Group.Label" prefix)
METRIC_LABELS <- do.call(c, lapply(COMP_METRIC_CHOICES, function(grp) {
  setNames(names(grp), grp)
}))

cmp_seasons <- sort(unique(combined_metrics$rapm_season))
cmp_season_choices <- setNames(
  cmp_seasons,
  paste0(cmp_seasons, "-", substr(as.character(cmp_seasons + 1), 3, 4))
)

# ── Column helpers ────────────────────────────────────────────────────────────
apply_cols <- function(df, col_map) {
  present   <- col_map[col_map %in% names(df)]
  df        <- df |> select(all_of(present))
  names(df) <- names(present)
  df
}

MVP_COLS <- c(
  "Rank" = "rank", "Player" = "player", "Team" = "tm", "Age" = "age",
  "1st-Place Votes" = "first", "Pts Won" = "pts_won", "Pts Max" = "pts_max",
  "Vote Share" = "share", "G" = "g", "PTS" = "pts", "TRB" = "trb",
  "AST" = "ast", "STL" = "stl", "BLK" = "blk", "WS" = "ws", "WS/48" = "ws_48"
)
NBA_COLS <- c(
  "Team" = "nba_team", "Pos" = "pos", "Player" = "player", "Tm" = "tm",
  "Age" = "age", "Pts Won" = "pts_won", "Pts Max" = "pts_max",
  "Vote Share" = "share", "1st Tm Votes" = "x1st_tm",
  "2nd Tm Votes" = "x2nd_tm", "3rd Tm Votes" = "x3rd_tm",
  "PTS" = "pts", "TRB" = "trb", "AST" = "ast", "WS" = "ws"
)
DEF_COLS <- c(
  "Team" = "defense_team", "Pos" = "pos", "Player" = "player", "Tm" = "tm",
  "Age" = "age", "Pts Won" = "pts_won", "Pts Max" = "pts_max",
  "Vote Share" = "share", "PTS" = "pts", "TRB" = "trb", "AST" = "ast",
  "STL" = "stl", "BLK" = "blk", "DWS" = "dws", "DBPM" = "dbpm",
  "DRtg" = "d_rtg"
)

# Available columns per award type (display names, matching keys of *_COLS above)
AWARD_COL_CHOICES <- list(
  mvp         = names(MVP_COLS),
  all_nba     = names(NBA_COLS),
  all_defense = names(DEF_COLS)
)
# Sensible defaults — the most commonly useful subset
AWARD_COL_DEFAULTS <- list(
  mvp         = c("Rank", "Player", "Team", "Vote Share", "PTS", "TRB", "AST", "WS"),
  all_nba     = c("Team", "Pos", "Player", "Vote Share", "PTS", "TRB", "AST", "WS"),
  all_defense = c("Team", "Pos", "Player", "Vote Share", "STL", "BLK", "DWS", "DBPM", "DRtg")
)

# ── Theme ─────────────────────────────────────────────────────────────────────
app_theme <- bs_theme(bootswatch = "flatly", base_font = font_google("Inter"))

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = "NBA Analytics Explorer",
  theme = app_theme,

  # ── Tab 0: Home / Landing Page ───────────────────────────────────────────────
  nav_panel(
    title = "Home",
    icon  = icon("house"),

    div(
      style = "max-width: 1100px; margin: 0 auto; padding: 2rem 1.5rem;",

      div(
        class = "text-center mb-5",
        h1("NBA Analytics Explorer", class = "display-5 fw-bold mb-2"),
        p(
          class = "lead text-muted",
          "A research dashboard combining custom RAPM, NBA awards data, ",
          "BBref advanced stats, and external impact metrics into one place."
        ),
        hr(class = "my-4")
      ),

      layout_columns(
        col_widths = c(12),
        card(
          class = "mb-4",
          card_body(
            p(
              "This app is built around a custom ",
              tags$strong("Regularized Adjusted Plus-Minus (RAPM)"),
              " model calculated from stint-level NBA play-by-play data spanning the ",
              tags$strong("2016-17 through 2024-25 seasons."),
              " It layers that model on top of award voting records, BBref advanced stats,",
              " and four external impact metrics (MAMBA, RAPTOR, DARKO, LEBRON) sourced",
              " live from ",
              tags$a("nbarapm.com", href = "https://www.nbarapm.com", target = "_blank"),
              "."
            )
          )
        )
      ),

      h4("What's inside", class = "mb-3 fw-semibold"),
      layout_columns(
        col_widths = c(4, 4, 4),
        fill = FALSE,

        card(
          height = "100%",
          card_header(
            class = "d-flex align-items-center gap-2",
            icon("trophy", class = "text-warning"), tags$span("Awards")
          ),
          card_body(
            p("Browse MVP voting, All-NBA, and All-Defensive results by season.
               Filter by team tier and download the data.")
          )
        ),

        card(
          height = "100%",
          card_header(
            class = "d-flex align-items-center gap-2",
            icon("magnifying-glass", class = "text-primary"), tags$span("RAPM Search")
          ),
          card_body(
            p("Look up any player's custom RAPM score and rank across one season or all seasons.")
          )
        ),

        card(
          height = "100%",
          card_header(
            class = "d-flex align-items-center gap-2",
            icon("circle-dot", class = "text-success"), tags$span("MVP vs RAPM")
          ),
          card_body(
            p("Scatter plot of MVP vote share against RAPM rank. Shows how well voters
               track true on-court impact each season.")
          )
        )
      ),

      layout_columns(
        col_widths = c(4, 4, 4),
        fill = FALSE,
        class = "mt-3",

        card(
          height = "100%",
          card_header(
            class = "d-flex align-items-center gap-2",
            icon("medal", class = "text-danger"), tags$span("RAPM Recognition")
          ),
          card_body(
            p("See what share of top-N RAPM players earned All-NBA, All-Defensive,
               or MVP recognition in a given season.")
          )
        ),

        card(
          height = "100%",
          card_header(
            class = "d-flex align-items-center gap-2",
            icon("table-cells", class = "text-info"), tags$span("Metrics Explorer")
          ),
          card_body(
            p("Compare our RAPM, BBref advanced stats, and external metrics side by side.
               Scatter any two metrics against each other and search the full table.")
          )
        ),

        card(
          height = "100%",
          card_header(
            class = "d-flex align-items-center gap-2",
            icon("chart-line", class = "text-secondary"), tags$span("Impact Metrics")
          ),
          card_body(
            p("Search a player to see their season-by-season history across MAMBA,
               RAPTOR, DARKO, and LEBRON, with offensive and defensive breakdowns.")
          )
        )
      ),

      layout_columns(
        col_widths = c(4, 8),
        fill = FALSE,
        class = "mt-3",

        card(
          height = "100%",
          card_header(
            class = "d-flex align-items-center gap-2",
            icon("chart-line"), tags$span("New Lineups Per Day")
          ),
          card_body(
            p("Tracks how many unique 5-man lineup combinations debuted each day
               of a selected season.")
          )
        ),

        card(
          height = "100%",
          card_header(
            class = "d-flex align-items-center gap-2",
            icon("database"), tags$span("Data Sources")
          ),
          card_body(
            tags$ul(
              class = "mb-0",
              tags$li(tags$strong("Custom RAPM:"),
                " Ridge regression on stint-level play-by-play data, 2016-17 to 2024-25."),
              tags$li(tags$strong("Basketball-Reference:"),
                " Award voting and advanced stats (PER, WS, BPM, VORP) scraped via polite."),
              tags$li(tags$strong("nbarapm.com:"),
                " MAMBA, RAPTOR, DARKO, and LEBRON loaded live via their public API.")
            )
          )
        )
      )
    )
  ),

  # ── Tab 1: Awards ───────────────────────────────────────────────────────────
  nav_panel(
    title = "Awards",
    icon  = icon("trophy"),

    layout_sidebar(
      sidebar = sidebar(
        width = 230,

        selectInput("season", "Season",
          choices  = award_seasons,
          selected = max(award_seasons)
        ),
        selectInput("award", "Award",
          choices = c(
            "MVP Voting"          = "mvp",
            "All-NBA Teams"       = "all_nba",
            "All-Defensive Teams" = "all_defense"
          )
        ),
        conditionalPanel(
          condition = "input.award == 'all_nba'",
          checkboxGroupInput("nba_tier", "Show Teams",
            choices  = c("1st Team", "2nd Team", "3rd Team", "Other Votes"),
            selected = c("1st Team", "2nd Team", "3rd Team")
          )
        ),
        conditionalPanel(
          condition = "input.award == 'all_defense'",
          checkboxGroupInput("def_tier", "Show Teams",
            choices  = c("1st Team", "2nd Team", "Other Votes"),
            selected = c("1st Team", "2nd Team")
          )
        ),
        hr(),
        tags$label(class = "form-label small fw-semibold", "Columns to show"),
        div(
          style = "max-height: 210px; overflow-y: auto; border: 1px solid #dee2e6;
                   border-radius: 4px; padding: 4px 8px; background: #fff;",
          checkboxGroupInput("award_cols", label = NULL,
            choices  = AWARD_COL_CHOICES$mvp,
            selected = AWARD_COL_DEFAULTS$mvp
          )
        ),
        hr(),
        downloadButton("dl_awards", "Download CSV",
          class = "btn-sm btn-outline-primary w-100")
      ),

      layout_columns(
        fill = FALSE,
        value_box("Season",         textOutput("card_season"), showcase = icon("calendar"), theme = "primary"),
        value_box("Award",          textOutput("card_award"),  showcase = icon("trophy"),   theme = "success"),
        value_box("Players Shown",  textOutput("card_n"),      showcase = icon("list"),     theme = "info")
      ),
      card(
        full_screen = TRUE,
        card_header("Results"),
        DTOutput("awards_tbl")
      )
    )
  ),

  # ── Tab 2: RAPM Search ──────────────────────────────────────────────────────
  nav_panel(
    title = "RAPM Search",
    icon  = icon("magnifying-glass"),

    layout_sidebar(
      sidebar = sidebar(
        width = 260,

        textInput("rapm_search", "Player name",
          placeholder = "e.g. LeBron, Curry…"
        ),

        selectInput("rapm_season", "Season",
          choices  = c("All seasons" = "all", rapm_season_choices),
          selected = "all"
        ),

        hr(),
        p(class = "text-muted small",
          "Showing players ranked by RAPM within the selected season(s). ",
          "Search is case-insensitive partial match."
        ),
        hr(),
        downloadButton("dl_rapm", "Download CSV",
          class = "btn-sm btn-outline-primary w-100")
      ),

      layout_columns(
        fill = FALSE,
        value_box("Players Found",  textOutput("rapm_card_n"),    showcase = icon("user"),        theme = "primary"),
        value_box("Top RAPM",       textOutput("rapm_card_top"),  showcase = icon("arrow-up"),    theme = "success"),
        value_box("Seasons Shown",  textOutput("rapm_card_seas"), showcase = icon("calendar"),    theme = "info")
      ),
      card(
        full_screen = TRUE,
        card_header("RAPM Results"),
        DTOutput("rapm_tbl")
      )
    )
  ),

  # ── Tab 3: MVP Vote Share vs RAPM ───────────────────────────────────────────
  nav_panel(
    title = "MVP vs RAPM",
    icon  = icon("circle-dot"),

    layout_sidebar(
      sidebar = sidebar(
        width = 240,

        selectInput("corr_season", "Season",
          choices  = c("All seasons" = "all", corr_season_choices),
          selected = "all"
        ),

        hr(),
        sliderInput("corr_topn", "Show top N vote-getters",
          min = 3, max = 20, value = 10, step = 1
        ),

        hr(),
        p(class = "text-muted small",
          "Each point is a player-season. Only seasons 2016-17 onward ",
          "are shown (RAPM coverage). Players are labelled when they ",
          "received at least 10% of MVP votes."
        )
      ),

      card(
        full_screen = TRUE,
        card_header("MVP Vote Share vs RAPM Rank"),
        plotOutput("corr_plot", height = "480px")
      )
    )
  ),

  # ── Tab 4: RAPM Recognition ─────────────────────────────────────────────────
  nav_panel(
    title = "RAPM Recognition",
    icon  = icon("medal"),

    layout_sidebar(
      sidebar = sidebar(
        width = 240,

        selectInput("recog_season", "Season",
          choices  = c("All seasons" = "all", corr_season_choices),
          selected = "all"
        ),

        sliderInput("recog_topn", "Top-N RAPM threshold",
          min = 5, max = 30, value = 15, step = 5
        ),

        hr(),
        p(class = "text-muted small",
          "For each season, shows whether top-N RAPM players were ",
          "recognised with All-NBA selection or MVP votes."
        ),
        hr(),
        downloadButton("dl_recog", "Download CSV",
          class = "btn-sm btn-outline-primary w-100")
      ),

      layout_columns(
        fill = FALSE,
        value_box("All-NBA Rate",     textOutput("recog_allnba_rate"), showcase = icon("star"),    theme = "success"),
        value_box("All-Defense Rate", textOutput("recog_alldef_rate"), showcase = icon("shield"),  theme = "info"),
        value_box("MVP Votes Rate",   textOutput("recog_mvp_rate"),    showcase = icon("trophy"),  theme = "primary"),
        value_box("Unrecognized",     textOutput("recog_neither_rate"),showcase = icon("xmark"),   theme = "secondary")
      ),
      card(
        full_screen = TRUE,
        card_header("Top-N RAPM Players — Award Recognition"),
        DTOutput("recog_tbl")
      )
    )
  ),

  # ── Tab 5: Metrics Explorer ──────────────────────────────────────────────────
  nav_panel(
    title = "Metrics Explorer",
    icon  = icon("table-cells"),

    layout_sidebar(
      sidebar = sidebar(
        width = 280,

        selectInput("cmp_season", "Season",
          choices  = c("All seasons" = "all", cmp_season_choices),
          selected = "all"
        ),

        sliderInput("cmp_topn", "Filter to top-N by RAPM rank",
          min = 10, max = nrow(combined_metrics), value = 50, step = 10
        ),

        textInput("cmp_player", "Search player (optional)",
          placeholder = "e.g. Curry, Jokic…"
        ),

        hr(),
        tags$p(class = "fw-semibold mb-1 small", "Scatter plot axes"),
        selectInput("cmp_x", "X axis",
          choices  = COMP_METRIC_CHOICES,
          selected = "RAPM"
        ),
        selectInput("cmp_y", "Y axis",
          choices  = COMP_METRIC_CHOICES,
          selected = "DARKO"
        ),

        hr(),
        tags$p(class = "fw-semibold mb-1 small", "Show / hide panels"),
        checkboxInput("show_cmp_tbl",  "Data table",     value = TRUE),
        checkboxInput("show_cmp_plot", "Scatter plot",   value = TRUE),
        hr(),
        p(class = "text-muted small",
          "Our RAPM and BBref stats use start-year season convention. ",
          "External metrics from nbarapm.com joined by NBA player ID.",
          if (!bbref_available)
            tags$span(class = "text-warning",
              " BBref advanced stats not yet loaded — run scrape_advanced_data.qmd first.")
        ),
        hr(),
        downloadButton("dl_cmp", "Download CSV",
          class = "btn-sm btn-outline-primary w-100")
      ),

      conditionalPanel(
        condition = "input.show_cmp_tbl",
        card(
          full_screen = TRUE,
          card_header("All Metrics Table"),
          DTOutput("cmp_tbl")
        )
      ),

      conditionalPanel(
        condition = "input.show_cmp_plot",
        card(
          full_screen = TRUE,
          card_header("Metric Scatter Plot"),
          plotOutput("cmp_plot", height = "460px")
        )
      )
    )
  ),

  # ── Tab 6: Impact Metrics History (nbarapm.com) ─────────────────────────────
  nav_panel(
    title = "Impact Metrics",
    icon  = icon("chart-line"),

    layout_sidebar(
      sidebar = sidebar(
        width = 270,

        textInput("ext_player", "Search player",
          placeholder = "e.g. LeBron James, Jokic…"
        ),

        radioButtons("ext_view", "Component",
          choices  = c("Total", "Offense", "Defense"),
          selected = "Total",
          inline   = TRUE
        ),

        checkboxGroupInput("ext_metrics", "Metrics to show",
          choices  = c("MAMBA", "RAPTOR", "DARKO", "LEBRON"),
          selected = c("MAMBA", "RAPTOR", "DARKO", "LEBRON")
        ),

        hr(),
        p(class = "text-muted small",
          tags$b("Units:"), " points per 100 possessions above average.",
          tags$br(),
          tags$b("Sources:"), " MAMBA (Teemohoops), RAPTOR (538 — stopped after 2021-22), ",
          "DARKO (darko.app), LEBRON (BBall-Index).",
          tags$br(),
          "Data via ", tags$a("nbarapm.com", href = "https://www.nbarapm.com", target = "_blank"), "."
        )
      ),

      card(
        full_screen = TRUE,
        card_header("Metric History Chart"),
        plotOutput("ext_plot", height = "420px")
      ),

      card(
        full_screen = TRUE,
        card_header("Metric History Table"),
        DTOutput("ext_tbl")
      )
    )
  ),

  # ── Tab 7: New Lineups Per Day ──────────────────────────────────────────────
  nav_panel(
    title = "New Lineups Per Day",
    icon  = icon("chart-line"),

    layout_sidebar(
      sidebar = sidebar(
        width = 230,

        selectInput(
          "lineup_season", "Season",
          choices  = lineup_season_choices,
          selected = max(lineup_seasons)
        ),

        hr(),
        p(class = "text-muted small",
          "Shows how many unique 5-man lineup combinations appeared ",
          "for the first time each day during the selected season. ",
          "Home and away lineups are pooled."
        )
      ),

      card(
        full_screen = TRUE,
        card_header("New 5-Man Lineup Combinations Per Day"),
        plotOutput("lineup_plot", height = "420px")
      ),

      card(
        card_header("Write-Up"),
        p("Need to write up later.")
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Awards tab ──────────────────────────────────────────────────────────────
  # Refresh column choices whenever the award type changes
  observeEvent(input$award, {
    updateCheckboxGroupInput(session, "award_cols",
      choices  = AWARD_COL_CHOICES[[input$award]],
      selected = AWARD_COL_DEFAULTS[[input$award]]
    )
  })

  awards_filtered <- reactive({
    yr <- as.integer(input$season)
    df <- if (input$award == "mvp") {
      mvp_data |> filter(season == yr) |> arrange(rank) |> apply_cols(MVP_COLS)
    } else if (input$award == "all_nba") {
      nba_data |>
        filter(season == yr, nba_team %in% input$nba_tier) |>
        arrange(match(nba_team, c("1st Team","2nd Team","3rd Team","Other Votes")), pos) |>
        apply_cols(NBA_COLS)
    } else {
      def_data |>
        filter(season == yr, defense_team %in% input$def_tier) |>
        arrange(match(defense_team, c("1st Team","2nd Team","Other Votes")), pos) |>
        apply_cols(DEF_COLS)
    }
    # Apply user-selected column filter; always keep Player if deselected
    chosen <- union("Player", intersect(input$award_cols, names(df)))
    if (length(chosen) == 0) df else df[, chosen, drop = FALSE]
  })

  awards_raw <- reactive({
    yr <- as.integer(input$season)
    if (input$award == "mvp") {
      mvp_data |> filter(season == yr)
    } else if (input$award == "all_nba") {
      nba_data |> filter(season == yr, nba_team %in% input$nba_tier)
    } else {
      def_data |> filter(season == yr, defense_team %in% input$def_tier)
    }
  })

  award_label <- reactive({
    switch(input$award,
      mvp         = "MVP Voting",
      all_nba     = "All-NBA Teams",
      all_defense = "All-Defensive Teams"
    )
  })

  output$card_season <- renderText(input$season)
  output$card_award  <- renderText(award_label())
  output$card_n      <- renderText(nrow(awards_filtered()))

  output$awards_tbl <- renderDT({
    df <- awards_filtered()
    datatable(df, rownames = FALSE, selection = "none",
      options = list(pageLength = 25, dom = "ftp", scrollX = TRUE)
    ) |>
      formatPercentage(intersect("Vote Share", names(df)), digits = 1) |>
      formatRound(intersect(c("WS","WS/48","DBPM","DWS"), names(df)), digits = 2) |>
      formatRound(intersect(c("PTS","TRB","AST","STL","BLK"), names(df)), digits = 1)
  })

  output$dl_awards <- downloadHandler(
    filename = function() paste0("awards_", input$award, "_", input$season, ".csv"),
    content  = function(file) write_csv(awards_raw(), file)
  )

  # ── RAPM tab ────────────────────────────────────────────────────────────────
  rapm_filtered <- reactive({
    df <- rapm_data

    # Season filter
    if (input$rapm_season != "all") {
      df <- df |> filter(season == as.integer(input$rapm_season))
    }

    # Player name search (case-insensitive partial match)
    query <- trimws(input$rapm_search)
    if (nchar(query) > 0) {
      df <- df |> filter(str_detect(player_name, regex(query, ignore_case = TRUE)))
    }

    df |>
      select(Season = season_label, Player = player_name, RAPM, Rank = RAPM_rank) |>
      arrange(Season, Rank)
  })

  output$rapm_card_n    <- renderText(nrow(rapm_filtered()))
  output$rapm_card_top  <- renderText({
    df <- rapm_filtered()
    if (nrow(df) == 0) return("—")
    best <- df |> slice_max(RAPM, n = 1, with_ties = FALSE)
    paste0(round(best$RAPM, 2), " (", best$Player, ")")
  })
  output$rapm_card_seas <- renderText({
    n <- length(unique(rapm_filtered()$Season))
    if (n == 0) "0" else if (input$rapm_season == "all") paste0("All (", n, ")") else n
  })

  output$rapm_tbl <- renderDT({
    df <- rapm_filtered()
    datatable(df, rownames = FALSE, selection = "none",
      options = list(pageLength = 25, dom = "ftp", scrollX = TRUE,
        order = list(list(2, "desc"))   # default sort: RAPM descending
      )
    ) |>
      formatRound("RAPM", digits = 3) |>
      formatStyle("RAPM",
        background         = styleColorBar(range(rapm_data$RAPM, na.rm = TRUE), "#aed6f1"),
        backgroundSize     = "88% 55%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      )
  })

  output$dl_rapm <- downloadHandler(
    filename = function() {
      season_str <- if (input$rapm_season == "all") "all" else input$rapm_season
      paste0("rapm_", season_str, ".csv")
    },
    content = function(file) write_csv(rapm_filtered(), file)
  )

  # ── MVP vs RAPM tab ──────────────────────────────────────────────────────────
  output$corr_plot <- renderPlot({
    df <- mvp_rapm
    if (input$corr_season != "all") {
      df <- df |> filter(season == as.integer(input$corr_season))
    }
    # Keep only top-N vote-getters per season
    df <- df |>
      group_by(season) |>
      slice_min(rank, n = input$corr_topn, with_ties = FALSE) |>
      ungroup()

    label_df <- df |> filter(share >= 0.10)

    ggplot(df, aes(x = RAPM_rank, y = share, colour = season_label)) +
      geom_point(size = 3, alpha = 0.8) +
      geom_text_repel(
        data  = label_df,
        aes(label = player),
        size  = 3.2,
        max.overlaps = 15,
        show.legend  = FALSE
      ) +
      scale_y_continuous(labels = label_percent(), name = "MVP Vote Share") +
      scale_x_continuous(name = "RAPM Rank (lower = better)") +
      scale_colour_viridis_d(name = "Season") +
      labs(
        title    = "MVP Vote Share vs RAPM Rank",
        subtitle = "Each point is a player-season; labelled players received ≥10% of votes"
      ) +
      theme_bw(base_size = 13) +
      theme(plot.title = element_text(face = "bold"))
  })

  # ── RAPM Recognition tab ─────────────────────────────────────────────────────
  recog_filtered <- reactive({
    df <- rapm_recognition_base |> filter(RAPM_rank <= input$recog_topn)
    if (input$recog_season != "all") {
      df <- df |> filter(season == as.integer(input$recog_season) - 1)
    }
    df |>
      arrange(season, RAPM_rank) |>
      select(
        Season       = season_label,
        Rank         = RAPM_rank,
        Player       = player_name,
        RAPM,
        `All-NBA`    = all_nba_team,
        `All-Defense` = all_def_team,
        `MVP Share`  = mvp_share,
        `MVP Rank`   = mvp_rank
      )
  })

  recog_rate <- reactive({
    df <- rapm_recognition_base |> filter(RAPM_rank <= input$recog_topn)
    if (input$recog_season != "all") {
      df <- df |> filter(season == as.integer(input$recog_season) - 1)
    }
    list(
      allnba   = mean(df$all_nba_team != "Not Selected", na.rm = TRUE),
      alldef   = mean(df$got_all_defense, na.rm = TRUE),
      mvp      = mean(df$got_mvp_votes, na.rm = TRUE),
      neither  = mean(
        df$all_nba_team == "Not Selected" & !df$got_all_defense & !df$got_mvp_votes,
        na.rm = TRUE
      )
    )
  })

  output$recog_allnba_rate  <- renderText(paste0(round(recog_rate()$allnba  * 100, 1), "%"))
  output$recog_alldef_rate  <- renderText(paste0(round(recog_rate()$alldef  * 100, 1), "%"))
  output$recog_mvp_rate     <- renderText(paste0(round(recog_rate()$mvp     * 100, 1), "%"))
  output$recog_neither_rate <- renderText(paste0(round(recog_rate()$neither * 100, 1), "%"))

  output$recog_tbl <- renderDT({
    df <- recog_filtered()
    datatable(df, rownames = FALSE, selection = "none",
      options = list(pageLength = 20, dom = "ftp", scrollX = TRUE)
    ) |>
      formatRound("RAPM", digits = 3) |>
      formatPercentage("MVP Share", digits = 1) |>
      formatStyle("All-NBA",
        backgroundColor = styleEqual(
          c("1st Team", "2nd Team", "3rd Team", "Not Selected"),
          c("#d4efdf",  "#d6eaf8",  "#fef9e7",  "#f9f9f9")
        )
      ) |>
      formatStyle("All-Defense",
        backgroundColor = styleEqual(
          c("1st Team", "2nd Team", "Not Selected"),
          c("#d4efdf",  "#d6eaf8",  "#f9f9f9")
        )
      )
  })

  output$dl_recog <- downloadHandler(
    filename = function() paste0("rapm_recognition_top", input$recog_topn, ".csv"),
    content  = function(file) write_csv(recog_filtered(), file)
  )

  # ── Metrics Explorer tab ─────────────────────────────────────────────────────
  cmp_filtered <- reactive({
    df <- combined_metrics

    # Season filter
    if (input$cmp_season != "all") {
      df <- df |> filter(rapm_season == as.integer(input$cmp_season))
    }

    # RAPM rank threshold (per season)
    df <- df |>
      group_by(rapm_season) |>
      filter(RAPM_rank <= input$cmp_topn) |>
      ungroup()

    # Optional player name search
    q <- trimws(input$cmp_player)
    if (nchar(q) > 0) {
      df <- df |> filter(str_detect(player_name, regex(q, ignore_case = TRUE)))
    }

    df
  })

  output$cmp_plot <- renderPlot({
    df  <- cmp_filtered()
    xcol <- input$cmp_x
    ycol <- input$cmp_y

    # Drop rows missing either axis metric
    plot_df <- df |>
      filter(!is.na(.data[[xcol]]), !is.na(.data[[ycol]]))

    if (nrow(plot_df) == 0) {
      par(mar = c(0, 0, 0, 0)); plot.new()
      text(0.5, 0.5, "No data available for selected metrics / filters.",
           cex = 1.2, col = "gray50", adj = 0.5)
      return()
    }

    # Label top-25 players by RAPM rank to avoid overplotting
    label_df <- plot_df |>
      group_by(rapm_season) |>
      slice_min(RAPM_rank, n = 25, with_ties = FALSE) |>
      ungroup()

    # Friendly axis labels from the flat lookup (no "Group.Label" prefix)
    x_lab <- METRIC_LABELS[xcol]; if (is.na(x_lab)) x_lab <- xcol
    y_lab <- METRIC_LABELS[ycol]; if (is.na(y_lab)) y_lab <- ycol

    ggplot(plot_df, aes(x = .data[[xcol]], y = .data[[ycol]],
                        colour = season_label, label = player_name)) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "gray70") +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "gray70") +
      geom_smooth(aes(group = 1), method = "lm", se = TRUE,
                  colour = "black", linewidth = 0.7, alpha = 0.12, show.legend = FALSE) +
      geom_point(size = 2.5, alpha = 0.8) +
      geom_text_repel(
        data         = label_df,
        size         = 2.8,
        max.overlaps = 12,
        show.legend  = FALSE
      ) +
      scale_colour_viridis_d(name = "Season") +
      labs(
        title    = paste0(x_lab, " vs ", y_lab),
        subtitle = paste0(
          "Top ", input$cmp_topn, " by RAPM rank",
          if (input$cmp_season != "all") paste0(" \u00b7 ", input$cmp_season) else " \u00b7 all seasons",
          "  \u00b7  regression line with 95% CI"
        ),
        x = x_lab,
        y = y_lab
      ) +
      theme_bw(base_size = 13) +
      theme(
        plot.title      = element_text(face = "bold"),
        legend.position = "right"
      )
  })

  output$cmp_tbl <- renderDT({
    df <- cmp_filtered()

    # Select columns that have at least one non-NA value
    metric_cols <- c(
      "RAPM",
      if (bbref_available) c("PER", "WS", "BPM", "VORP", "OBPM", "DBPM", "OWS", "DWS"),
      "MAMBA", "O_MAMBA", "D_MAMBA",
      "RAPTOR", "O_RAPTOR", "D_RAPTOR",
      "DARKO", "O_DARKO", "D_DARKO",
      "LEBRON", "O_LEBRON", "D_LEBRON"
    )
    metric_cols <- metric_cols[metric_cols %in% names(df)]
    keep_cols   <- c("season_label", "player_name", "RAPM_rank", metric_cols)

    display_df <- df |>
      select(all_of(keep_cols)) |>
      rename(Season = season_label, Player = player_name, `RAPM Rank` = RAPM_rank) |>
      arrange(`RAPM Rank`, Season)

    round_cols <- intersect(metric_cols, names(display_df))

    datatable(
      display_df,
      rownames  = FALSE,
      selection = "none",
      filter    = "top",
      options   = list(
        pageLength = 20,
        dom        = "ftip",
        scrollX    = TRUE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      )
    ) |>
      formatRound(round_cols, digits = 2) |>
      formatStyle("RAPM Rank",
        background         = styleColorBar(range(df$RAPM_rank, na.rm = TRUE), "#aed6f1"),
        backgroundSize     = "88% 55%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      )
  })

  output$dl_cmp <- downloadHandler(
    filename = function() {
      seas <- if (input$cmp_season == "all") "all" else input$cmp_season
      paste0("metrics_explorer_top", input$cmp_topn, "_", seas, ".csv")
    },
    content = function(file) write_csv(cmp_filtered(), file)
  )

  # ── Impact Metrics tab ───────────────────────────────────────────────────────
  # Reactive: filter each dataset by player search and selected component
  ext_player_long <- reactive({
    query <- trimws(input$ext_player)
    if (nchar(query) < 2) return(NULL)

    view <- input$ext_view
    col_suffix <- switch(view, Total = "", Offense = "O_", Defense = "D_")

    filter_player <- function(df) {
      df |> filter(str_detect(player_name, regex(query, ignore_case = TRUE)))
    }

    parts <- list()

    if ("MAMBA" %in% input$ext_metrics && nrow(ext_mamba) > 0) {
      d <- filter_player(ext_mamba)
      val_col <- paste0(col_suffix, "MAMBA")
      if (nrow(d) > 0 && val_col %in% names(d))
        parts[["MAMBA"]] <- d |>
          select(player_name, year, season_label, value = all_of(val_col)) |>
          mutate(metric = "MAMBA")
    }

    if ("RAPTOR" %in% input$ext_metrics && nrow(ext_raptor) > 0) {
      d <- filter_player(ext_raptor)
      val_col <- paste0(col_suffix, "RAPTOR")
      if (nrow(d) > 0 && val_col %in% names(d))
        parts[["RAPTOR"]] <- d |>
          select(player_name, year, season_label, value = all_of(val_col)) |>
          mutate(metric = "RAPTOR")
    }

    if ("DARKO" %in% input$ext_metrics && nrow(ext_darko) > 0) {
      d <- filter_player(ext_darko)
      val_col <- paste0(col_suffix, "DARKO")
      if (nrow(d) > 0 && val_col %in% names(d))
        parts[["DARKO"]] <- d |>
          select(player_name, year, season_label, value = all_of(val_col)) |>
          mutate(metric = "DARKO")
    }

    if ("LEBRON" %in% input$ext_metrics && nrow(ext_lebron) > 0) {
      d <- filter_player(ext_lebron)
      val_col <- paste0(col_suffix, "LEBRON")
      if (nrow(d) > 0 && val_col %in% names(d))
        parts[["LEBRON"]] <- d |>
          select(player_name, year, season_label, value = all_of(val_col)) |>
          mutate(metric = "LEBRON")
    }

    if (length(parts) == 0) return(NULL)
    bind_rows(parts) |>
      mutate(metric = factor(metric, levels = names(EXT_COLOURS)))
  })

  output$ext_plot <- renderPlot({
    df <- ext_player_long()

    if (is.null(df) || nrow(df) == 0) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5,
        if (nchar(trimws(input$ext_player)) < 2)
          "Enter at least 2 characters to search for a player."
        else
          "No data found. Try a different name or metric selection.",
        cex = 1.2, col = "gray50", adj = 0.5)
      return()
    }

    player_title <- unique(df$player_name) |> paste(collapse = " / ")
    view_label   <- input$ext_view

    # Season labels ordered by year
    lvls <- df |> arrange(year) |> pull(season_label) |> unique()
    df   <- df |> mutate(season_label = factor(season_label, levels = lvls))

    active_colours <- EXT_COLOURS[names(EXT_COLOURS) %in% unique(as.character(df$metric))]

    ggplot(df, aes(x = season_label, y = value,
                   colour = metric, group = metric)) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "gray70") +
      geom_line(linewidth = 1.2, na.rm = TRUE) +
      geom_point(size = 3, na.rm = TRUE) +
      scale_colour_manual(values = active_colours, name = "Metric") +
      scale_y_continuous(name = paste0(view_label, " Impact (pts/100 above avg)")) +
      labs(
        title    = paste0(player_title, " \u2014 ", view_label, " Impact History"),
        subtitle = "Points per 100 possessions above average \u00b7 Source: nbarapm.com",
        x        = "Season"
      ) +
      theme_bw(base_size = 13) +
      theme(
        plot.title      = element_text(face = "bold"),
        axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "top"
      )
  })

  output$ext_tbl <- renderDT({
    df <- ext_player_long()

    if (is.null(df) || nrow(df) == 0) {
      return(datatable(
        tibble(Message = "Enter a player name above to see results."),
        rownames = FALSE, options = list(dom = "t")
      ))
    }

    wide <- df |>
      pivot_wider(
        id_cols     = c(player_name, year, season_label),
        names_from  = metric,
        values_from = value
      ) |>
      arrange(year) |>
      select(Player = player_name, Season = season_label,
             any_of(c("MAMBA", "RAPTOR", "DARKO", "LEBRON")))

    metric_cols <- intersect(c("MAMBA", "RAPTOR", "DARKO", "LEBRON"), names(wide))

    datatable(wide, rownames = FALSE, selection = "none",
      options = list(pageLength = 15, dom = "ftp", scrollX = TRUE)
    ) |>
      formatRound(metric_cols, digits = 2) |>
      formatStyle(metric_cols,
        color = styleInterval(
          c(-1, 0, 1, 3),
          c("#c0392b", "#e67e22", "#555555", "#1a7a4a", "#0d5c38")
        )
      )
  })

  # ── Lineup novelty tab ───────────────────────────────────────────────────────
  output$lineup_plot <- renderPlot({
    yr <- as.integer(input$lineup_season)
    df <- new_lineups_all |> filter(season == yr)

    ggplot(df, aes(x = game_date, y = new_lineups, colour = factor(season))) +
      geom_line(alpha = 0.7) +
      geom_smooth(aes(group = 1), se = FALSE, colour = "black", linewidth = 0.8) +
      scale_x_date(date_breaks = "1 week", date_labels = "%W") +
      scale_colour_viridis_d(name = "Season start") +
      labs(
        title    = "New 5-man lineup combinations per day",
        subtitle = "First appearance across all seasons — home and away lineups combined",
        x        = NULL,
        y        = "New lineups"
      ) +
      theme_bw()
  })
}

shinyApp(ui, server)
