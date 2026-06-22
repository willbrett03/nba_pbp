# stint_to_rapm.R
#
# Two independent functions for the RAPM pipeline:
#
#   prepare_season_data(year)
#     Reads stints_YYYY_YY.csv, builds lineup-level moving-average priors
#     (reset fresh each season), expands player columns with parallel name
#     columns, and writes combined_lineups_YYYY_YY.rds.
#
#   fit_season_rapm(year)
#     Reads combined_lineups_YYYY_YY.rds, fits ORTG and DRTG ridge models
#     via cv.glmnet, joins player names into results, and writes
#     rapm_YYYY_YY.csv.
#
# Usage:
#   prepare_season_data(2022)
#   fit_season_rapm(2022)
#
#   # Or run all seasons:
#   walk(2016:2024, prepare_season_data)
#   walk(2016:2024, fit_season_rapm)

library(tidyverse)
library(Matrix)
library(glmnet)

# ── Configuration ─────────────────────────────────────────────────────────────

# Resolve the directory that contains this script so the file can be run from
# any working directory.  Works in RStudio (interactive) and via Rscript.
.script_dir <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    dirname(rstudioapi::getActiveDocumentContext()$path)
  } else {
    args     <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg) > 0) {
      dirname(normalizePath(sub("^--file=", "", file_arg)))
    } else {
      getwd()
    }
  }
}

STINT_DIR <- .script_dir()
MIN_POSS  <- 1       # drop stints where either team has fewer possessions

# Loaded once at source time and shared by both functions
player_names <- read_csv(
  file.path(STINT_DIR, "player_names.csv"),
  col_types = cols(player_id = col_integer(), player_name = col_character())
)

# ── Shared helpers ────────────────────────────────────────────────────────────

season_tag <- function(year) sprintf("%d_%02d", year, (year + 1) %% 100)

# Expand an underscore-joined lineup string into one column per player position.
# prefix = "P" for home team, "DP" for away team.
expand_players <- function(df, lineup_col, prefix) {
  df |>
    select(game_id, stint_id, lineup = {{ lineup_col }}) |>
    separate_rows(lineup, sep = "_") |>
    group_by(game_id, stint_id) |>
    mutate(pos = paste0(prefix, row_number())) |>
    pivot_wider(names_from = pos, values_from = lineup)
}

# Add parallel _name columns alongside ID columns for a set of player positions.
# id_cols:   e.g. c("P1","P2","P3","P4","P5")
# name_cols: e.g. c("P1_name","P2_name","P3_name","P4_name","P5_name")
add_player_names <- function(df, id_cols, name_cols) {
  for (i in seq_along(id_cols)) {
    id_col   <- id_cols[i]
    name_col <- name_cols[i]
    # Cast ID column to integer to match player_names$player_id
    df <- df |>
      mutate(!!id_col := as.integer(.data[[id_col]])) |>
      left_join(
        player_names |> rename(!!id_col := player_id, !!name_col := player_name),
        by = id_col
      )
  }
  df
}

# Sparse indicator matrix: rows = stints, cols = players, values = +1 or -1.
make_sparse <- function(df, player_cols, value, players) {
  n   <- nrow(df)
  p   <- length(players)
  mat <- Matrix(0, nrow = n, ncol = p, sparse = TRUE,
                dimnames = list(NULL, players))
  for (col in player_cols) {
    ids <- df[[col]]
    for (i in seq_len(n)) {
      j <- match(ids[i], players)
      if (!is.na(j)) mat[i, j] <- value
    }
  }
  mat
}


# ═══════════════════════════════════════════════════════════════════════════════
# SECTION 1 — DATA PREPARATION
# ═══════════════════════════════════════════════════════════════════════════════

prepare_season_data <- function(year) {

  tag <- season_tag(year)
  message("\n── prepare_season_data(", year, ") ── season ", tag, " ──────────────────")

  # ── 1. Load ────────────────────────────────────────────────────────────────

  path <- file.path(STINT_DIR, sprintf("stints_%s.csv", tag))
  if (!file.exists(path)) stop("File not found: ", path)

  stints <- read_csv(path, col_types = cols(
    game_id          = col_character(),
    game_date        = col_date(),
    period           = col_integer(),
    stint_id         = col_integer(),
    home_team_id     = col_character(),
    away_team_id     = col_character(),
    home_lineup      = col_character(),
    away_lineup      = col_character(),
    home_possessions = col_integer(),
    away_possessions = col_integer(),
    n_pos            = col_integer(),
    home_points      = col_integer(),
    away_points      = col_integer(),
    start_game_clock = col_double(),
    end_game_clock   = col_double(),
    seconds_played   = col_double()
  ), show_col_types = FALSE) |>
    mutate(season = year)

  message("  Loaded ", nrow(stints), " stints from ", n_distinct(stints$game_id), " games")

  # ── 2. Clean ───────────────────────────────────────────────────────────────

  stints <- stints |>
    filter(home_possessions >= MIN_POSS, away_possessions >= MIN_POSS) |>
    mutate(minutes = seconds_played / 60) |>
    arrange(game_date, game_id, period, stint_id)

  # ── 3. Stint-level ratings ─────────────────────────────────────────────────

  stints <- stints |>
    mutate(
      home_ORTG = 100 * home_points / home_possessions,
      home_DRTG = 100 * away_points / away_possessions,
      margin    = 100 * (home_points - away_points) / n_pos
    )

  # ── 4. Lineup-level moving-average priors ──────────────────────────────────
  #
  # Prior resets each season — no carry-over from previous years.
  # Collapse stints to game-level totals first so cummean counts games,
  # not individual stints. A lineup with multiple stints in one game gets
  # one entry in its history, and all stints in that game share the same
  # pre-game prior.

  home_game <- stints |>
    group_by(lineup = home_lineup, game_id, game_date) |>
    summarise(
      points = sum(home_points), poss = sum(home_possessions),
      opp_points = sum(away_points), opp_poss = sum(away_possessions),
      .groups = "drop"
    ) |>
    mutate(ORTG_game = 100 * points / poss,
           DRTG_game = 100 * opp_points / opp_poss)

  away_game <- stints |>
    group_by(lineup = away_lineup, game_id, game_date) |>
    summarise(
      points = sum(away_points), poss = sum(away_possessions),
      opp_points = sum(home_points), opp_poss = sum(home_possessions),
      .groups = "drop"
    ) |>
    mutate(ORTG_game = 100 * points / poss,
           DRTG_game = 100 * opp_points / opp_poss)

  all_lineup_games <- bind_rows(home_game, away_game) |>
    group_by(lineup, game_id, game_date) |>
    summarise(
      points = sum(points), poss = sum(poss),
      opp_points = sum(opp_points), opp_poss = sum(opp_poss),
      .groups = "drop"
    ) |>
    mutate(ORTG_game = 100 * points / poss,
           DRTG_game = 100 * opp_points / opp_poss) |>
    arrange(game_date, game_id)

  lineup_priors <- all_lineup_games |>
    group_by(lineup) |>
    mutate(
      ORTG_RA = lag(cummean(ORTG_game)),
      DRTG_RA = lag(cummean(DRTG_game))
    ) |>
    ungroup() |>
    select(lineup, game_id, ORTG_RA, DRTG_RA)

  stints <- stints |>
    left_join(
      lineup_priors |> rename(home_lineup = lineup,
                              ORTG_RA_home = ORTG_RA, DRTG_RA_home = DRTG_RA),
      by = c("home_lineup", "game_id")
    ) |>
    left_join(
      lineup_priors |> rename(away_lineup = lineup,
                              ORTG_RA_away = ORTG_RA, DRTG_RA_away = DRTG_RA),
      by = c("away_lineup", "game_id")
    )

  # ── 5. Build combined_lineups ───────────────────────────────────────────────

  home_players_wide <- expand_players(stints, home_lineup, "P")
  away_players_wide <- expand_players(stints, away_lineup, "DP")

  combined_lineups <- stints |>
    select(
      game_id, game_date, season, period, stint_id,
      home_team_id, away_team_id,
      home_lineup, away_lineup,
      pts_home  = home_points,      pts_away  = away_points,
      poss_home = home_possessions, poss_away = away_possessions,
      seconds_played, minutes, margin,
      home_ORTG, home_DRTG,
      ORTG_RA_home, DRTG_RA_home,
      ORTG_RA_away, DRTG_RA_away
    ) |>
    left_join(home_players_wide, by = c("game_id", "stint_id")) |>
    left_join(away_players_wide, by = c("game_id", "stint_id")) |>
    mutate(
      ORTG_RA_team = ORTG_RA_home,
      DRTG_RA_opp  = DRTG_RA_away,
      ORTG_RA_opp  = ORTG_RA_away,
      DRTG_RA_team = DRTG_RA_home,
      ORTG  = home_ORTG,
      DRTG  = home_DRTG,
      netrt = ORTG - DRTG
    ) |>
    select(-ORTG_RA_home, -DRTG_RA_home, -ORTG_RA_away, -DRTG_RA_away,
           -home_ORTG, -home_DRTG) |>
    filter(
      !is.na(ORTG_RA_team), !is.na(DRTG_RA_opp),
      !is.na(ORTG_RA_opp),  !is.na(DRTG_RA_team),
      !is.infinite(ORTG),   !is.infinite(DRTG),
      !is.na(ORTG),         !is.na(DRTG)
    )

  # ── 6. Add parallel player name columns ────────────────────────────────────

  combined_lineups <- combined_lineups |>
    add_player_names(
      id_cols   = paste0("P",  1:5),
      name_cols = paste0("P",  1:5, "_name")
    ) |>
    add_player_names(
      id_cols   = paste0("DP", 1:5),
      name_cols = paste0("DP", 1:5, "_name")
    )

  # Reorder: keep each ID column immediately followed by its name column
  id_name_pairs <- c(rbind(paste0("P",  1:5), paste0("P",  1:5, "_name"),
                           paste0("DP", 1:5), paste0("DP", 1:5, "_name")))
  other_cols <- setdiff(names(combined_lineups), id_name_pairs)
  combined_lineups <- combined_lineups |> select(all_of(other_cols), all_of(id_name_pairs))

  message("  Retained ", nrow(combined_lineups), " stints after prior filter")
  message("  Unique players (home): ",
          n_distinct(c(combined_lineups$P1, combined_lineups$P2,
                       combined_lineups$P3, combined_lineups$P4,
                       combined_lineups$P5)))

  # ── 7. Save ────────────────────────────────────────────────────────────────

  out_path <- file.path(STINT_DIR, sprintf("combined_lineups_%s.rds", tag))
  saveRDS(combined_lineups, out_path)
  message("  Saved → ", out_path)

  invisible(combined_lineups)
}


# ═══════════════════════════════════════════════════════════════════════════════
# SECTION 2 — MODEL FITTING
# ═══════════════════════════════════════════════════════════════════════════════

fit_season_rapm <- function(year) {
  # Gold-standard single-regression RAPM following the scorenetwork pipeline.
  #
  # Reads directly from the raw stint CSV — bypasses the combined_lineups RDS
  # so that all stints are used, not just those that survived the prior filter.
  #
  # Outcome:  margin = 100 * (home_points - away_points) / n_pos  (per stint)
  # Encoding: home players = +1, away players = -1
  # Model:    ridge regression (alpha = 0) via cv.glmnet
  #
  # Output: stint_data/rapm_YYYY_YY.csv

  tag <- season_tag(year)
  message("\n── fit_season_rapm(", year, ") ── season ", tag, " ──────────────────────")

  # ── 1. Load raw stint CSV ─────────────────────────────────────────────────

  in_path <- file.path(STINT_DIR, sprintf("stints_%s.csv", tag))
  if (!file.exists(in_path)) {
    stop("Stint file not found: ", in_path)
  }

  stints <- read_csv(in_path, col_types = cols(
    game_id          = col_character(),
    game_date        = col_date(),
    period           = col_integer(),
    stint_id         = col_integer(),
    home_team_id     = col_character(),
    away_team_id     = col_character(),
    home_lineup      = col_character(),
    away_lineup      = col_character(),
    home_possessions = col_integer(),
    away_possessions = col_integer(),
    n_pos            = col_integer(),
    home_points      = col_integer(),
    away_points      = col_integer(),
    start_game_clock = col_double(),
    end_game_clock   = col_double(),
    seconds_played   = col_double()
  ), show_col_types = FALSE) |>
    filter(n_pos > 0) |>
    mutate(margin = 100 * (home_points - away_points) / n_pos)

  message("  Loaded ", nrow(stints), " stints from ", n_distinct(stints$game_id), " games")

  # ── 2. Expand lineups into player columns ──────────────────────────────────

  home_wide <- stints |>
    select(game_id, stint_id, home_lineup) |>
    separate_rows(home_lineup, sep = "_") |>
    group_by(game_id, stint_id) |>
    mutate(pos = paste0("P", row_number())) |>
    pivot_wider(names_from = pos, values_from = home_lineup)

  away_wide <- stints |>
    select(game_id, stint_id, away_lineup) |>
    separate_rows(away_lineup, sep = "_") |>
    group_by(game_id, stint_id) |>
    mutate(pos = paste0("DP", row_number())) |>
    pivot_wider(names_from = pos, values_from = away_lineup)

  stints <- stints |>
    left_join(home_wide, by = c("game_id", "stint_id")) |>
    left_join(away_wide, by = c("game_id", "stint_id")) |>
    mutate(across(c(P1:P5, DP1:DP5), as.integer))

  # ── 3. Player universe ─────────────────────────────────────────────────────

  unique_players <- unique(na.omit(as.integer(c(
    stints$P1,  stints$P2,  stints$P3,  stints$P4,  stints$P5,
    stints$DP1, stints$DP2, stints$DP3, stints$DP4, stints$DP5
  ))))
  message("  Player universe: ", length(unique_players), " players")

  # ── 4. Build ±1 design matrix ─────────────────────────────────────────────

  X_home <- make_sparse(stints, paste0("P",  1:5), +1, unique_players)
  X_away <- make_sparse(stints, paste0("DP", 1:5), -1, unique_players)
  X      <- X_home + X_away

  # ── 5. Ridge regression ───────────────────────────────────────────────────

  set.seed(42)
  cv_fit <- cv.glmnet(X, stints$margin,
                      alpha = 0, nfolds = 10, standardize = FALSE)
  fit    <- glmnet(X, stints$margin,
                   alpha = 0, standardize = FALSE,
                   lambda = cv_fit$lambda.min)

  coefs <- as.matrix(coef(fit))[-1, , drop = FALSE]

  # ── 6. Assemble results ────────────────────────────────────────────────────

  RAPM <- tibble(
    player_id = unique_players,
    RAPM      = coefs[, 1]
  ) |>
    mutate(
      season      = year,
      player_name = player_names$player_name[match(player_id, player_names$player_id)],
      player_id   = as.character(player_id)
    ) |>
    arrange(desc(RAPM)) |>
    mutate(RAPM_rank = row_number()) |>
    select(season, player_id, player_name, RAPM, RAPM_rank)

  message("  Model fit complete")
  print(RAPM |> head(10))

  # ── 7. Save ────────────────────────────────────────────────────────────────

  out_path <- file.path(STINT_DIR, sprintf("rapm_%s.csv", tag))
  write_csv(RAPM, out_path)
  message("  Saved → ", out_path)

  invisible(RAPM)
}

## Visualize the number of unique 5-man lineups that are new each day

library(tidyverse)

# Load and stack all seasons
season_tag <- function(year) sprintf("%d_%02d", year, (year + 1) %% 100)

stints_all <- map(2016:2024, function(year) {
  path <- file.path(STINT_DIR, sprintf("stints_%s.csv", season_tag(year)))
  if (!file.exists(path)) return(NULL)
  read_csv(path, col_types = cols(
    game_id   = col_character(),
    game_date = col_date(),
    .default  = col_guess()
  ), show_col_types = FALSE) |>
    mutate(season = year)
}) |> list_rbind()

# Stack home and away lineups with their dates
all_lineups <- bind_rows(
  stints_all |> select(game_date, season, lineup = home_lineup),
  stints_all |> select(game_date, season, lineup = away_lineup)
) |>
  drop_na(game_date, lineup)

# For each lineup, find the first date it ever appeared

first_appearances <- all_lineups |>
  group_by(season, lineup) |>
  slice_min(game_date, n = 1, with_ties = FALSE) |>
  ungroup() |>
  filter(season == 2022)

# Count new lineups per day
new_per_day <- first_appearances |>
  count(game_date, season, name = "new_lineups") |>
  arrange(game_date)

# Plot
ggplot(new_per_day, aes(x = game_date, y = new_lineups, colour = factor(season))) +
  geom_line(alpha = 0.7) +
  geom_smooth(aes(group = 1), se = FALSE, colour = "black", linewidth = 0.8) +
  scale_x_date(date_breaks = "1 week", date_labels = "%W") +
  scale_colour_viridis_d(name = "Season start") +
  labs(
    title = "New 5-man lineup combinations per day",
    subtitle = "First appearance across all seasons — home and away lineups combined",
    x = NULL,
    y = "New lineups"
  ) +
  theme_bw()