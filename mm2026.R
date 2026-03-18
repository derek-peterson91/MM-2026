

# Load packages and data
required_packages <- c(
  "dplyr", "tidyr", "purrr", "stringr", "lubridate",
  "slider", "tibble", "tidymodels", "xgboost", "hoopR"
)

to_install <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(slider)
library(tibble)
library(tidymodels)
library(xgboost)
library(hoopR)
library(progress)
library(purrr)
library(pROC)

tidymodels_prefer()

seasons <- 2024:2026
raw_box <- hoopR::load_mbb_team_box(seasons = seasons)

# Build team-game table
df <- raw_box %>%
  transmute(
    game_id,
    season,
    game_date = as.Date(game_date),
    team_id,
    team_short_display_name,
    team_display_name,
    team_home_away,
    pts = as.numeric(team_score),
    win = as.integer(team_winner),
    fgm = as.numeric(field_goals_made),
    fga = as.numeric(field_goals_attempted),
    fg3m = as.numeric(three_point_field_goals_made),
    fg3a = as.numeric(three_point_field_goals_attempted),
    ftm = as.numeric(free_throws_made),
    fta = as.numeric(free_throws_attempted),
    orb = as.numeric(offensive_rebounds),
    drb = as.numeric(defensive_rebounds),
    trb = orb + drb,
    ast = as.numeric(assists),
    stl = as.numeric(steals),
    blk = as.numeric(blocks),
    tov = as.numeric(turnovers),
    pf  = as.numeric(fouls)
  )

# Pair each team with opponent
games2 <- df %>%
  group_by(game_id) %>%
  mutate(n_teams = n()) %>%
  ungroup() %>%
  filter(n_teams == 2) %>%
  select(-n_teams)

team1 <- games2 %>%
  group_by(game_id) %>%
  slice(1) %>%
  ungroup() %>%
  rename_with(~ paste0(.x, "_team"), -game_id)

team2 <- games2 %>%
  group_by(game_id) %>%
  slice(2) %>%
  ungroup() %>%
  rename_with(~ paste0(.x, "_opp"), -game_id)

game_level_a <- team1 %>%
  left_join(team2, by = "game_id")

game_level_b <- team2 %>%
  left_join(team1, by = "game_id") %>%
  rename_with(~ sub("_opp$", "_tmp", .x)) %>%
  rename_with(~ sub("_team$", "_opp", .x)) %>%
  rename_with(~ sub("_tmp$", "_team", .x))

game_level <- bind_rows(game_level_a, game_level_b) %>%
  mutate(
    season = season_team,
    game_date = game_date_team,
    team_id = team_id_team,
    opp_id = team_id_opp,
    team_name = team_short_display_name_team,
    opp_name = team_short_display_name_opp,
    win = win_team,
    is_home = if_else(team_home_away_team == "home", 1, 0),
    is_away = if_else(team_home_away_team == "away", 1, 0),
    is_neutral = if_else(team_home_away_team == "neutral", 1, 0),
    poss_team = fga_team - orb_team + tov_team + 0.475 * fta_team,
    poss_opp  = fga_opp  - orb_opp  + tov_opp  + 0.475 * fta_opp,
    poss = (poss_team + poss_opp) / 2,
    off_eff = 100 * pts_team / poss,
    def_eff = 100 * pts_opp / poss,
    net_eff = off_eff - def_eff,
    efg = (fgm_team + 0.5 * fg3m_team) / fga_team,
    efg_d = (fgm_opp + 0.5 * fg3m_opp) / fga_opp,
    tov_rate = tov_team / poss_team,
    tov_rate_d = tov_opp / poss_opp,
    orb_rate = orb_team / (orb_team + drb_opp),
    drb_rate = drb_team / (drb_team + orb_opp),
    ftr = fta_team / fga_team,
    ftr_d = fta_opp / fga_opp,
    ast_rate = ast_team / fgm_team,
    three_par = fg3a_team / fga_team
  )

# Build pregame rolling features
team_history <- game_level %>%
  arrange(season, team_id, game_date, game_id) %>%
  group_by(season, team_id) %>%
  mutate(
    games_played = row_number(),
    games_before = games_played - 1,
    off_eff_pre = dplyr::lag(cummean(off_eff)),
    def_eff_pre = dplyr::lag(cummean(def_eff)),
    net_eff_pre = dplyr::lag(cummean(net_eff)),
    efg_pre = dplyr::lag(cummean(efg)),
    efg_d_pre = dplyr::lag(cummean(efg_d)),
    tov_pre = dplyr::lag(cummean(tov_rate)),
    tov_d_pre = dplyr::lag(cummean(tov_rate_d)),
    orb_pre = dplyr::lag(cummean(orb_rate)),
    drb_pre = dplyr::lag(cummean(drb_rate)),
    ftr_pre = dplyr::lag(cummean(ftr)),
    ftr_d_pre = dplyr::lag(cummean(ftr_d)),
    ast_pre = dplyr::lag(cummean(ast_rate)),
    three_par_pre = dplyr::lag(cummean(three_par)),
    pace_pre = dplyr::lag(cummean(poss))
  ) %>%
  ungroup()

# Build game-level modeling table
model_games <- team_history %>%
  select(
    game_id, season, game_date,
    team_id, opp_id, team_name, opp_name, win,
    is_home, is_away, is_neutral, games_before,
    off_eff_pre, def_eff_pre, net_eff_pre,
    efg_pre, efg_d_pre,
    tov_pre, tov_d_pre,
    orb_pre, drb_pre,
    ftr_pre, ftr_d_pre,
    ast_pre, three_par_pre, pace_pre
  )

team_a <- model_games %>%
  rename(
    team_id_a = team_id,
    opp_id_a = opp_id,
    team_name_a = team_name,
    opp_name_a = opp_name,
    win_a = win,
    is_home_a = is_home,
    is_away_a = is_away,
    is_neutral_a = is_neutral,
    games_before_a = games_before,
    off_eff_pre_a = off_eff_pre,
    def_eff_pre_a = def_eff_pre,
    net_eff_pre_a = net_eff_pre,
    efg_pre_a = efg_pre,
    efg_d_pre_a = efg_d_pre,
    tov_pre_a = tov_pre,
    tov_d_pre_a = tov_d_pre,
    orb_pre_a = orb_pre,
    drb_pre_a = drb_pre,
    ftr_pre_a = ftr_pre,
    ftr_d_pre_a = ftr_d_pre,
    ast_pre_a = ast_pre,
    three_par_pre_a = three_par_pre,
    pace_pre_a = pace_pre
  )

team_b <- model_games %>%
  rename(
    team_id_b = team_id,
    opp_id_b = opp_id,
    team_name_b = team_name,
    opp_name_b = opp_name,
    win_b = win,
    is_home_b = is_home,
    is_away_b = is_away,
    is_neutral_b = is_neutral,
    games_before_b = games_before,
    off_eff_pre_b = off_eff_pre,
    def_eff_pre_b = def_eff_pre,
    net_eff_pre_b = net_eff_pre,
    efg_pre_b = efg_pre,
    efg_d_pre_b = efg_d_pre,
    tov_pre_b = tov_pre,
    tov_d_pre_b = tov_d_pre,
    orb_pre_b = orb_pre,
    drb_pre_b = drb_pre,
    ftr_pre_b = ftr_pre,
    ftr_d_pre_b = ftr_d_pre,
    ast_pre_b = ast_pre,
    three_par_pre_b = three_par_pre,
    pace_pre_b = pace_pre
  )

matchup_df <- team_a %>%
  inner_join(
    team_b,
    by = c(
      "game_id",
      "team_id_a" = "opp_id_b",
      "opp_id_a" = "team_id_b"
    )
  ) %>%
  filter(team_id_a < opp_id_a) %>%
  mutate(
    target = win_a,
    net_eff_diff = net_eff_pre_a - net_eff_pre_b,
    efg_diff = efg_pre_a - efg_pre_b,
    tov_diff = tov_pre_a - tov_pre_b,
    orb_diff = orb_pre_a - orb_pre_b,
    ftr_diff = ftr_pre_a - ftr_pre_b,
    ast_diff = ast_pre_a - ast_pre_b,
    three_par_diff = three_par_pre_a - three_par_pre_b,
    pace_diff = pace_pre_a - pace_pre_b,
    exp_diff = games_before_a - games_before_b,
    home_edge = is_home_a - is_home_b
  )

# Keep only usable modeling rows
model_df <- matchup_df %>%
  filter(games_before_a >= 5, games_before_b >= 5) %>%
  filter(
    !is.na(target),
    !is.na(net_eff_diff),
    !is.na(efg_diff),
    !is.na(tov_diff),
    !is.na(orb_diff),
    !is.na(ftr_diff),
    !is.na(ast_diff),
    !is.na(three_par_diff),
    !is.na(pace_diff),
    !is.na(exp_diff),
    !is.na(home_edge)
  )

# Train baseline model
train_df <- model_df %>% filter(season.x == 2024:2025)
test_df  <- model_df %>% filter(season.x == 2026)

fit_glm <- glm(
  target ~ 
    net_eff_diff +
    efg_diff +
    tov_diff +
    orb_diff +
    ftr_diff +
    ast_diff +
    three_par_diff +
    pace_diff +
    exp_diff +
    home_edge,
  data = train_df,
  family = binomial()
)

test_df$pred_prob <- predict(fit_glm, newdata = test_df, type = "response")
roc_obj <- roc(test_df$target, test_df$pred_prob)
auc(roc_obj)

# Clean version of the ratings layer
ratings_base <- game_level %>%
  select(
    season,
    game_id,
    team_id,
    opp_id,
    team_name,
    opp_name,
    off_eff,
    def_eff
  )

# Initialize ratings
teams <- ratings_base %>%
  group_by(season, team_id) %>%
  summarise(
    team_name = dplyr::first(na.omit(team_name)),
    .groups = "drop"
  )

ratings <- teams %>%
  distinct(season, team_id, .keep_all = TRUE) %>%
  mutate(
    adj_off = 100,
    adj_def = 100
  )

# Iterate
for (i in 1:10) {
  ratings_new <- ratings_base %>%
    left_join(
      ratings %>% select(season, team_id, adj_off, adj_def),
      by = c("season", "team_id")
    ) %>%
    rename(
      team_adj_off = adj_off,
      team_adj_def = adj_def
    ) %>%
    left_join(
      ratings %>% select(season, team_id, adj_off, adj_def),
      by = c("season", "opp_id" = "team_id")
    ) %>%
    rename(
      opp_adj_off = adj_off,
      opp_adj_def = adj_def
    ) %>%
    mutate(
      adj_off_game = off_eff / opp_adj_def * 100,
      adj_def_game = def_eff / opp_adj_off * 100
    ) %>%
    group_by(season, team_id) %>%
    summarise(
      team_name = dplyr::first(na.omit(team_name)),
      adj_off = mean(adj_off_game, na.rm = TRUE),
      adj_def = mean(adj_def_game, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    distinct(season, team_id, .keep_all = TRUE)
  
  ratings <- ratings_new
}

team_ratings <- ratings %>%
  mutate(adj_net = adj_off - adj_def) %>%
  arrange(season, desc(adj_net))

# Create prior games table
ratings_games <- game_level %>%
  select(
    season,
    game_date,
    game_id,
    team_id,
    opp_id,
    team_name,
    off_eff,
    def_eff
  ) %>%
  arrange(season, game_date, game_id)

# Create season-date grid
season_dates <- ratings_games %>%
  distinct(season, game_date) %>%
  arrange(season, game_date)


# Helper function to compute adjusted ratings from a subset of games
compute_adj_ratings <- function(games_subset, n_iter = 10) {
  
  if (nrow(games_subset) == 0) {
    return(tibble::tibble(
      team_id = integer(),
      team_name = character(),
      adj_off = numeric(),
      adj_def = numeric(),
      adj_net = numeric()
    ))
  }
  
  teams <- games_subset %>%
    group_by(team_id) %>%
    summarise(
      team_name = dplyr::first(na.omit(team_name)),
      .groups = "drop"
    ) %>%
    distinct(team_id, .keep_all = TRUE)
  
  ratings <- teams %>%
    mutate(
      adj_off = 100,
      adj_def = 100
    )
  
  for (i in 1:n_iter) {
    ratings <- games_subset %>%
      left_join(
        ratings %>% select(team_id, adj_off, adj_def),
        by = "team_id"
      ) %>%
      rename(
        team_adj_off = adj_off,
        team_adj_def = adj_def
      ) %>%
      left_join(
        ratings %>% select(team_id, adj_off, adj_def),
        by = c("opp_id" = "team_id")
      ) %>%
      rename(
        opp_adj_off = adj_off,
        opp_adj_def = adj_def
      ) %>%
      mutate(
        adj_off_game = off_eff / opp_adj_def * 100,
        adj_def_game = def_eff / opp_adj_off * 100
      ) %>%
      group_by(team_id) %>%
      summarise(
        team_name = dplyr::first(na.omit(team_name)),
        adj_off = mean(adj_off_game, na.rm = TRUE),
        adj_def = mean(adj_def_game, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      distinct(team_id, .keep_all = TRUE)
  }
  
  ratings %>%
    mutate(adj_net = adj_off - adj_def)
}

# Build daily pre-game adjusted ratings
pregame_adj_ratings_list <- vector("list", nrow(season_dates))

for (i in seq_len(nrow(season_dates))) {
  
  this_season <- season_dates$season[i]
  this_date   <- season_dates$game_date[i]
  
  games_before <- ratings_games %>%
    filter(season == this_season, game_date < this_date)
  
  if (nrow(games_before) == 0) {
    pregame_adj_ratings_list[[i]] <- tibble::tibble(
      season = integer(),
      game_date = as.Date(character()),
      team_id = integer(),
      team_name = character(),
      adj_off_pre = numeric(),
      adj_def_pre = numeric(),
      adj_net_pre = numeric()
    )
  } else {
    this_ratings <- compute_adj_ratings(games_before, n_iter = 10) %>%
      transmute(
        season = this_season,
        game_date = this_date,
        team_id,
        team_name,
        adj_off_pre = adj_off,
        adj_def_pre = adj_def,
        adj_net_pre = adj_net
      )
    
    pregame_adj_ratings_list[[i]] <- this_ratings
  }
  
  if (i %% 10 == 0) {
    cat("Completed", i, "of", nrow(season_dates), "dates\n")
  }
}

pregame_adj_ratings <- dplyr::bind_rows(pregame_adj_ratings_list)

# merge pregame adjusted ratings into the matchup model
matchup_df2 <- matchup_df %>%
  left_join(
    pregame_adj_ratings %>%
      select(season, game_date, team_id, adj_net_pre),
    by = c("season.x" = "season", "game_date.x" = "game_date", "team_id_a" = "team_id")
  ) %>%
  rename(adj_net_pre_a = adj_net_pre) %>%
  left_join(
    pregame_adj_ratings %>%
      select(season, game_date, team_id, adj_net_pre),
    by = c("season.x" = "season", "game_date.x" = "game_date", "opp_id_a" = "team_id")
  ) %>%
  rename(adj_net_pre_b = adj_net_pre) %>%
  mutate(
    adj_net_diff = adj_net_pre_a - adj_net_pre_b
  )

# Build the upgraded model dataset
model_df2 <- matchup_df2 %>%
  filter(games_before_a >= 5, games_before_b >= 5) %>%
  filter(
    !is.na(target),
    !is.na(adj_net_diff),
    !is.na(efg_diff),
    !is.na(tov_diff),
    !is.na(orb_diff),
    !is.na(ftr_diff),
    !is.na(ast_diff),
    !is.na(three_par_diff),
    !is.na(pace_diff),
    !is.na(exp_diff),
    !is.na(home_edge)
  )

# Train and test
train_df2 <- model_df2 %>% filter(season.x == 2024 | season.x == 2025)
test_df2  <- model_df2 %>% filter(season.x == 2026)

dim(train_df2)
dim(test_df2)

# Fit the model
fit_glm2 <- glm(
  target ~ 
    adj_net_diff +
    efg_diff +
    tov_diff +
    orb_diff +
    ftr_diff +
    ast_diff +
    three_par_diff +
    pace_diff +
    exp_diff +
    home_edge,
  data = train_df2,
  family = binomial()
)

summary(fit_glm2)

test_df2$pred_prob <- predict(fit_glm2, newdata = test_df2, type = "response")

roc_obj2 <- roc(test_df2$target, test_df2$pred_prob)
auc(roc_obj2)

# Make predictions
latest_adj <- pregame_adj_ratings %>%
  group_by(season, team_id) %>%
  slice_tail(n = 1) %>%
  ungroup()

predict_matchup_glm2 <- function(team_a, team_b, snapshots, adj_tbl, model) {
  
  a1 <- snapshots %>% filter(team_name == team_a) %>% slice(1)
  b1 <- snapshots %>% filter(team_name == team_b) %>% slice(1)
  
  a2 <- adj_tbl %>% filter(team_id == a1$team_id) %>% slice(1)
  b2 <- adj_tbl %>% filter(team_id == b1$team_id) %>% slice(1)
  
  new_data <- data.frame(
    adj_net_diff   = a2$adj_net_pre - b2$adj_net_pre,
    efg_diff       = a1$efg_pre - b1$efg_pre,
    tov_diff       = a1$tov_pre - b1$tov_pre,
    orb_diff       = a1$orb_pre - b1$orb_pre,
    ftr_diff       = a1$ftr_pre - b1$ftr_pre,
    ast_diff       = a1$ast_pre - b1$ast_pre,
    three_par_diff = a1$three_par_pre - b1$three_par_pre,
    pace_diff      = a1$pace_pre - b1$pace_pre,
    exp_diff       = a1$games_before - b1$games_before,
    home_edge      = 0
  )
  
  prob <- predict(model, newdata = new_data, type = "response")
  
  data.frame(
    team_a = team_a,
    team_b = team_b,
    prob_team_a_win = prob
  )
}

team_latest_stats <- team_history %>%
  arrange(season, team_id, game_date, game_id) %>%
  group_by(season, team_id) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(
    season,
    team_id,
    team_name,
    game_date,
    games_before,
    efg_pre,
    tov_pre,
    orb_pre,
    ftr_pre,
    ast_pre,
    three_par_pre,
    pace_pre
  )

team_latest_adj <- pregame_adj_ratings %>%
  arrange(season, team_id, game_date) %>%
  group_by(season, team_id) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(
    season,
    team_id,
    team_name,
    game_date,
    adj_off_pre,
    adj_def_pre,
    adj_net_pre
  )

team_rankings <- team_latest_adj %>%
  arrange(season, desc(adj_net_pre)) %>%
  group_by(season) %>%
  mutate(rank = row_number()) %>%
  ungroup()

team_rankings %>%
  filter(season == max(season)) %>%
  select(rank, team_name, adj_off_pre, adj_def_pre, adj_net_pre) %>%
  print(n = 100)


predict_matchup_glm2("Houston", "Purdue", team_snapshots, latest_adj, fit_glm2)

predict_total <- function(a, b) {
  A <- team_latest_adj %>% filter(team_name == a) %>% slice(1)
  B <- team_latest_adj %>% filter(team_name == b) %>% slice(1)
  S <- team_latest_stats %>% filter(team_name %in% c(a,b))
  
  pace <- mean(S$pace_pre)
  
  pts_a <- pace * (A$adj_off_pre / B$adj_def_pre)
  pts_b <- pace * (B$adj_off_pre / A$adj_def_pre)
  
  pts_a + pts_b
}

predict_total("Houston", "Purdue")
