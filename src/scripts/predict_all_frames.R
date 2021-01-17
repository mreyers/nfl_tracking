flog.appender(appender.file('logs/predict_all_frames.log'), 'all_pred')
flog.info('Reading in the data. Looped due to file name.', name = 'all_pred')

# Need to load in data in a slightly different fashion due to my code changes
# all_frames_covariates are now individual files

# Read in because of earlier memory challenges
all_frames_data <- tibble()
covariate_files <- list.files(paste0(default_path, time_of_arrival_explicit),
                              pattern = "all_frames_covariates")
for(i in 1:length(covariate_files)){
  
  temp <- readRDS(paste0(default_path, time_of_arrival_explicit, covariate_files[i]))
  all_frames_data <- all_frames_data %>%
    bind_rows(temp)
  rm(temp)
}

sack_data <- readRDS(glue('{default_path}{time_of_arrival_explicit}/sack_and_rush_plays_frames.rds'))

all_frames_data <- all_frames_data %>%
  bind_rows(sack_data)

# Quick fix for a column that is still nested
temp <- all_frames_data %>% 
  group_by(game_id, play_id) %>% 
  slice(1) %>%
  select(game_id, play_id, first_elig, last_elig, pocket_dist) %>%
  unnest(pocket_dist) %>% 
  mutate(frame_id_2 = first_elig + row_number() - 1) %>%
  select(game_id, play_id, frame_id_2, dist_from_pocket)

all_frames_data <- all_frames_data %>%
  dplyr::select(-pocket_dist) %>%
  left_join(temp, by = c('game_id', 'play_id', 'frame_id_2'))

# Time to redo predictions in the more appropriate tidymodels framework
# So long h2o, you were a real one
cp_at_release_model <- readRDS("~/GitHub/nfl_tracking/src/Data/release/comp_prob.rds")

glimpse(cp_at_release_model$train)

# Need to update some covariate names and add context variables
games_reduced <- games %>%
  select(game_id, home_team_abbr, visitor_team_abbr)

nfl_pbp <- plays %>%
  left_join(games_reduced, by = "game_id") %>%
  filter(!is.na(pass_result)) %>%
  mutate(yardline_100 = if_else(possession_team == yardline_side,
                                100 - yardline_number,
                                yardline_number),
         yardline_100 = if_else(is.na(yardline_100), 50, yardline_100),
         score_differential = if_else(home_team_abbr == possession_team,
                                      visitor_score_before_play - home_score_before_play,
                                      home_score_before_play - visitor_score_before_play),
         is_redzone = factor(yardline_100 < 20, levels = c(FALSE, TRUE))) %>%
  select(game_id,
         play_id, yardline_100, down, ydstogo = yards_to_go,
         score_differential, is_redzone, number_of_pass_rushers)

names(all_frames_data)


# Actual covariates I need
all_covariates_data <- all_frames_data %>%
  ungroup() %>%
  mutate(pass_result_fake = if_else(pass_result == "C", "C", "I"),
         pass_result_f = factor(pass_result_fake, levels = c("C", "I"))) %>% 
  left_join(nfl_pbp, by = c('game_id', 'play_id')) %>%
  left_join(players %>% select(nfl_id, position), by = c("nfl_id")) %>%
  mutate(position_f = factor(case_when(
    position == "WR" ~ "WR",
    position == "TE" ~ "TE",
    TRUE ~ "RB"
  ), levels = c("WR", "TE", "RB"))) %>%
  select(-position) %>%
  select(pass_result_f, air_dist, rec_separation,
         sideline_sep,
         qb_vel, time_to_throw, dist_from_pocket, 
         # Ownership Features, only used 1 due to colinearity
         n_cells = player_centric_n_cells_at_throw,
         own_intensity = player_centric_own_intensity_at_throw,
         own_avg_intensity = player_centric_own_avg_intensity_at_throw,
         # Additional features that differ slightly
         air_yards_x,
         # Context Features
         yardline_100, down, ydstogo,
         # Additional features created for calibration testing
         score_differential, is_redzone, number_of_pass_rushers, position_f)

# Remove any NA values we have or PCA will fail
all_covariates_data <- all_covariates_data %>%
  filter(!is.na(rec_separation), !is.na(air_yards_x))

all_covariates_and_preds <- all_covariates_data %>% 
  bind_cols(predict(cp_at_release_model, ., type = "prob"))

# Need to add back game_id and play_id
add_game_play_cols <- all_frames_data %>%
  filter(!is.na(rec_separation), !is.na(air_yards_x)) %>%
  select(game_id, play_id, nfl_id, display_name, target, frame_id_2)

all_covariates_preds_and_ids <- add_game_play_cols %>%
  bind_cols(all_covariates_and_preds)

saveRDS(all_covariates_preds_and_ids, "Data/release/all_covariates_preds_and_ids.rds")
