# Fixing EP calculations as the current knn approach is obviously faulty
flog.appender(appender.file('logs/redo_ep.log'), 'redo')
flog.info('EP was previously calculated using a lazy approach. Doing it
          better by now using Yurko model to fit estimates. Will do both
          EP and WP in this script. Just functions to be called later.', name = 'redo')

# Just need to bring in Yurko model and make sure I have relevant covariates
# devtools::install_github(repo = "maksimhorowitz/nflscrapR")
library(nflscrapR)

# Update data to nflfastR
ep_requirements <- nflfastr_stuff %>%
  dplyr::select(game_id = old_game_id, play_id, half_seconds_remaining,
                yardline_100, down, ydstogo,
                season, home_team, posteam, roof,
                posteam_timeouts_remaining, defteam_timeouts_remaining,
                # This is a relic to compare against nflscrapR model
                goal_to_go)
# Don't need to load sysdata in nflfastR, can just use their helper function
# To use, must have:
  #' season, home_team
  #' posteam, roof (coded as 'outdoors', 'dome', or {'open' / 'closed' / NA} (retractable))}
  #' half_seconds_remaining, yardline_100, down, ydstogo,
  #' posteam_timeouts_remaining, defteam_timeouts_remaining

# Full data set is a combination of yards_downfield and YAC estimates at each frame
  # Simpler dataset, grab the minimal
yards_downfield <- readRDS(glue('{default_path}{time_of_arrival_explicit}/yards_to_be_gained_{epsilon}add_frames.rds')) %>%
  filter(game_id == 2017090700, play_id == 68) %>%
  select(game_id, play_id, nfl_id, frame_id, arrival_x, yards_downfield)
  # Running dataset, include all fields
yards_after_catch_est <- readRDS("Data_from_complete_runs/Data/release/all_preds_and_covariates_complete.rds") %>%
  filter(game_id == 2017090700, play_id == 68)

all_yards_measures <- yards_after_catch_est %>%
  left_join(yards_downfield, by = c("game_id", "play_id", "nfl_id", "frame_id_2" = "frame_id")) %>%
  group_by(game_id, play_id, nfl_id, frame_id_2) %>%
  slice(1) %>% 
  ungroup() %>%
  left_join(play_params %>%
              # Typing
              mutate(game_id = as.numeric(game_id)) %>%
              # Already in df
              select(-c(down, yardline_100, ydstogo)),
            by = c("game_id", "play_id"))

# # # Continue here
all_frames_with_pbp_fastr_global <- all_yards_measures

# Now to do a super fun full refactor to accommodate nflfastR
ep_for_receivers_fastr <- function(all_frames_with_pbp_fastr){
  
  # * Goal: Can I make the ep_for_receivers_exact function in
    # * nflfastr?
    # * In record time?
  # * Will require me to vectorize more, previous version was a pmap setup
  # * but made some inefficiencies in the function definition
  
  # * Step 1: Calculate initial EP for the play
    # NOTE: Only need one example per play since each target/frame has same initial within
    # a play
  initial_ep_df <- all_frames_with_pbp_fastr %>%
    group_by(game_id, play_id) %>%
    slice(1) %>%
    ungroup()
  
  initial_ep <- nflfastR::calculate_expected_points(initial_ep_df) %>%
    select(game_id, play_id, ep)
  
  # * Step 2: Build the logic for the 3 main outcomes (assume no time run off currently)
    # * Completion
    # * Estimate field position based on DOT + YAC
    # * Check for down increment/reset, turnovers, and scores
  complete_params <- all_frames_with_pbp_fastr %>%
    mutate(yards_gained = round(yards_downfield + pred_yac), # yards gained on the play
           yac = round(pred_yac),
           los = yardline_100,
           td_flag = (yards_gained >= los),
           new_ydstogo = ydstogo - yards_gained,
           new_down = case_when(new_ydstogo <= 0 ~ 1,
                                down == 4 ~ NA_real_,
                                TRUE ~ down + 1),
           new_ydstogo = ifelse(new_ydstogo <= 0, 10, new_ydstogo),
           # Use pmax to achieve rowwise maxs without performance dropoff
           new_yardline_100 = pmax(yardline_100 - yards_gained, 0),
           safety_flag = (new_yardline_100 >= 100),
           new_yardline_100 = ifelse(new_yardline_100 >= 100, 100, new_yardline_100),
           # pmin same reason
           new_ydstogo = ifelse(new_ydstogo <= 0 | new_yardline_100 < 10,
                                pmin(10, new_yardline_100), new_ydstogo),
           new_ydstogo = ifelse(is.na(new_down), 10, new_ydstogo),
           new_ydstogo = ifelse(new_yardline_100 >= 100, 10, new_ydstogo),
           # Need to make new ep negative on turnovers
           modifier = ifelse(is.na(new_down), -1, 1),
           new_yardline_100 = ifelse(is.na(new_down), 100 - new_yardline_100, new_yardline_100), #turnover
           new_down = ifelse(is.na(new_down) | new_yardline_100 >= 100, 
                             1, # Turnover case
                             new_down),
           new_yardline_100 = as.numeric(ifelse(new_yardline_100 <= 0, 1, new_yardline_100)), #TD corrections, these 2
           new_ydstogo = ifelse(new_ydstogo <= 0, 1, new_ydstogo)) %>% 
    # nflfastR requires specific var names so have to change new_* back to *
    select(-c(yardline_100, ydstogo, down)) %>%
    rename(yardline_100 = new_yardline_100, ydstogo = new_ydstogo, down = new_down) %>%
    ungroup()
  
  # Calculate EP on completion and select relevant joining columns
  complete_ep <- nflfastR::calculate_expected_points(complete_params) %>%
    mutate(adj_comp_ep = ifelse(td_flag, 
                                7,
                                modifier * ep)) %>%
    select(game_id, play_id, frame_id_2, nfl_id, adj_comp_ep)
  
    # * Incompletion
    # * Slightly easier: is there a turnover on downs?
    # * Can even be done using intial_ep_df as its the same regardless of location
  incomplete_params <- initial_ep_df %>%
    mutate(new_down = ifelse(down == 4, NA_real_, down + 1),
           modifier = ifelse(is.na(new_down), -1, 1),
           new_yardline_100 = ifelse(is.na(new_down), 100 - yardline_100, yardline_100),
           new_ydstogo = ifelse(is.na(new_down), pmin(10, new_yardline_100), ydstogo),
           new_down = ifelse(is.na(new_down), 1, new_down)) %>%
    # Need to update here as well
    select(-c(yardline_100, ydstogo, down)) %>%
    rename(yardline_100 = new_yardline_100, ydstogo = new_ydstogo, down = new_down)
  
  # Not unique to player therefore fewer selected columns
  incomplete_ep <- nflfastR::calculate_expected_points(incomplete_params) %>%
    mutate(adj_inc_ep = modifier * ep) %>%
    select(game_id, play_id, adj_inc_ep)
  
    # * Interception
    # * Field position is relevant, assumption of no run-back though
  interception_params <- all_frames_with_pbp_fastr %>%
    mutate(down = 1, # turnover, new set of downs for other team
           line_of_scrimmage = yardline_100,
           yardline_100 = 100 - (line_of_scrimmage - yards_downfield),
           # Safety check
           yardline_100 = if_else(yardline_100 > 100, 80, yardline_100),
           # Goal to go check
           ydstogo = pmin(10, yardline_100),
           # Interception in endzone?
           td_flag = (ydstogo <= 0),
           # Small correction as we do log(ydstogo) (might be nflscrapR relic)
           ydstogo = pmax(ydstogo, 0)) 
  
  # All locations have diff interception values
  interception_ep <- nflfastR::calculate_expected_points(interception_params) %>%
    mutate(adj_int_ep = if_else(td_flag, -7, -1 * ep)) %>%
    select(game_id, play_id, frame_id_2, nfl_id, adj_int_ep)
  
  # Now I just need to bind them all together, calculate EPA, calculate Hypothetical EPA
  all_frames_with_epa <- all_frames_with_pbp_fastr %>%
    left_join(initial_ep, by = c("game_id", "play_id")) %>%
    left_join(complete_ep, by = c("game_id", "play_id", "frame_id_2", "nfl_id")) %>%
    left_join(incomplete_ep, by = c("game_id", "play_id")) %>%
    left_join(interception_ep, by = c("game_id", "play_id", "frame_id_2", "nfl_id")) %>%
    mutate(comp_epa = adj_comp_ep - ep,
           inc_epa = adj_inc_ep - ep,
           int_epa = adj_int_ep - ep,
           hyp_epa = completion_prob_pred * comp_epa +
             incompletion_prob_pred * inc_epa +
             interception_prob_pred * int_epa)
  
  return(all_frames_with_epa)
}


# Actually new thought
# I only use WP as a filter on eligible plays, not as a metric
# As such I can just use what is in nflfastR
# This should be a major time save
relevant_win_probs <- nflfastr_stuff %>%
  dplyr::select(game_id = old_game_id, play_id, wp) %>%
  mutate(game_id = as.numeric(game_id))

# Calculate everything in a vectorized call
all_frames_with_ep_fastr <- all_frames_with_pbp_fastr_global %>%
  ep_for_receivers_fastr() %>%
  left_join(relevant_win_probs, by = c("game_id", "play_id"))

saveRDS(all_frames_with_ep_fastr, 
        glue('{default_path}{time_of_arrival_explicit}/all_frames_with_ep_fastr.rds'))
