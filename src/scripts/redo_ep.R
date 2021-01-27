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
                season, home_team, posteam, roof, posteam_timeouts_remaining,
                # This is a relic to compare against nflscrapR model
                goal_to_go)
# Don't need to load sysdata in nflfastR, can just use their helper function
# To use, must have:
  #' season, home_team
  #' posteam, roof (coded as 'outdoors', 'dome', or {'open' / 'closed' / NA} (retractable))}
  #' half_seconds_remaining, yardline_100, down, ydstogo,
  #' posteam_timeouts_remaining


# 
play_params <- ep_requirements %>%
  filter(game_id ==2017090700, play_id == 68)
# 
initial_ep <- nflscrapR::calculate_expected_points(play_params, "half_seconds_remaining",
                                        "yardline_100", "down", "ydstogo", "goal_to_go")

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
  ungroup()

# # # Continue here

# Now to do a super fun full refactor to accommodate nflfastR
ep_for_receivers_fastr <- function(all_frames_with_pbp_fastr){
  
  # * Goal: Can I make the ep_for_receivers_exact function in
    # * nflfastr?
    # * In record time?
  # * Will require me to vectorize more, previous version was a pmap setup
  # * but made some inefficiencies in the function definition
  
  
}

# I can use most of these parameters with the actual play to define starting EP
# Now to adjust the function that I use to estimate EP currently
# This is an exact copy from thesis_helpers.R
ep_for_receivers_exact <- function(all_frames_with_pbp, game_id_par, play_id_par,
                                   yardline_100, ydstogo, down){
  
  print(glue::glue("Game id: {game_id_par}, Play id: {play_id_par}"))
  
  # Relies on global dependency
  play_params <- ep_requirements %>%
    filter(game_id == game_id_par, play_id == play_id_par)  %>%
    mutate(half_seconds_remaining = if_else(is.na(half_seconds_remaining), 500, half_seconds_remaining))
  
  if(game_id_par == 2017092100 & play_id_par == 3173){
    # Problematic case where snap isnt recorded but is not dealt with for some reason
    return(list(NA))
  }
  
  if(dim(play_params)[1] == 0){
    View(c(game_id_par, play_id_par))
  }
  initial_ep <- calculate_expected_points(play_params, "half_seconds_remaining",
                                          "yardline_100", "down", "ydstogo", "goal_to_go")$ep
  # Store nfl_scrapr_playdata somewhere globally
  # Currently stored as nfl_2017_small
  line_of_scrimmage <- round(yardline_100)
  
  # Now calculate the new situation for each play, assuming line_of_scrimmage is in tracking coord system
  change_df <- data.frame(los = line_of_scrimmage,
                          ydstogo = ydstogo,
                          down = down,
                          yardlines = ceiling(all_frames_with_pbp$yards_downfield),
                          yac = all_frames_with_pbp$yac,
                          frame_id_2 = all_frames_with_pbp$frame_id_2,
                          display_name = as.character(all_frames_with_pbp$display_name),
                          receiver = as.character(all_frames_with_pbp$receiver))
  
  if(any(is.na(change_df$yardlines))){
    return(list(NA))
  }
  
  complete_params <- change_df %>%
    rowwise() %>%
    mutate(yards_gained = round(yardlines + yac), # yards gained on the play
           yac = yac,
           td_flag = (yards_gained >= los),
           new_ydstogo = ydstogo - yards_gained,
           new_down = case_when(new_ydstogo <= 0 ~ 1,
                                down == 4 ~ NA_real_,
                                TRUE ~ down + 1),
           new_ydstogo = ifelse(new_ydstogo <= 0, 10, new_ydstogo),
           new_yardline_100 = max(yardline_100 - yards_gained, 0),
           safety_flag = (new_yardline_100 >= 100),
           new_yardline_100 = ifelse(new_yardline_100 >= 100, 100, new_yardline_100),
           new_ydstogo = ifelse(new_ydstogo <= 0 | new_yardline_100 < 10,
                                min(10, new_yardline_100), new_ydstogo),
           new_ydstogo = ifelse(is.na(new_down), 10, new_ydstogo),
           new_ydstogo = ifelse(new_yardline_100 >= 100, 10, new_ydstogo),
           modifier = ifelse(is.na(new_down), -1, 1), # Need to make new ep negative on turnovers
           new_yardline_100 = ifelse(is.na(new_down), 100 - new_yardline_100, new_yardline_100), #turnover
           new_down = ifelse(is.na(new_down) | new_yardline_100 >= 100, 
                             1, # Turnover case
                             new_down),
           new_yardline_100 = as.numeric(ifelse(new_yardline_100 <= 0, 1, new_yardline_100)), #TD corrections, these 2
           new_ydstogo = ifelse(new_ydstogo <= 0, 1, new_ydstogo)) %>% # Standard
    ungroup() %>% # add on the play covariates
    mutate(half_seconds_remaining = play_params$half_seconds_remaining,
           half_seconds_remaining = if_else(is.na(half_seconds_remaining), 500, half_seconds_remaining),
           goal_to_go = play_params$goal_to_go)
  # Some weird error popped up, seeing if as.numeric is sufficient to fix
  #Error: variable 'yrdline100' was fitted with type "numeric" but type "logical" was supplied
  
  complete_ep <- calculate_expected_points(complete_params, "half_seconds_remaining",
                                           "new_yardline_100", "new_down", "new_ydstogo",
                                           "goal_to_go") %>%
    mutate(adj_comp_ep = ifelse(td_flag, 
                                7,
                                modifier * ep)) %>%# To handle turnovers
    dplyr::select(-tidyselect::contains("_prob"))
  
  
  # What happens if the pass goes incomplete, there is some penalty to associated with this
  failure_params <- data.frame(yardline_100 = line_of_scrimmage,
                               ydstogo = ydstogo,
                               down = down,
                               display_name = as.character(all_frames_with_pbp$display_name))%>%
    mutate(new_down = ifelse(down == 4, NA_real_, down + 1),
           modifier = ifelse(is.na(new_down), -1, 1),
           new_yardline_100 = ifelse(is.na(new_down), 100 - yardline_100, yardline_100),
           new_ydstogo = ifelse(is.na(new_down), min(10, new_yardline_100), ydstogo),
           new_down = ifelse(is.na(new_down), 1, new_down)) %>% # add on the play covariates
    mutate(half_seconds_remaining = max(play_params$half_seconds_remaining - 7, 1),
           half_seconds_remaining = if_else(is.na(half_seconds_remaining), 500, half_seconds_remaining),
           goal_to_go = play_params$goal_to_go)
  
  incomplete_ep <- calculate_expected_points(failure_params, "half_seconds_remaining",
                                             "yardline_100", "new_down", "new_ydstogo",
                                             "goal_to_go") %>%
    mutate(adj_inc_ep = modifier * ep) %>%
    dplyr::select(adj_inc_ep)
  
  
  # Revisit, this seems a little wonky as it sometimes increases when we lose a down
  # Perhaps this is because NFL teams throw more on 2nd down and this gets captured by EP model
  
  # What happens if the pass gets intercepted?
  interception_params <- data.frame(yardlines = ceiling(all_frames_with_pbp$yards_downfield)) %>%
    rowwise() %>%
    mutate(down = 1, # turnover, new set of downs for other team
           yardline_100 = 100 - (line_of_scrimmage - yardlines),
           yardline_100 = if_else(yardline_100 > 100, 80, yardline_100), # Safety check
           ydstogo = min(10, yardline_100), # add on the play covariates
           td_flag = (ydstogo <= 0),
           ydstogo = max(ydstogo, 1)) %>% # Small correction as we do log(ydstogo)
    mutate(half_seconds_remaining = play_params$half_seconds_remaining,
           half_seconds_remaining = if_else(is.na(half_seconds_remaining), 500, half_seconds_remaining),
           goal_to_go = as.numeric(yardline_100 < 10)) # turn over happened within own 10 yardline
  
  interception_ep <- calculate_expected_points(interception_params, "half_seconds_remaining",
                                               "yardline_100", "down", "ydstogo", "goal_to_go") %>%
    mutate(adj_int_ep = if_else(td_flag, -7, -1 * ep)) %>%
    dplyr::select(adj_int_ep)
  
  # Now to get ep for each 
  change_df <- complete_ep %>%
    bind_cols(incomplete_ep) %>%
    bind_cols(interception_ep) %>%
    mutate(initial_ep = initial_ep,
           complete_epa = adj_comp_ep - initial_ep,
           incomplete_epa = adj_inc_ep - initial_ep,
           interception_epa = adj_int_ep - initial_ep)
  
  return(change_df)
}



# Now I need to find a way to incorporate WP
# This is a little trickier because scoring plays now need the score to update
# From there I need to calculate WP on each play and the difference (WPA)
# Still plenty of helper functions available on nflscrapR, leverage these

# Use the base nflscrapr data to grab the necessary covariates for the start of each play
# Need to iterate by GameId


# Okay, this works for getting the basic setup
# Now I just need to use this information to establish baselines and calculate new values
# The baseline part is easy
# Challenge is manipulating the appropriate columns correctly
# Lets start with just a single play, the one I have worked with the most

# What does the WP model rely on?
# Well it looks like both EP and WP rely on score differential in some way
# Need to find out how they relate and fix that
# Nevermind, the EP model building process depends on score differential for weights
# The prediction just depends on the covariates I have used

# WP still depends on the following variables, some of which need some work to achieve
# Expected score differential = ExpPts + ScoreDiff
# Number of seconds remaining in game
# Expected score time ratio
# Current half of the game (1st, 2nd, or overtime)
# Number of seconds remaining in half
# Indicator for whether or not time remaining in half is under two minutes
# Time outs remaining for offensive (possession) team
# Time outs remaining for defensive team
load(file = '~/Github/thesis/Data/sysdata.rda')
wp_function <- function(all_frames_exp_values){
  
  game_ids <- all_frames_exp_values %>% pull(game_id) %>% unique()
  
  wp_requirements <- tibble()
  for(i in 1:length(game_ids)){
    temp_2 <- nfl_2017 %>% filter(game_id == game_ids[i])
    wp_requirements <- wp_requirements %>%
      bind_rows(temp_2)
  }
  
  filter_criteria <- all_frames_exp_values %>%
    select(game_id, play_id) %>% 
    group_by(game_id, play_id) %>% 
    slice(1) #%>%
  #ungroup() %>%
  #mutate_all(as.character)
  
  # Actually need to filter for plays I have, not on the PassAttempt criteria
  all_with_win_prob <- wp_requirements %>%
    #filter(PassAttempt >0 ) %>%
    inner_join(filter_criteria, by = c('game_id',  'play_id')) %>%
    dplyr::select(GameID = game_id, play_id, qtr, down, yrdline100 = yardline_100,
                  TimeSecs = game_seconds_remaining, ScoreDiff = score_differential,
                  TimeUnder = half_seconds_remaining, 
                  posteam_timeouts_pre = posteam_timeouts_remaining,
                  HomeTimeouts_Remaining_Pre = home_timeouts_remaining,
                  AwayTimeouts_Remaining_Pre = away_timeouts_remaining,
                  posteam, HomeTeam = home_team, AwayTeam = away_team,
                  ExpPts = ep) %>%
    mutate(TimeUnder = round(TimeUnder / 60, 0)) %>% 
    filter(qtr < 5) %>% # Filter OT plays first to get this thing working
    mutate(Half_Ind = as.factor(ifelse(qtr %in% c(1,2), "Half1", "Half2")),
           down = factor(down),
           Season = as.numeric(substr(as.character(GameID),1,4)),
           Month = as.numeric(substr(as.character(GameID),5,6)),
           score_time_ratio = ScoreDiff / (TimeSecs + 1),
           TimeSecs_Remaining = ifelse(qtr %in% c(1,2),
                                       TimeSecs - 1800,
                                       ifelse(qtr == 5 &
                                                (Season == 2017 &
                                                   Month > 4),
                                              TimeSecs + 600,
                                              ifelse(qtr == 5 &
                                                       (Season < 2017 |
                                                          (Season == 2017 &
                                                             Month <= 4)),
                                                     TimeSecs + 900,
                                                     TimeSecs))),
           Time_Yard_Ratio = (1 + TimeSecs_Remaining) / (1 + yrdline100),
           oppteam_timeouts_pre = ifelse(posteam == HomeTeam,
                                         AwayTimeouts_Remaining_Pre,
                                         HomeTimeouts_Remaining_Pre),
           Under_TwoMinute_Warning = ifelse(TimeSecs_Remaining < 120,1,0),
           posteam_timeouts_pre = ifelse(posteam_timeouts_pre < 0,
                                         0,posteam_timeouts_pre),
           oppteam_timeouts_pre = ifelse(oppteam_timeouts_pre < 0,
                                         0,oppteam_timeouts_pre),
           TimeSecs_Adj = ifelse(qtr == 5 &
                                   (Season == 2017 &
                                      Month > 4),
                                 TimeSecs + 600,
                                 ifelse(qtr == 5 &
                                          (Season < 2017 |
                                             (Season == 2017 &
                                                Month <= 4)),
                                        TimeSecs + 900,
                                        TimeSecs)),
           ExpScoreDiff = ExpPts + ScoreDiff,
           ExpScoreDiff_Time_Ratio = ExpScoreDiff / (TimeSecs_Adj + 1))
  
  OffWinProb <- as.numeric(mgcv::predict.bam(wp_model,newdata=all_with_win_prob,
                                             type = "response"))
  DefWinProb <- 1 - OffWinProb
  # Establish start of play win probabilities
  all_with_win_prob <- all_with_win_prob %>%
    mutate(off_wp = OffWinProb,
           def_wp = DefWinProb)
  
  # Grab just the necessary columns to update wp
  min_from_ep <- all_frames_exp_values %>% 
    ungroup() %>%
    dplyr::select(game_id, play_id, frame_id_2, display_name, epa_of_pass)
  
  # Combine with the identifying columns for the play
  joining_set <- all_with_win_prob %>%
    dplyr::select(GameID, play_id, qtr, down, yrdline100, Half_Ind, Season, Month,
                  score_time_ratio, TimeSecs_Remaining, Time_Yard_Ratio,
                  TimeSecs, TimeSecs_Adj, ScoreDiff,
                  TimeUnder, posteam_timeouts_pre, oppteam_timeouts_pre,
                  HomeTimeouts_Remaining_Pre, AwayTimeouts_Remaining_Pre, Under_TwoMinute_Warning,
                  posteam, HomeTeam, AwayTeam,
                  ExpScoreDiff, ExpScoreDiff_Time_Ratio, off_wp, def_wp) %>%
    mutate(GameID = as.numeric(GameID), play_id = as.numeric(play_id))
  
  simple_together <- min_from_ep %>%
    left_join(joining_set, by = c('game_id' = 'GameID', 'play_id')) %>%
    mutate(ExpScoreDiff = (ExpScoreDiff + epa_of_pass),
           ExpScoreDiff_Time_Ratio = (ExpScoreDiff + 1) / (TimeSecs_Adj + 1))
  
  # New fit and update
  off_win_prob <- as.numeric(mgcv::predict.bam(wp_model,newdata=simple_together,
                                               type = "response"))
  def_win_prob <- 1 - off_win_prob
  
  simple_together <- simple_together %>%
    mutate(new_off_wp = off_win_prob,
           new_def_wp = def_win_prob,
           wpa = new_off_wp - off_wp)
  
  # 58 plays have some sort of data error preventing WP calculation
  # Most seem to be from 2 games 2017100108/9 
  # Otherwise this works reasonably well
  
  # Now just grab the necessary components and write to a data source to be used later
  results <- simple_together %>%
    dplyr::select(game_id, play_id, frame_id_2, display_name, off_wp, new_off_wp, wpa)
  
  return(results)
}



ep_requirements <- nfl_data %>%
  dplyr::select(game_id, play_id, half_seconds_remaining,
                yardline_100, down, ydstogo, goal_to_go)
# 
play_params <- ep_requirements %>%
  filter(game_id ==2017090700, play_id == 68)
# 
initial_ep <- calculate_expected_points(play_params, "half_seconds_remaining",
                                       "yardline_100", "down", "ydstogo", "goal_to_go")

# 
# temp <- all_frames_exp_yards %>%
#   ungroup() %>%
#   mutate(yac = yac_preds) %>%
#   nest(-game_id, -play_id, -yardline_100, -ydstogo, -down) %>%
#   filter(game_id == 2017090700, play_id == 94) %>%
#   mutate(ep_rec = pmap(list(data, game_id, play_id, yardline_100, ydstogo, down), 
#                        ~ep_for_receivers_exact(..1, ..2, ..3, ..4, ..5, ..6)))


# I might be able to just estimate the ScoreDiff at the next step 
# one_wp_play_small <- one_wp_play %>%
#   dplyr::select(GameID, play_id, qtr, down, yrdline100, TimeSecs, ScoreDiff,
#                 TimeUnder, posteam_timeouts_pre,
#                 HomeTimeouts_Remaining_Pre, AwayTimeouts_Remaining_Pre,
#                 posteam, HomeTeam, AwayTeam,
#                 ExpPts) %>%
#   filter(qtr < 5) %>% # Filter OT plays first to get this thing working
#   mutate(Half_Ind = as.factor(ifelse(qtr %in% c(1,2), "Half1", "Half2")),
#          down = factor(down),
#          Season = as.numeric(substr(as.character(GameID),1,4)),
#          Month = as.numeric(substr(as.character(GameID),5,6)),
#          score_time_ratio = ScoreDiff / (TimeSecs + 1),
#          TimeSecs_Remaining = ifelse(qtr %in% c(1,2),
#                                      TimeSecs - 1800,
#                                      ifelse(qtr == 5 & 
#                                               (Season == 2017 & 
#                                                  Month > 4),
#                                             TimeSecs + 600,
#                                             ifelse(qtr == 5 &
#                                                      (Season < 2017 |
#                                                         (Season == 2017 &
#                                                            Month <= 4)),
#                                                    TimeSecs + 900,
#                                                    TimeSecs))),
#          OffWinProb = NA,
#          Time_Yard_Ratio = (1 + TimeSecs_Remaining) / (1 + yrdline100),
#          oppteam_timeouts_pre = ifelse(posteam == HomeTeam,
#                                        AwayTimeouts_Remaining_Pre,
#                                        HomeTimeouts_Remaining_Pre),
#          Under_TwoMinute_Warning = ifelse(TimeSecs_Remaining < 120,1,0),
#          posteam_timeouts_pre = ifelse(posteam_timeouts_pre < 0,
#                                        0,posteam_timeouts_pre),
#          oppteam_timeouts_pre = ifelse(oppteam_timeouts_pre < 0,
#                                        0,oppteam_timeouts_pre),
#          TimeSecs_Adj = ifelse(qtr == 5 & 
#                                  (Season == 2017 & 
#                                     Month > 4),
#                                TimeSecs + 600,
#                                ifelse(qtr == 5 &
#                                         (Season < 2017 |
#                                            (Season == 2017 &
#                                               Month <= 4)),
#                                       TimeSecs + 900,
#                                       TimeSecs)),
#          ExpScoreDiff = ExpPts + ScoreDiff,
#          ExpScoreDiff_Time_Ratio = ExpScoreDiff / (TimeSecs_Adj + 1))
# 
# 
# 
# OffWinProb <- as.numeric(mgcv::predict.bam(wp_model,newdata=one_wp_play_small,
#                                            type = "response"))
# DefWinProb <- 1 - OffWinProb

# # Now do more generically with all the plays observed values
# tic()

# 
# OffWinProb <- as.numeric(mgcv::predict.bam(wp_model,newdata=all_with_win_prob,
#                                            type = "response"))
# DefWinProb <- 1 - OffWinProb
# 
# all_with_win_prob <- all_with_win_prob %>%
#   mutate(off_wp = OffWinProb,
#          def_wp = DefWinProb)
# toc()
# 
# # So I can successfully calculate WP with no NA values for the passing plays in my data set
# # Now I just need to do it for each artificial conclusion to a play
# bare_minimum <- test_play %>% 
#   dplyr::select(game_id, play_id, epa_of_pass) # Just need epa because I can add that to ExpScoreDiff to update
# 
# joining_set <- all_with_win_prob %>%
#   dplyr::select(GameID, play_id, qtr, down, yrdline100, Half_Ind, Season, Month,
#                 score_time_ratio, TimeSecs_Remaining, Time_Yard_Ratio,
#                 TimeSecs, TimeSecs_Adj, ScoreDiff,
#                 TimeUnder, posteam_timeouts_pre, oppteam_timeouts_pre,
#                 HomeTimeouts_Remaining_Pre, AwayTimeouts_Remaining_Pre, Under_TwoMinute_Warning,
#                 posteam, HomeTeam, AwayTeam,
#                 ExpScoreDiff, ExpScoreDiff_Time_Ratio, off_wp, def_wp) %>%
#   mutate(GameID = as.numeric(GameID), play_id = as.numeric(play_id))
# 
# # Now to create an updated set of covariates, which really is just updating the ExpScoreDiff
# # and time possibly (or leave it as is)
# simple_together <- bare_minimum %>%
#   left_join(joining_set, by = c('game_id' = 'GameID', 'play_id')) %>%
#   mutate(ExpScoreDiff = (ExpScoreDiff + epa_of_pass),
#          ExpScoreDiff_Time_Ratio = (ExpScoreDiff + 1) / (TimeSecs_Adj + 1))
# 
# off_win_prob <- as.numeric(mgcv::predict.bam(wp_model,newdata=simple_together,
#                                              type = "response"))
# def_win_prob <- 1 - off_win_prob
# 
# simple_together <- simple_together %>%
#   mutate(new_off_wp = off_win_prob,
#          new_def_wp = def_win_prob,
#          wpa = new_off_wp - off_wp)
# 
# # So this surprisingly worked, time to generalize further hopefully