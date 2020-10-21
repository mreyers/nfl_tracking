# Expected Max Yards Calculation
# Doing in a separate file from QB Evaluation as that file is already getting terribly cumbersome
flog.appender(appender.file('logs/all_time_points_decisions.log'), 'all_time')
flog.info('Start of all_time_points_decisions.R. Computing relevant covariates for all pass frames in 
          the first 6 weeks of the 2017-2018 NFL season.', name = 'all_time')

# Now I will need to calculate the zone of influence at each frame in the range of first to last elig frames
# At each frame of influence, I will need to find the covariates associated with each receiver
# Reminder of the covariates:

# a) covariates specifically relating to measurements achievable with values from throw_frame
# 'rec_separation', 'sideline_sep', 'no_frame_rush_sep',
# 'avg_speed', 'time_to_throw', 'dist_from_pocket', 

# b) covariates that are measured at throw_frame but require info wrt ball arrival frame (ball x,y, time)
# 'n_cells_at_throw', 'own_intensity_at_throw', 'own_avg_intensity_at_throw',
# 'air_yards_y', 'air_time_ball', 'air_dist'

# I'll do a function for each to avoid the infamous omnibus function
# Each function will need subfunctions I imagine
# Work on structure later

# Receiver separation from nearest defender
all_separation <- function(one_frame, run=FALSE){
  # Receiver separation
  
  position_group <- c('WR', 'TE', 'RB')
  if(run){
    position_group <- c('WR', 'TE', 'RB', 'QB')
  }
  
  receivers_df <- one_frame %>%
    filter(position %in% position_group) %>%
    dplyr::select(nfl_id, display_name, x, y)
  
  def_df <- one_frame %>%
    filter(team != poss_team)
  
  dist_vec <- c()
  for(i in 1:dim(receivers_df)[1]){
    close <- def_df %>% 
      mutate(rec_x = receivers_df$x[i],
             rec_y = receivers_df$y[i],
             dist_rec = sqrt((x - rec_x)^2 + (y - rec_y)^2)) %>%
      arrange(dist_rec) %>%
      slice(1) %>%
      pull(dist_rec)
    
    dist_vec <- c(dist_vec, close)
  }
  
  receivers_df <- receivers_df %>%
    mutate(rec_separation = dist_vec) # Should be equal lengths
  
  return(receivers_df)
}

# Add with left_join

# Receiver separation from nearest sideline
all_sideline_sep <- function(one_frame, run=FALSE){
  
  position_group <- c('WR', 'TE', 'RB')
  if(run){
    position_group <- c('WR', 'TE', 'RB', 'QB')
  }
  
  sideline_df <- one_frame %>%
    filter(position %in% position_group) %>%
    dplyr::select(nfl_id, display_name, x, y) %>%
    rowwise() %>%
    mutate(dist_top = y,
           dist_bot = 53 - y,
           sideline_sep = min(dist_top, dist_bot)) %>%
    dplyr::select(nfl_id, display_name, sideline_sep)
  
  return(sideline_df)
}

# Add with left_join

# No frame rush separation
pass_rush_sep <- function(one_frame){
  
  # Get distance from nearest defender and the qb at time of throw
  qb_loc <- one_frame %>%
    filter(position %in% "QB") %>%
    dplyr::select(x, y, frame_id)
  
  def_dist <- one_frame %>%
    filter(!(team %in% poss_team), team %in% c("home", "away")) %>%
    mutate(dist = sqrt((x - qb_loc$x)^2 + (y - qb_loc$y)^2)) %>%
    ungroup() %>%
    arrange(dist) %>%
    slice(1) %>%
    pull(dist)
  
  return(def_dist)
}

# Add with mutate

# QB speed at frame
avg_qb_speed <- function(one_frame){
  qb_speed <- one_frame %>% 
    filter(position %in% 'QB') %>%
    pull(velocity)
  
  return(qb_speed)
}

# Add with mutate

# Time since ball snap
time_calc <- function(one_frame, first_elig_frame_res){
  time_from_snap <- one_frame %>%
    slice(1) %>%
    mutate(time_to_throw = (frame_id - first_elig_frame_res) / 10) %>%
    pull(time_to_throw)
  
  return(time_from_snap)
}

# Add with mutate

# Distance from the pocket (TO BE REPLACED BY POCKET_FIXED)
# Replaced by pocket() which calls pocket_fixed(), defined in thesis_helpers

# So that is all the simple covariates, time to make a small omnibus function that adds them to all frames
simple_covariates <- function(pass_play, first_elig, last_elig, run = FALSE){
  # Lets see how far I have come: no for loops
  
  valid_pass_part <- pass_play %>% 
    filter(dplyr::between(frame_id, first_elig, last_elig)) %>%
    mutate(frame_id_2 = frame_id) %>% # quick fix to deal with nesting, dont want to lose covariate
    ungroup() %>%
    nest(-frame_id_2) %>%
    mutate(rec_sep = map(data, ~ all_separation(., run = run)), # This just gets covariates for QB
           sideline_sep = map(data, ~all_sideline_sep(., run = run)),
           rush_sep = map_dbl(data, ~pass_rush_sep(.)),
           qb_speed = map_dbl(data, ~avg_qb_speed(.)),
           time_throw = map2_dbl(data, first_elig, ~time_calc(.x, .y))) %>% # pocket() defined in thesis_helpers
    dplyr::select(-data)
  
  return(valid_pass_part)
}

# Excellent, now to do the ownership based metrics or that need a ball position
# b) covariates that are measured at throw_frame but require info wrt ball arrival frame (ball x,y, time)
# 'n_cells_at_throw', 'own_intensity_at_throw', 'own_avg_intensity_at_throw',
# 'air_yards_y', 'air_time_ball', 'air_dist'

# Current covariates are calculated receiver by receiver, results being stored in a nested tibble
# Continue this approach

# Okay good, now the actual functions
ownership_at_throw <- function(one_frame, influence, ball_speed = 20, run = FALSE){
  # Ball speed is set at 20 yards per second as an exploratory measure, used as argument to be tailored
  
  # QB position for my calculations
  qb_loc <- one_frame %>%
    filter(position %in% 'QB') %>%
    slice(1)
  
  position_group <- c('WR', 'TE', 'RB')
  if(run){
    position_group <- c('WR', 'TE', 'RB', 'QB')
  }
  # Receiver locations and anticipated locations
  receiver_loc <- one_frame %>%
    filter(position %in% position_group) %>%
    mutate(dist_to_qb = sqrt((x - qb_loc$x) ^ 2 + (y - qb_loc$y)^2), 
           speed_diff = ball_speed - velocity,
           time_to_arrival = dist_to_qb / speed_diff,
           speed_x = velocity * cos(dir * pi / 180),
           arrival_x = time_to_arrival * speed_x + x,
           arrival_x = case_when(arrival_x < 0 ~ 0,
                                 arrival_x > 120 ~ 120,
                                 TRUE ~ arrival_x),
           air_yards_x = abs(arrival_x - qb_loc$x),
           speed_y = velocity * sin(dir * pi / 180),
           arrival_y = time_to_arrival * speed_y + y,
           arrival_y = case_when(arrival_y < 0 ~ 0,
                                 arrival_y > 53 ~ 53,
                                 TRUE ~ arrival_y),
           air_dist = sqrt((arrival_x - qb_loc$x) ^ 2 + (arrival_y - qb_loc$y) ^ 2))
  
  sub_fn <- function(one_frame, proj_x, proj_y, influence){
    
    influence <- influence %>%
      mutate(x = round(s_1),
             y = round(s_2))
    
    if(one_frame$poss_team[1] %in% 'home'){
      influence <- influence %>% mutate(off_inf = home_inf)
    } else{
      influence <- influence %>% mutate(off_inf = away_inf)
    }
    
    pass_time_results <- influence %>%
      ungroup() %>%
      mutate(dist_from_ball = sqrt((x - proj_x)^2 + (y - proj_y)^2)) %>%
      filter(dist_from_ball <= 5) %>%
      summarize(n_close = n(),
                n_cells_at_throw = sum(off_inf > 0.5),
                own_intensity_at_throw = sum(off_inf),
                own_avg_intensity_at_throw = if_else(is.na(mean(off_inf)), 0, mean(off_inf)))
    
    return(pass_time_results)
  }
  
  receiver_ownership_proj <- receiver_loc %>%
    mutate(metrics = map2(arrival_x, arrival_y, ~sub_fn(one_frame, .x, .y, influence))) %>%
    dplyr::select(nfl_id, display_name, air_time_ball = time_to_arrival, air_yards_x, air_dist, metrics) %>%
    unnest(cols = c(metrics))
  
  return(receiver_ownership_proj)
}


# So my 2 main functions rely on different levels of nesting
# The simple covariates are done at a play level while the ownership covariates are done framewise
# Try to write a wrapper for the ownership covariates that allows for play level summary

ownership_metric_wrapper <- function(pass_play, first_elig, last_elig){
  
  pass_play <- pass_play %>%
    filter(dplyr::between(frame_id, first_elig, last_elig)) %>%
    mutate(frame_id_2 = frame_id) %>% # quick fix to deal with nesting, dont want to lose covariate
    nest(-frame_id_2) %>%
    mutate(frame_inf = map(data, ~get_zone_influence(., lazy = TRUE)),
           ownership_metrics = map2(data, frame_inf, ~ownership_at_throw(.x, .y, # run to get QB covariates
                                                                         ball_speed = 20, run =TRUE))) %>%
    dplyr::select(-data, -frame_inf)
  
  return(pass_play)
}

# That looks like it will work, now to do this on a game level
# first_attempt <- tracking %>%
#   mutate(first_elig = map_int(data, ~first_elig_frame(.)),
#          last_elig = map_int(data, ~last_elig_frame(.)),
#          basic_covariates = pmap(list(data, first_elig, last_elig),
#                                         ~ simple_covariates(..1, ..2, ..3)),
#          complex_covariates = pmap(list(data, first_elig, last_elig),
#                                    ~ ownership_metric_wrapper(..1, ..2, ..3)))

# View(head(first_attempt))
# one_play <- first_attempt %>%
#   slice(1)

# one_play
# one_play$basic_covariates
# one_play$complex_covariates 
# 
# one_play %>% dplyr::select(complex_covariates) %>% unnest() %>% dplyr::select(-frame_inf) %>% slice(1) %>% unnest()
# one_play %>% dplyr::select(basic_covariates) %>% unnest() %>% slice(1)

joiner_fn <- function(basic_cov, complex_cov){
  rec_sep <- basic_cov %>% # Fix joiner function for the new pocket_dist function location
    dplyr::select(frame_id_2, no_frame_rush_sep = rush_sep, qb_vel = qb_speed,
           time_to_throw = time_throw, pocket_dist, rec_separation = rec_sep) %>%
    unnest(rec_separation)
  
  sideline_sep <- basic_cov %>%
    dplyr::select(frame_id_2, sideline_sep) %>%
    unnest(sideline_sep)
  
  owner_met <- complex_cov %>%
    dplyr::select(frame_id_2, ownership_metrics) %>%
    unnest(ownership_metrics)
  
  all_together <- rec_sep %>%
    left_join(sideline_sep, by = c('frame_id_2', 'nfl_id', 'display_name')) %>%
    left_join(owner_met, by = c('frame_id_2', 'nfl_id', 'display_name'))
  
  return(all_together)
}

# second_attempt <- first_attempt %>%
#   dplyr::select(-data) %>%
#   mutate(covariates = map2(basic_covariates, complex_covariates, ~ joiner_fn(.x, .y)))
  
# This structure seems to work but I obviously need to parallelize this
# Even if I manually have to do this game by game, parallel will save me time

###########################################################
###########################################################
num_cores <- parallel::detectCores() - 2
cluster <- new_cluster(num_cores)

cluster %>%
  cluster_library('tidyverse') %>%
  cluster_library('mvtnorm') %>%
  cluster_library('scales') %>%
  cluster_copy('first_elig_frame') %>%
  cluster_copy('last_elig_frame') %>%
  cluster_copy('simple_covariates') %>%
  cluster_copy('ownership_metric_wrapper') %>%
  cluster_copy('joiner_fn') %>% 
  cluster_copy('all_separation') %>%
  cluster_copy('all_sideline_sep') %>%
  cluster_copy('pass_rush_sep') %>%
  cluster_copy('avg_qb_speed') %>%
  cluster_copy('time_calc') %>%
  cluster_copy('pocket') %>%
  cluster_copy('ownership_at_throw') %>%
  cluster_copy('get_zone_influence') %>%
  cluster_copy('pass_air_start') %>%
  cluster_copy('pass_air_end') %>% 
  cluster_copy('pocket_fixed')

flog.info('Parallel component established. Now to run.', name = 'all_time')

###############################################################
###############################################################

# Not bad run time, now I just need to find a way to iterate over the games, unnest and expand
# Below is the necessary filtering from earlier

# Lets try to incorporate 3 additional frames, i.e epsilon = 5
epsilon <- 5
cluster %>% cluster_copy('epsilon')

for(i in 1:9){
  flog.info('Started iteration %s at time %s.', i, format(Sys.time(), '%X'), name = 'all_time')
  lower <- (i-1) * 10 + 1
  upper <- i * 10
  if(i == 9){
    upper <- upper + 1 # have 91 files
  }
  
  tracking <- read_many_tracking_data("/Data/", lower:upper) %>% 
    group_by(game_id, play_id) %>%
    join_additional_data("/Data/", players = TRUE) %>%
    ungroup()
  
  # Need possession team for current format
  possession <- get_possession_team(tracking)
  
  # Control whether offensive routes are standardized: False for visualization, True for work
  reorient <- FALSE
  
  # 6960 plays before cleaning
  # 6887 plays after cleaning, lost 73 plays (not bad)
  tracking <- tracking %>%
    left_join(possession) %>%
    nest(-game_id, -play_id) %>%
    inner_join(play_ids) %>%
    mutate(data = pmap(list(data, game_id, play_id), ~quick_nest_fix(..1, ..2, ..3)),
           data = map(data, ~standardize_play(., reorient)),
           cleaning = map_dbl(data, ~handle_no_ball(.))) %>%
    filter(!is.na(cleaning), pass_result %in% c('C', 'I', 'IN'))
  
  
  tracking <- tracking %>%
    mutate(data = map(data, ball_fix_2),
           target = map_dbl(data, intended_receiver),
           complete = map_lgl(data, play_success),
           fake_pt = map_dbl(data, fake_punt),
           qb_check = map_dbl(data, no_qb),
           pass_recorded = map_lgl(data, ~pass_start_event(.))) %>%
    left_join(players %>% dplyr::select(nfl_id, display_name), by = c("target" = "nfl_id")) %>%
    filter(!is.na(target), !is.na(fake_pt), !is.na(qb_check), pass_recorded) %>%
    mutate(receiver = display_name) %>%
    dplyr::select(-display_name)

  
  # Looks like Brady's data might be messed up for 2017092407
  tracking <- tracking %>%
    anti_join(exempt_plays) %>%
    filter(!(game_id == 2017092407))
  
  # Begin debugging here
  parallel_res <- tracking %>%
    partition(cluster) %>%
    mutate(first_elig = map_int(data, ~first_elig_frame(.)),
           last_elig = map_int(data, ~last_elig_frame(.)) + epsilon,
           pocket_dist = pmap(list(data, first_elig, last_elig),
                             ~pocket_fixed(..1, ..2, ..3)),
           basic_covariates = pmap(list(data, first_elig, last_elig),
                                   ~ simple_covariates(..1, ..2, ..3, run = TRUE)),
           basic_covariates = map2(basic_covariates, pocket_dist,
                                   ~ .x %>% mutate(pocket_dist = list(.y)))) %>%
    dplyr::select(-pocket_dist) # remove from exterior, now mapped it back
 
  parallel_res <- parallel_res %>%
           mutate(complex_covariates = pmap(list(data, first_elig, last_elig),
                                     ~ ownership_metric_wrapper(..1, ..2, ..3)))
  
  parallel_res <- parallel_res %>%
           mutate(covariates = map2(basic_covariates, complex_covariates, ~ joiner_fn(.x, .y))) %>%
    dplyr::select(-data, -basic_covariates, -complex_covariates) %>%
    collect()
  
  flog.info('Ended iteration %s at time %s.', i, format(Sys.time(), '%X'), name = 'all_time')
  flog.info('Writing to all_track_res_%s_%s.rds', lower, upper, name = 'all_time')
  
  parallel_res %>%
    unnest(covariates) %>%
    write_rds(paste0('all_track_res_5add_frames', lower, "_", upper, '.rds'))

  rm(parallel_res)
  gc(verbose=FALSE)
}



