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

# Looks good for new data
# Receiver separation from nearest defender
all_separation <- function(one_frame, run=FALSE){
  # Receiver separation
  
  position_group <- c('WR', 'TE', 'RB', 'FB', 'HB')
  if(run){
    position_group <- c('WR', 'TE', 'RB', 'QB', 'FB', 'HB')
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

# Looks good for new data
# Receiver separation from nearest sideline
all_sideline_sep <- function(one_frame, run=FALSE){
  
  position_group <- c('WR', 'TE', 'RB', 'HB', 'FB')
  if(run){
    position_group <- c('WR', 'TE', 'RB', 'QB', 'HB', 'FB')
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

# Unnecessary, return NA just for ease of use
# No frame rush separation
pass_rush_sep <- function(one_frame){
  
  # # Get distance from nearest defender and the qb at time of throw
  # qb_loc <- one_frame %>%
  #   filter(position %in% "QB") %>%
  #   dplyr::select(x, y, frame_id)
  # 
  # def_dist <- one_frame %>%
  #   filter(!(team %in% poss_team), team %in% c("home", "away")) %>%
  #   mutate(dist = sqrt((x - qb_loc$x)^2 + (y - qb_loc$y)^2)) %>%
  #   ungroup() %>%
  #   arrange(dist) %>%
  #   slice(1) %>%
  #   pull(dist)
  
  def_dist <- NA_real_
  
  return(def_dist)
}

# Add with mutate

# Looks good for new approach
# QB speed at frame
avg_qb_speed <- function(one_frame){
  
  # Damn you Sean Payton, we dont need Taysom Hill on the field with Brees
  qb_speed <- one_frame %>% 
    filter(position %in% 'QB') %>%
    slice(1)
  
  if(dim(qb_speed)[1] >= 2){
    print(one_frame)
  }
  if(!("velocity" %in% names(one_frame))){
    return(qb_speed %>% pull(s))
  }
  
  return(qb_speed %>% pull(velocity))
  
}

# Add with mutate

# This should be fine for new data assuming first_elig works
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

# Switch last_elig to be the pass_outcome event frame <- this has been done
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
ownership_at_throw <- function(one_frame, influence,
                               ball_speed = 20, run = FALSE,
                               ball_coords = NA){
  # Ball speed is set at 20 yards per second as an exploratory measure, used as argument to be tailored
  #tictoc::tic()
  # QB position for my calculations
  qb_loc <- one_frame %>%
    filter(position %in% 'QB') %>%
    slice(1)
  
  position_group <- c('WR', 'TE', 'RB', 'HB', 'FB')
  if(run){
    position_group <- c('WR', 'TE', 'RB', 'QB', 'HB', 'FB')
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
  
  if(!is.na(ball_coords)){
    ball_centric_output <- list(sub_fn(one_frame, ball_coords$x, ball_coords$y, influence))
  } else{
    ball_centric_output <- list(NA)
  }
  #tictoc::toc()
  
  #tictoc::tic()
  receiver_ownership_proj <- receiver_loc %>%
    mutate(player_centric = map2(arrival_x, arrival_y, ~sub_fn(one_frame, .x, .y, influence)),
           ball_centric= ball_centric_output) %>%
    dplyr::select(nfl_id, display_name, air_time_ball = time_to_arrival,
                  air_yards_x, air_dist, player_centric, ball_centric) %>%
    unnest(cols = c(player_centric, ball_centric), names_sep = "_")
  #tictoc::toc()
  
  return(receiver_ownership_proj)
}

pass_arrive_location <- function(pass_play){
  ball_coords <- pass_play %>%
    arrange(desc(frame_id)) %>%
    filter(team %in% c("football", "ball")) %>%
    slice(1) %>%
    select(x, y)
  
  return(ball_coords)
}

# So my 2 main functions rely on different levels of nesting
# The simple covariates are done at a play level while the ownership covariates are done framewise
# Try to write a wrapper for the ownership covariates that allows for play level summary

# I think I want to keep frame_inf and ownership_metrics
ownership_metric_wrapper <- function(pass_play, first_elig, last_elig, is_football = FALSE){
  
  pass_play <- pass_play %>%
    filter(dplyr::between(frame_id, first_elig, last_elig)) %>%
    mutate(frame_id_2 = frame_id)
  
  tictoc::tic()
  pass_play <- pass_play %>% # quick fix to deal with nesting, dont want to lose covariate
    nest(-frame_id_2) %>%
    mutate(ball_at_arrival_coords = map(data, pass_arrive_location),
           frame_inf = map(data, ~get_zone_influence(., lazy = TRUE,
                                                     is_football = is_football)))
  tictoc::toc()
  
  tictoc::tic()
  pass_play <- pass_play %>%
    mutate(ownership_metrics = pmap(list(data, frame_inf, ball_at_arrival_coords),
                                    ~ownership_at_throw(..1, ..2, 
                                                        ball_speed = 20, run =FALSE,
                                                        ball_coords = ..3))) %>%
    dplyr::select(-data, -frame_inf)
  tictoc::toc()
  
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
  if(!("frame_id_2" %in% names(complex_cov))){
    print("This play tripped an error in creating complex covariate, need to skip")
    return(NULL)
  }
  
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
#num_cores <- parallel::detectCores() - 2
plan(sequential) # To be parallelized later, this is for debugging

flog.info('Parallel component established. Now to run.', name = 'all_time')

###############################################################
###############################################################

# Not bad run time, now I just need to find a way to iterate over the games, unnest and expand
# Below is the necessary filtering from earlier

# Lets try to incorporate 3 additional frames, i.e epsilon = 5
epsilon <- 0

# Parallelize with future_map
plan(multisession, workers = max(availableCores() - 2, 1))

# Iteration 1 worked, lets try the rest!
  # Saving data the wrong way, these are game files not weeks
  # Update saving structure and fix weird issues with standardize play
for(i in 41:91){
  if(i == 40){
    flog.info("This game is broken, no tracking on certain objects all game. Skip")
    next
  }
  
  file_name <- file_list[i]
  save_file_name <- str_extract(file_name, "[A-z0-9_]*")
  
  flog.info('Starting iteration %s at time %s.', i, format(Sys.time(), '%X'), name = 'all_time')

  tictoc::tic()
  tracking <- read_csv(paste0(default_path, file_name)) %>%
    select_at(select_cols) %>%
    janitor::clean_names() %>%
    left_join(players %>% select(-display_name), by = "nfl_id") %>%
    rename(velocity = s)
  # 4 sec
  
  # Need possession team for current format
  possession <- get_possession_team(tracking)
  
  # Control whether offensive routes are standardized: False for visualization, True for work
  reorient <- FALSE
  
  # Do some basic cleaning to start
  tictoc::tic()
  tracking_clean <- tracking %>%
    left_join(possession) %>%
    nest(-game_id, -play_id) %>%
    inner_join(play_ids) %>%
    mutate(data = pmap(list(data, game_id, play_id), ~quick_nest_fix(..1, ..2, ..3)),
           data = map(data, ~standardize_play(., reorient)),
           cleaning = map_dbl(data, ~handle_no_ball(.))) %>%
    filter(!is.na(cleaning), pass_result %in% c('C', 'I', 'IN'))
  rm(tracking)
  tictoc::toc()
  # 77 sec
  
  # Add additional filtering criteria
  tictoc::tic()
  tracking_additional <- tracking_clean %>%
    mutate(data = map(data, ball_fix_2),
           target = map_dbl(data, intended_receiver, is_football = new_age_tracking_data),
           complete = map_lgl(data, play_success),
           fake_pt = map_dbl(data, fake_punt),
           qb_check = map_dbl(data, no_qb),
           pass_recorded = map_lgl(data, ~pass_start_event(.))) %>%
    left_join(players %>% dplyr::select(nfl_id, display_name), by = c("target" = "nfl_id")) %>%
    filter(!is.na(target), !is.na(fake_pt), !is.na(qb_check), pass_recorded) %>%
    mutate(receiver = display_name) %>%
    dplyr::select(-display_name)
  rm(tracking_clean)
  tictoc::toc()
  
  # Begin debugging here, this is going to be a train wreck
  tictoc::tic()
  parallel_res <- tracking_additional %>%
    mutate(first_elig = map_int(data, ~first_elig_frame(.)),
           last_elig = map_int(data, ~last_elig_frame(.)) + epsilon,
           pocket_dist = pmap(list(data, first_elig, last_elig),
                             ~pocket_fixed(..1, ..2, ..3)))
  rm(tracking_additional)
  tictoc::toc()
  
  # Good through here
  tictoc::tic()
  parallel_res <- parallel_res %>%
    mutate(basic_covariates = future_pmap(list(data, first_elig, last_elig),
                                   ~ simple_covariates(..1, ..2, ..3, run = TRUE)),
           basic_covariates = map2(basic_covariates, pocket_dist,
                                   ~ .x %>% mutate(pocket_dist = list(.y)))) %>%
    dplyr::select(-pocket_dist) # remove from exterior, now mapped it back
  tictoc::toc()
  # 17 seconds for 5 sequential, 11 seconds parallel
  
  # Quick forced tidy up
  plan(sequential)
  gc(verbose = FALSE)
  cl <- future::makeClusterPSOCK(4L, rscript_libs = c(libs, "*"))
  plan(cluster, workers = cl)
  
  # Lets see
  tictoc::tic()
  parallel_res_temp <- parallel_res %>%
           mutate(complex_covariates = future_pmap(list(data, first_elig, last_elig),
                                     ~ ownership_metric_wrapper(..1, ..2, ..3,
                                                                is_football = new_age_tracking_data)))
  tictoc::toc()
  # 88 seconds for 5 sequential, 60 for 2 parallel, 40 for 4 parallel
  
  tictoc::tic()
  parallel_res_temp <- parallel_res_temp %>%
           mutate(covariates = map2(basic_covariates, complex_covariates, ~ joiner_fn(.x, .y))) %>%
    dplyr::select(-data, -basic_covariates, -complex_covariates)
  tictoc::toc()
  
  flog.info('Ended iteration %s at time %s.', i, format(Sys.time(), '%X'), name = 'all_time')
  flog.info('Writing to all_frames_covariates_week%s.rds', i, name = 'all_time')
  
  parallel_res_temp %>%
    unnest(covariates) %>%
    write_rds(glue::glue("{default_path}{time_of_arrival_explicit}/all_frames_covariates_{save_file_name}.rds"))

  rm(parallel_res, parallel_res_temp)
  gc(verbose=FALSE)
}

#4+ hours per iteration

