# Add in QB Run decisions and sacks
# Both of these are evidence of some sort of decision making or lack there of


# First I'll do QB Sacks
# This is just a generic play, like any pass play, but there is no intended receiver
# Because of that my base functions break
# To fix this I need to manipulate the targeted receiver
# What I could do simplistically is use the QB as the targeted receiver
# This might actually fix both problems
# My other work estimates the YAC as well which could be viewed as the rushing yards feasible
# if a pass were to land in the hands of the QB instantly (run)

# Interesting, try to make the QB the intended receiver on the play

# 452 sacks
sacks <- plays %>%
  filter(pass_result %in% 'S')

# 252 runs
qb_runs <- plays %>%
  filter(pass_result %in% 'R')

qb_target_plays <- sacks %>%
  bind_rows(qb_runs) %>%
  select(game_id, play_id, pass_result)

# Get the tracking data for these plays
qb_tracking_plays <- tibble()

for(i in 1:length(file_list)){
  if(i == 40){
    flog.info("This game is broken, no tracking on certain objects all game. Skip")
    next
  }
  
  file_name <- file_list[i]
  save_file_name <- str_extract(file_name, "[A-z0-9_]*")
  
  # Read in
  tracking <- read_csv(paste0(default_path, file_name)) %>%
    select_at(select_cols) %>%
    janitor::clean_names() %>%
    left_join(players %>% select(-display_name), by = "nfl_id") %>%
    rename(velocity = s)
  
  # Need possession team for current format
  possession <- get_possession_team(tracking)
  
  tracking <- tracking %>%
    left_join(possession) %>%
    nest(-game_id, -play_id) %>%
    inner_join(qb_target_plays) %>%
    mutate(data = pmap(list(data, game_id, play_id), ~quick_nest_fix(..1, ..2, ..3)),
           data = map(data, ~standardize_play(., reorient)),
           cleaning = map_dbl(data, ~handle_no_ball(.))) %>%
    filter(!is.na(cleaning)) 
  
  qb_tracking_plays <- qb_tracking_plays %>%
    bind_rows(tracking)
}

# After processing there are 701 plays with Sacks or Runs that should be workable
# Now to modify them such that the intended receiver is the QB
# intended_receiver() function just returns an nfl_id so this will do the same but for the qb
qb_tuck <- function(sack_or_run){
  
  qb_id <- sack_or_run %>%
    filter(position %in% 'QB') %>%
    slice(1) %>%
    pull(nfl_id)
  
  return(qb_id)
}


qb_tracking_plays <- qb_tracking_plays %>%
  anti_join(exempt_plays) %>%
  mutate(data = map(data, ball_fix_2),
         fake_pt = map_dbl(data, fake_punt),
         qb_check = map_dbl(data, no_qb)) %>%
  filter( !is.na(fake_pt), !is.na(qb_check)) %>%
  mutate(target = map_dbl(data, qb_tuck)) %>%
  filter(!is.na(target)) %>%
  left_join(players %>% select(nfl_id, display_name), by = c("target" = "nfl_id")) %>%
  mutate(receiver = display_name) %>%
  select(-display_name)

# There we go, this should handle the setup, now to try and run the code
# Need to generate the covariates for these plays so that I can estimate the other options
# Then I can send it through all frames predictions to predict YAC

# A sack has no YAC and is just down at point of contact
# This doesnt need any modification, it just is a measure of value lost associated with down/yards
# Still needs all of the covariates though to estimate the other options

# A rush has only YAC and it starts when QB rushes: event %in% 'run'
# This means last_elig_frame needs to be calculated differently, like with the pass_end
# event being in 'run', 'tackle', 'first_contact', 'out_of_bounds' but the first frame of these

qb_rush_plays <- qb_tracking_plays %>%
  filter(pass_result %in% 'R') %>%
  filter(game_id != 2017090700 & play_id != 3528) # This run is actually a forward pass, wrong category

# Except for last_elig_frame, I think I can just basically copy all_time_points_decisions.R
# Begin debugging here
last_elig_frame_no_pass <- function(pass_play){
  frame <- pass_play %>%
    group_by(frame_id) %>%
    summarize(n = sum(event %in% c('run', 'tackle', 'first_contact', 'out_of_bounds',
                                   'qb_sack', 'qb_strip_sack'))) %>%
    ungroup() %>%
    filter(n > 0) %>%
    arrange(frame_id) %>%
    slice(1) %>%
    pull(frame_id)
  
  return(frame)
}

ownership_metric_wrapper_2 <- function(pass_play, first_elig, last_elig){
  
  pass_play <- pass_play %>%
    filter(dplyr::between(frame_id, first_elig, last_elig)) %>%
    mutate(frame_id_2 = frame_id) %>% # quick fix to deal with nesting, dont want to lose covariate
    nest(-frame_id_2) %>%
    mutate(frame_inf = map(data, ~get_zone_influence(., lazy = TRUE, run = TRUE)),
           ownership_metrics = map2(data, frame_inf, ~ownership_at_throw(.x, .y,
                                                                         ball_speed = 20, run = TRUE))) %>%
    dplyr::select(-data, -frame_inf)
  
  return(pass_play)
}
# Try without parallel because something funky is happening with multidplyr
# Too slow, do with parallel but use future
plan(multisession, workers = 4)

parallel_res_2 <- qb_rush_plays %>%
  filter(game_id != 2017090700 & play_id != 3528) %>%
  mutate(first_elig = map_dbl(data, ~first_elig_frame(.)),
         last_elig = map_dbl(data, ~last_elig_frame_no_pass(.)) + epsilon,
         pocket_dist = pmap(list(data, first_elig, last_elig),
                            ~pocket_fixed(..1, ..2, ..3)),
         basic_covariates = future_pmap(list(data, first_elig, last_elig),
                                        ~ simple_covariates(..1, ..2, ..3, run = TRUE)),
         basic_covariates = map2(basic_covariates, pocket_dist,
                                 ~ .x %>% mutate(pocket_dist = list(.y)))) %>%
  dplyr::select(-pocket_dist) # remove from exterior, now mapped it back

safe_ownership_wrapper_2 <- safely(ownership_metric_wrapper_2)

parallel_res_2 <- parallel_res_2 %>%
  anti_join(exempt_plays) %>%
  filter(!(game_id == 2017092407)) %>%
  mutate(complex_covariates = pmap(list(data, first_elig, last_elig),
                                          ~ safe_ownership_wrapper_2(..1, ..2, ..3)))

parallel_res_3 <- parallel_res_2 %>%
  # Need to make some modifications to deal with purrr::safely
  unnest(complex_covariates) %>%
  mutate(len = map_int(complex_covariates, length)) %>%
  filter(len > 0) %>%
  mutate(covariates = map2(basic_covariates, complex_covariates, ~ joiner_fn(.x, .y))) %>%
  dplyr::select(-data, -basic_covariates, -complex_covariates) 

# View(parallel_res_3 %>% dplyr::select(-covariates))
# View(parallel_res_3 %>% slice(1) %>% unnest(covariates))

# saveRDS(parallel_res_3, 'Data/release/qb_rush_val_covariates.rds')
# parallel_res_3 will store the covariates for the other options on the play
# Doesnt specifically have the yac predictions for the QB or the necessary covariates


# To set up the stuff for a sack, its the same as above but no YAC estimates
# Just create the covariates
# Actually these can be done together I think
# Technically could do together but something is off
# I think the Sack data has a few cleaning things to be done
sack_exempt_plays <- tibble(game_id = c(2017100104
                                        ,2017092411
                                        ,2017091710
                                        ,2017092410
                                        ,2017091011),
                            play_id = c(223, 707, 4239, 1868, 637))

qb_sack_plays <- qb_tracking_plays %>%
  filter(pass_result %in% 'S') %>%
  anti_join(exempt_plays) %>% 
  anti_join(sack_exempt_plays) %>%
  filter(!(game_id == 2017092407)) %>%
  filter(!(game_id == 2017100103 & play_id == 3258))

parallel_sack <- qb_sack_plays %>%
  mutate(first_elig = map_dbl(data, ~first_elig_frame(.)),
         last_elig = map_dbl(data, ~last_elig_frame_no_pass(.)) + epsilon,
         pocket_dist = pmap(list(data, first_elig, last_elig),
                            ~pocket_fixed(..1, ..2, ..3)),
         basic_covariates = future_pmap(list(data, first_elig, last_elig),
                                        ~ simple_covariates(..1, ..2, ..3, run = TRUE)),
         basic_covariates = map2(basic_covariates, pocket_dist,
                                 ~ .x %>% mutate(pocket_dist = list(.y)))) %>%
  dplyr::select(-pocket_dist) # remove from exterior, now mapped it back

parallel_sack <- parallel_sack %>%
  anti_join(exempt_plays) %>%
  filter(!(game_id == 2017092407)) %>%
  mutate(complex_covariates = future_pmap(list(data, first_elig, last_elig),
                                          ~ ownership_metric_wrapper_2(..1, ..2, ..3)))

parallel_sack_3 <- parallel_sack %>%
  mutate(covariates = map2(basic_covariates, complex_covariates, ~ joiner_fn(.x, .y))) %>%
  dplyr::select(-data, -basic_covariates, -complex_covariates) 

# saveRDS(parallel_sack_3, 'Data/release/qb_sack_val_covariates.rds')

# Now I need to integrate this into general results
# I have the sack and run values for the plays in which the QB was sacked or ran
# But I also need a way to estimate these values on general plays
# Again, QB sack is just the play ends wherever the QB is on a frame, down is lost, possible
# turnover update. Very similar to incomplete pass code but further back. 
# A QB run is then just the YAC for the QB if targetted
# Getting that value will require some changes to the YAC model, namely including QB rushes
# To incorporate QB runs, I need to estimate yards to be gained at each frame for QBs who run

# I am currently running the recalculation of all_times_points_decisions.R for 
# all eligible receivers and QB
# Once that runs I can send the data back over to here and stack it with the existing data
# and estimate the yards gained by rushing

# I dont think I need to run this through parallel_observed.R 
# What I do think I need is to make sure to identify the chosen option for each sack_and_run play
# This is either the sack on the last frame or the run on the last frame
# I do this in qb_eval_step.R at line 53 for standard plays
# Verify if this will work

# parallel_sack_3 %>%
#   slice(1) %>% unnest(covariates) %>% View()
parallel_res_3 %>%
  bind_rows(parallel_sack_3) %>%
  saveRDS(glue("{default_path}{time_of_arrival_explicit}/sack_and_rush_plays_frames.rds"))


# Some names in the above are off so I need to replace them
correct_player_names <- readRDS('new_player_name_fix.rds') %>%
  select(target = nfl_id, receiver_fix = display_name_fix)


# This is for sacks
holder <- parallel_sack_3 %>%
  unnest(covariates) %>%
  left_join(correct_player_names) %>%
  mutate(receiver = receiver_fix) %>%
  select(-receiver_fix)  %>%
  group_by(game_id, play_id) %>%
  arrange(desc(frame_id_2)) %>%
  mutate(is_chosen = if_else(frame_id_2 == first(frame_id_2) & (display_name == receiver),
                             TRUE, FALSE))

# This is for rushes
holder_rush <- parallel_res_3 %>%
  unnest(covariates) %>%
  left_join(correct_player_names) %>%
  mutate(receiver = receiver_fix) %>%
  select(-receiver_fix)  %>%
  group_by(game_id, play_id) %>%
  arrange(desc(frame_id_2)) %>%
  mutate(is_chosen = if_else(frame_id_2 == first(frame_id_2) & (display_name == receiver),
                             TRUE, FALSE))

# Stack them and save
holder %>%
  bind_rows(holder_rush) %>%
  saveRDS(glue("{default_path}{time_of_arrival_explicit}/sack_and_rush_plays_frames.rds"))
