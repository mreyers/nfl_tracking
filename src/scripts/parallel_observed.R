# Parallel stuff
flog.appender(appender.file('logs/parallel_observed.log'), 'par_obs')
flog.info('Start of parallel_observed.R. Computing relevant covariates for all observed passes in 
          the input tracking data.', name = 'par_obs')

# Load the games, players, and list of plays in the data set
games <- read_csv("Data/games.csv", col_types = cols()) %>%
  rename_all(to_snake_case)
players <- read_csv("Data/players.csv", col_types = cols()) %>%
  rename_all(to_snake_case) %>%
  unite("display_name", c(first_name, last_name), sep = " ")
plays <- read_csv("Data/plays.csv", col_types = cols()) %>%
  rename_all(to_snake_case)

# Gather positions that I need for analysis, eligible receivers and defensive players
player_pos_id_key <- players %>%
  dplyr::select(nfl_id, position_abbr)

route_runners_pos_id_key <- player_pos_id_key %>%
  filter(position_abbr %in% c("RB", "FB", "WR", "TE", "OLB", "SS", "ILB", "DE", "CB", "NT",
                              "MLB", "FS", "DT", "LB", "DB"))

# For play standardization
reorient <- FALSE

# Also need the play_ids which are the plays that had passes
play_ids <- plays %>%
  filter(!is.na(pass_result)) %>% 
  select(game_id, play_id, pass_result)

# Generic event labels
pass_air_end <- 
  c(# "pass_arrived",
    "pass_outcome_caught",
    "pass_outcome_incomplete",
    # "pass_tipped",
    "touchdown",
    "pass_outcome_interception",
    "pass_outcome_touchdown"
  )

pass_air_start <-
  c("pass_forward",
    "pass_shovel"
  )

# Plays and games that had specific, unresolvable issues (generally broken tracking data for player/ball)
exempt_plays <- tibble(game_id = c(2017091700,2017091706, 2017091711, 2017092407, 2017092407, 2017091007), 
                       play_id = c(2288, 2126, 3386, 190, 270, 1702))
flog.info('Setup complete for general purpose variables. Defining cluster now.', name = 'par_obs')

num_cores <- parallel::detectCores()
plan(multisession)

flog.info('Cluster prepared, running loop.', name = 'par_obs')

#  # # # # #  The loop # # #  # # # # # # 

# Be sure to run all the functions from the QB_evaluation file first
n_games <- dim(games)[1]
iter <- ceiling(n_games / 10)

for(i in 1:iter){
  flog.info('Loop %s started at %s.', i, format(Sys.time(), '%X'), name = 'par_obs') # Fix
  lower <- (i-1) * 10 + 1
  upper <- i * 10
  
  tracking <- read_many_tracking_data("/Data/", lower:upper) %>%
    group_by(game_id, play_id) %>%
    join_additional_data("/Data/", players = TRUE) %>%
    ungroup()
  
  # Need possession team for current format
  possession <- get_possession_team(tracking)
  
  tracking <- tracking %>%
    left_join(possession) %>%
    nest(-game_id, -play_id) %>%
    inner_join(play_ids) %>%
    mutate(data = future_pmap(list(data, game_id, play_id), ~quick_nest_fix(..1, ..2, ..3)),
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
    left_join(players %>% select(nfl_id, display_name), by = c("target" = "nfl_id")) %>%
    filter(!is.na(target), !is.na(fake_pt), !is.na(qb_check), pass_recorded) %>%
    mutate(receiver = display_name) %>%
    select(-display_name)
  
  tracking <- tracking %>%
    anti_join(exempt_plays) %>%
    filter(!(game_id == 2017092407))
  
  # All the parallel stuff
  parallel_res <- tracking %>%
    ungroup() %>% 
    mutate(air_dist = map2_dbl(data, target, ~air_distance(.x, .y)),
           separation = map2(data, target, ~separation(.x, .y)),
           sideline_sep = map2_dbl(data, target, ~sideline_sep(.x, .y)),
           passrush_sep = map(data, ~pass_rush_sep(.))) 
  
  parallel_res <- parallel_res %>%
    mutate(qb_vel = map_dbl(data, ~qb_speed(.)),
           first_elig = map_dbl(data, ~first_elig_frame(.)),
           last_elig = map_dbl(data, ~last_elig_frame(.))) %>%
    mutate(time_to_throw = map_dbl(data, ~release_time(.)),
           dist_from_pocket = map_dbl(data, ~pocket(.)),
           qb_disp_name = map_chr(data, ~qb_name(.)))
  
  parallel_res <- parallel_res %>%
    mutate(inf_at_pass = future_map(data, ~ add_influence(.)))
  
  parallel_res <- parallel_res %>%
    mutate(ball_speed_arrival = map_dbl(data, ~ball_speed_at_arrival(.)),
           air_time_ball = map_dbl(data, ~air_time(.)),
           air_yards_x = map_dbl(data, ~air_yards(.))) %>%
    dplyr::select(-data) %>%
    unnest(separation) %>%
    unnest(passrush_sep) %>%
    unnest(inf_at_pass)
  
  flog.info('Loop %s ended at %s.', i, format(Sys.time(), '%X'), name = 'par_obs')
  flog.info('Writing file observed_covariates_%s_%s.rds', lower, upper, name= 'par_obs')
  
  parallel_res %>% write_rds(paste0('observed_covariates',lower, "_", upper, '.rds'))
  
  rm(parallel_res)
  gc(verbose = FALSE)
}

flog.info('Cluster prepared, running loop.', name = 'par_obs')
