# Workshopping some stuff for zone influence

# This is what gets passed into the data argument
test_play <- parallel_res %>%
  slice(1) %>%
  select(data, first_elig, last_elig) %>%
  unnest(data)

pass_play <- test_play

# Test standardize_play with this
standardize_play_test <- tracking %>%
  left_join(possession) %>%
  nest(-game_id, -play_id) %>%
  inner_join(play_ids) %>%
  mutate(data = pmap(list(data, game_id, play_id), ~quick_nest_fix(..1, ..2, ..3))) %>%
  slice(1) %>% 
  select(data) %>%
  unnest(data)

pass_play <- standardize_play_test         

# reorient is only useful for route classification
  # Here it is problematic as it will confuse completion probability
standardize_play <- function(pass_play, reorient = FALSE){
  
  data <- pass_play
  
  params <- pass_play %>%
    ungroup() %>%
    slice(1)
  
  snap_check <- data %>%
    select(event) %>%
    summarize(n_snap = sum(event %in% c("ball_snap", "snap_direct"), na.rm = TRUE)) %>%
    pull(n_snap)
  
  print(glue::glue("Game ID: {params$game_id}, Play ID: {params$play_id}"))
  
  if(snap_check == 0){
    print("No snap detected, modified first frame approach")
    # Fix situations where no ball snap recorded
    line_of_scrimmage <-
      data %>% 
      filter(frame_id == first(frame_id), team %in% c("home", "away")) %>% 
      group_by(game_id, play_id, team) %>% 
      summarise(right_scrim = max(x, na.rm=TRUE), left_scrim = min(x, na.rm=TRUE),
                poss_team = first(poss_team), .groups = "drop") %>%
      filter(team == poss_team) %>%
      select(game_id, play_id, right_scrim, left_scrim)
    
    play_direction <- data %>%
      filter(frame_id == first(frame_id)) %>%
      group_by(game_id, play_id, team) %>%
      summarise(mean_team = mean(x, na.rm=TRUE), .groups = "drop") %>%
      filter(team != "ball") %>%
      arrange(desc(mean_team)) %>%
      # Larger mean implies they are facing left at time of snap
      mutate(temp_dir = c("direction_left", "direction_right")) %>%
      # Column makes these rows falsely identifiable
      select(-mean_team) %>%
      pivot_wider(names_from = temp_dir,
                  values_from = team)
    
  } else{
    line_of_scrimmage <-
      data %>% 
      filter(event %in% c("ball_snap", "snap_direct"), team %in% c("home", "away")) %>% 
      group_by(game_id, play_id, team) %>% 
      summarise(right_scrim = max(x, na.rm=TRUE), left_scrim = min(x, na.rm=TRUE),
                poss_team = first(poss_team), .groups = "drop") %>%
      filter(team == poss_team) %>%
      select(game_id, play_id, right_scrim, left_scrim)
    
    play_direction <- data %>%
      filter(event %in% c("ball_snap", "snap_direct")) %>%
      group_by(game_id, play_id, team) %>%
      summarise(mean_team = mean(x, na.rm=TRUE), .groups = "drop") %>%
      filter(team != "ball") %>%
      arrange(desc(mean_team)) %>%
      # Larger mean implies they are facing left at time of snap
      mutate(temp_dir = c("direction_left", "direction_right")) %>%
      # Column makes these rows falsely identifiable
      select(-mean_team) %>%
      pivot_wider(names_from = temp_dir,
                  values_from = team)

  }
  
  # Continue here in refactor, too tired today :(
  possession <- plays %>%
    dplyr::select(game_id, play_id, possession_team) %>%
    left_join(games, by = "game_id") %>%
    mutate(possession_t = if_else(possession_team == home_team_abbr, "home", "away")) %>%
    dplyr::select(game_id, play_id, possession_t)
  
  data <- data %>%
    left_join(play_direction, by=c("game_id", "play_id")) %>%
    left_join(line_of_scrimmage, by = c("game_id", "play_id")) %>%
    left_join(possession, by = c("game_id", "play_id")) %>%
    mutate(line_of_scrimmage = if_else(possession_t == direction_left, left_scrim, right_scrim),
           play_direction = if_else(poss_team == direction_left, "left", "right")) %>%
    dplyr::select(-right_scrim, -left_scrim, -possession_t)%>%
    group_by(nfl_id) %>% 
    mutate(velocity_2 = sqrt((dplyr::lead(x, 2, default = 0) - dplyr::lag(x, 2, default = 0))^2 +
                               (dplyr::lead(y, 2, default = 0) - dplyr::lag(y, 2, default = 0)) ^ 2),
           angle = (base::atan2(dplyr::lead(y, 2, default = 0) - dplyr::lag(y, 2, default = 0), 
                                dplyr::lead(x, 2, default = 0) - dplyr::lag(x, 2, default = 0)) * 180/pi) %% 360,
           dir = angle) %>%
    ungroup() %>%
    drop_na(x)
  
  if(reorient){
    data <- data %>% 
      mutate(# flip the field to try and get all routes in the same direction
        play_dir = play_direction == "left",
        x = if_else(play_dir, 120 - x, x),
        y = if_else(play_dir, 160/3 - y, y),
        x = if_else(play_dir, x - (120 - line_of_scrimmage), x - line_of_scrimmage)) # really just x - line_of_scrimmage
  }
  
  return(data)
}

# Either way I should refactor for just thesis stuff, there is too much going on
# Actually should also redo standardized
get_zone_influence <- function(data, standardized = FALSE,
                               lazy = FALSE, run = FALSE, is_football = FALSE){
  
  # Change ball name dependent on data set
  ball_name <- "ball"
  if(is_football){
    ball_name <- "football"
  }
  
  if(standardized){
    # Has been standardized, rename 2 variables
    myPlay <- data %>%
      mutate(velocity = velocity_2, dir = angle)
  } else{
    myPlay <- data 
  }
  
  if(start_or_end %in% 'start'){
    # Want to filter down to the start of a pass
    pass_cond <- c('pass_forward', 'pass_shovel')
  } else{
    pass_cond <- 
      c("pass_arrived",
        "pass_outcome_caught",
        "pass_outcome_incomplete",
        # "pass_tipped",
        "touchdown",
        "pass_outcome_interception",
        "pass_outcome_touchdown"
      )
  }
  
  if(run){
    pass_cond <- c('run', 'tackle', 'first_contact', 'out_of_bounds', 
                   'qb_sack', 'qb_strip_sack')
  }
  # lazy is for my group mutate step, allows me to artificially make everything the throw frame
  if(lazy){
    throw_frame <- myPlay %>%
      ungroup() %>%
      slice(1) %>%
      pull(frame_id) %>%
      as.numeric()
    
  } else{
    # get the frame of throw
    throw_frame <- myPlay  %>% 
      filter(event %in% pass_cond) %>% 
      ungroup() %>%
      slice(1) %>%
      dplyr::select(frame_id) %>%
      summarize(frame = first(frame_id)) %>%
      pull(frame) %>%
      as.numeric()
  }
  
  
  # Input speeds are in yards per second, convert to metres per second for formula
  throw_time_positions <- myPlay %>%
    filter(frame_id %in% throw_frame) 
  
  # Dont need qb location, need ball location instead
  ball_loc <- 
    throw_time_positions %>%
    filter(team %in% ball_name) %>%
    select(x, y)
  
  qb_loc <- 
    myPlay %>%
    filter(frame_id %in% throw_frame) %>%
    filter(position %in% 'QB') %>%
    slice(1)
  
  # Calculate mu_i, Sratio, distance to ball, and non-zero elements of S matrix
  throw_time_positions <- 
    throw_time_positions %>%
    mutate(mu_i_x = x + velocity * cos(dir * pi / 180) * 0.5,
           mu_i_y = y + velocity * sin(dir * pi / 180) * 0.5,
           # Max observed speed was 22.11 mph, say 24 mph. Denom is in m/s
           speed_rat = velocity ^ 2 / (10.729^2),
           speed_x = velocity * cos(dir * pi / 180),
           speed_y = velocity * sin(dir * pi / 180),
           dis_to_ball = sqrt((x - ball_loc$x) ^ 2 +
                                (y - ball_loc$y) ^ 2)) %>% 
    ungroup()
  
  # Get positions at time of handoff, really just the same as the individual frame we have
  throw_time_positions <- 
    throw_time_positions %>% 
    mutate(team2 = ifelse((team == poss_team), "Off","Def"),
           R = purrr::map(dir, makeMatrix), #dir -> Dir
           # Use get_R_i_t to get the value in df
           R_i_t = get_R_i_t(dis_to_ball),
           S_i_t = map2(speed_rat, R_i_t, ~ matrix(
             c((.y + .y * .x) / 2,
               0,
               0,
               (.y - .y * .x) / 2),
             byrow = T, nrow = 2)),
           mu_i = map2(mu_i_x, mu_i_y, ~ matrix(c(.x, .y), nrow = 2)),
           cov_struc = map2(R, S_i_t, ~ .x %*% .y %*% .y %*% solve(.x))) 
  
  # Could probably refactor this to grab from nflfastr instead of
  # the more costly group_by %>% summarize paradigm
  # Also could be a little error prone, revisit, not currently biggest value add
    # Can be better handled in standardize_play
  avg_position <- myPlay %>%
    filter(team %in% c("away", "home")) %>%
    mutate(is_poss_team = ifelse(poss_team == team, 1, 0)) %>%
    group_by(is_poss_team) %>%
    summarize(avg_pos = mean(x, na.rm = TRUE), .groups = "drop") %>%
    arrange(avg_pos) %>%
    summarize(play_direction = ifelse(first(is_poss_team) == 1, 'right', 'left'),
              .groups = "drop") %>%
    pull(play_direction)
  
  # Restrict field for efficiency
  if(avg_position %in% 'left'){
    # QB is furthest right that we care about
    x_bounds <-
      throw_time_positions %>% 
      transmute(min_x = 0,
                max_x = qb_loc$x) %>% 
      slice(1)
  } else{
    # QB is furthest left that we care about
    x_bounds <-
      throw_time_positions %>% 
      transmute(min_x = qb_loc$x,
                max_x = 120) %>% 
      slice(1)
  }
  
  # We can now calculate the density of a player over the whole field
  data_grid <- expand.grid(s_1 = seq(x_bounds$min_x, x_bounds$max_x, by = 1),
                           s_2 = seq(-10, 53, by = 1)) # previously res, now 1
  
  # Now we will try to do the same thing over all the players in one play
  # This needs to be normalized by player position
  density_calc <- 
    throw_time_positions %>% 
    # Calculate density for each player frame: Instant
    mutate(current_density = pmap_dbl(list(x, y, mu_i, cov_struc),
                                      ~ dmvnorm(c(..1, ..2), mean = ..3, sigma = ..4))) %>%
    # Standardize at each player frame: Also instant
    mutate(density_val = pmap(list(mu_i, cov_struc, current_density),
                              ~ dmvnorm(data_grid, mean = ..1, sigma = ..2) / ..3))
  
  # Nested dataframe doesnt need to select play_id and game_id
  if("play_id" %in% colnames(myPlay)){
    density_field <-
      density_calc %>% 
      dplyr::select(density_val, nfl_id, play_id, game_id, team, team2, jersey_number) %>% 
      mutate(density_val = map(density_val, ~ bind_cols(as_tibble(.x), as_tibble(data_grid)))) %>% 
      unnest(cols = c(density_val))
  } else{
    density_field <-
      density_calc %>% 
      dplyr::select(density_val, nfl_id, team, team2, jersey_number) %>% 
      mutate(density_val = map(density_val, ~ bind_cols(as_tibble(.x), as_tibble(data_grid)))) %>% 
      unnest(cols = c(density_val))
  }
  
  
  # Next is the player influence
  influence <- density_field %>% 
    group_by(nfl_id) %>% 
    dplyr::arrange(nfl_id, desc(value)) %>%
    mutate(influence_unscaled = value / first(value)) %>%
    ungroup() %>% 
    filter(value > 1.30e-10) %>%
    group_by(s_1, s_2, team) %>%
    summarize(team_total = sum(value), .groups = "drop") %>%
    group_by(s_1, s_2) %>%
    arrange(s_1, s_2, team) %>%
    summarize(away_inf = 1 / (1 + exp(-(first(team_total) - last(team_total))/ sum(team_total) )), 
              home_inf = 1 / (1 + exp(-(last(team_total) - first(team_total))/ sum(team_total) )),
              .groups = "drop") 
  
  return(influence)
}

# End original function

# Additional benchmarking for how its used in all_time_points_decisions
pass_play <- test_play %>%
  filter(dplyr::between(frame_id, first_elig, last_elig)) %>%
  mutate(frame_id_2 = frame_id)

# Oh man this is reallllyyyy slow
pass_play_2 <- pass_play %>% # quick fix to deal with nesting, dont want to lose covariate
  nest(-frame_id_2) %>%
  mutate(ball_at_arrival_coords = map(data, pass_arrive_location),
         frame_inf = map(data, ~get_zone_influence(., lazy = TRUE,
                                                   is_football = new_age_tracking_data)))

# One play averages a 20 second runtime
  # This is no good, must improve
  # Approx 0.6 seconds for ball_at_arrival, rest of time in frame_inf
microbenchmark::microbenchmark(pass_play %>% # quick fix to deal with nesting, dont want to lose covariate
                                 nest(-frame_id_2) %>%
                                 mutate(ball_at_arrival_coords = map(data, pass_arrive_location),
                                        frame_inf = map(data, ~get_zone_influence(., lazy = TRUE,
                                                                                  is_football = new_age_tracking_data))),
                               times = 20)

# Naturally the problem is how many calculations have to be done
# Lets try and refactor and see if I can break it down

data <- pass_play %>%
  filter(frame_id_2 == max(frame_id_2))

# If I am already nesting by frame_id_2, why do I even need to filter at lower lever?
# I must be over filtering
  # Or maybe not because frame_id_2 = frame_id

# Also instead of nesting I could just fully vectorize, use more group_by -> summarize internal