# Additional all time points results

# # # # # 
flog.info('Starting additional all time points. This should be incorporated into all_time_points')
epsilon <- 5

yards_downfield <- tibble()
proper_names <- tibble()
for(i in 1:9){
  print(i)
  tic()
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
    filter(!is.na(cleaning), pass_result %in% c('C', 'I', 'IN', 'S', 'R'))
  
  
  tracking <- tracking %>%
    mutate(data = map(data, ball_fix_2),
           fake_pt = map_dbl(data, fake_punt),
           qb_check = map_dbl(data, no_qb)) %>%
    filter(!is.na(fake_pt), !is.na(qb_check)) %>%
    mutate(target = map2_dbl(data, pass_result, ~ if_else(.y %in% c('S', 'R'), qb_tuck(.x),
                                                          intended_receiver(.x)))) %>%
    filter(!is.na(target)) %>%
    left_join(players %>% dplyr::select(nfl_id, display_name), by = c("target" = "nfl_id")) %>%
    mutate(receiver = display_name) %>%
    dplyr::select(-display_name)
    
  
  # Keep track of exempt plays, ones with really weird tracking data
  # First is bad data, second & third is a punter pass (no QB), fourth is data error (QB not tracked)
  exempt_plays <- tibble(game_id = c(2017091700,2017091706, 2017091711, 2017092407, 2017092407, 2017091007),
                         play_id = c(2288, 2126, 3386, 190, 270, 1702))
  
  # Looks like Brady's data might be messed up for 2017092407
  tracking <- tracking %>%
    anti_join(exempt_plays) %>%
    filter(!(game_id == 2017092407))
  
  tracking <- tracking %>%
    mutate(first_elig = map_int(data, ~first_elig_frame(.)),
           last_elig = map2_int(data, pass_result, ~last_elig_frame(.x, .y)) + epsilon,
           pass_result_2 = pass_result) %>%
    select(-game_id, -play_id) %>%
    unnest(data) %>%
    nest(-game_id, -play_id, -frame_id, -first_elig, -last_elig, -pass_result_2) %>%
    filter(frame_id >= first_elig, frame_id <= last_elig)
  
  tracking <- tracking %>%
    mutate(yards_down_est = map(data, ~ all_rec_air_yards_if_caught(., run = TRUE))) %>%
    select(-data) %>%
    unnest(yards_down_est)
  
  yards_downfield <- yards_downfield %>%
    bind_rows(tracking)
}

yards_downfield %>%
 saveRDS('yards_to_be_gained_5add_frames.rds')

# Need to use play results to filter, I forgot I removed this variable
yards_downfield %>%
  filter(pass_result_2 %in% 'IN') %>%
  saveRDS('all_frames_interception_covariates_5add_frames.rds')

rm(yards_downfield)
gc(verbose=FALSE)


proper_names <- tibble()
for(i in 1:9){
  print(i)
  tic()
  lower <- (i-1) * 10 + 1
  upper <- i * 10
  if(i == 9){
    upper <- upper + 1 # have 91 files
  }
  
  tracking <- read_many_tracking_data("/Data/", lower:upper) %>%
    group_by(game_id, play_id) %>%
    join_additional_data("/Data/", players = TRUE) %>%
    ungroup()
  
  proper_names <- proper_names %>%
    bind_rows(tracking %>%
                select(nfl_id, display_name_fix = display_name) %>% unique(.))
}

proper_names_temp <- proper_names %>%
  select(nfl_id, display_name_fix) %>%
  unique(.)

official_names <- read_csv('Data/players.csv') %>%
  select(nfl_id = nflId, FirstName, LastName) %>%
  mutate(display_name = paste0(FirstName, " ", LastName)) %>%
  select(nfl_id, display_name)

proper_names_temp <- proper_names_temp %>%
  left_join(official_names)

saveRDS(proper_names_temp, 'new_player_name_fix.rds')

rm(tracking)
gc(verbose=FALSE)
