# Generate the necessary covariates to calculate yards after catch
library(sp)
library(rgdal)
library(rgeos)

downfield_x_calc <- function(receiver_data){
  
  sof <- receiver_data %>%
    pull(sect_of_field)
  
  if(sof %in% 'left'){
    receiver_data <- receiver_data %>%
      mutate(x1 = tan(20 * pi/180) * y,
             x2 = tan(20 * pi/180) * (16.66 - y))
    
  } else if(sof %in% 'middle'){
    receiver_data <- receiver_data %>%
      mutate(x1 = tan(20 * pi/180) * (y - 16.67) ,
             x2 = tan(20 * pi/180) * (37.66 - y))
    
  } else if(sof %in% 'right'){
    receiver_data <- receiver_data %>%
      mutate(x1 = tan(20 * pi/180) * (y - 37.67),
             x2 = tan(20 * pi/180) * (53 - y))
  } 
  
  return(receiver_data)
}

end_of_base <- function(receiver_data){
  
  play_dir <- receiver_data %>%
    pull(play_direction)
  
  x_max <- receiver_data %>%
    mutate(x_max = max(x1, x2)) %>%
    pull(x_max)
  
  if(play_dir %in% 'left'){
    # y decreases as ball is moved downfield
    receiver_data <- receiver_data %>%
      mutate(start_x1 = x - x1,
             start_x2 = x - x2,
             end_x = x - x_max - 10)
  } else{
    # y increases as ball is moved downfield
    receiver_data <- receiver_data %>%
      mutate(start_x1 = x + x1,
             start_x2 = x + x2,
             end_x = x + x_max + 10)
  }
}

sideline_vel <- function(receiver_data){
  # Want velocity with respect to sidelines, may be meaningful wrt yards after catch
  play_dir <- receiver_data %>%
    pull(play_direction)
  
  if(play_dir %in% 'left'){
    # 180 is direction to endzone
    receiver_data <- receiver_data %>%
      mutate(dir1 = 270 - dir,
             dir2 = dir - 90,
             vel_sideline = if_else(dir1 < 90, velocity * cos(dir1 * pi/180), velocity * cos(dir2 * pi/180)))
  } else{
    # 0/360 is direction to endzone
    receiver_data <- receiver_data %>%
      mutate(dir1 = 360 - dir,
             dir2 = dir - 0,
             vel_sideline = if_else(dir1 < 90, velocity * cos(dir1 * pi/180), velocity * cos(dir2 * pi/180)))
  }
  
  return(receiver_data)
}

spatial_region <- function(receiver_data){
  # Given the calculated points for polygon corners, define spatial polygon
  sof <- receiver_data %>%
    pull(sect_of_field)
  
  initial_coords <- receiver_data %>%
    select(x, y)
  
  if(sof %in% 'left'){
    x_coord <- c(initial_coords$x, receiver_data$start_x1, receiver_data$end_x, receiver_data$end_x,
                 receiver_data$start_x2, initial_coords$x)
    y_coord <- c(initial_coords$y, 0, 0, 16.66, 16.66, initial_coords$y)
    
  } else if(sof %in% 'middle'){
    x_coord <- c(initial_coords$x, receiver_data$start_x1, receiver_data$end_x, receiver_data$end_x,
                 receiver_data$start_x2, initial_coords$x)
    y_coord <- c(initial_coords$y, 16.67, 16.67, 37.66, 37.66, initial_coords$y)
    
  } else{
    x_coord <- c(initial_coords$x, receiver_data$start_x1, receiver_data$end_x, receiver_data$end_x,
                 receiver_data$start_x2, initial_coords$x)
    y_coord <- c(initial_coords$y, 37.67, 37.67, 53, 53, initial_coords$y)
    
  }
  
  df_p <- cbind(x_coord, y_coord)
  p <- Polygon(df_p)
  ps <- Polygons(list(p), 1)
  sps <- SpatialPolygons(list(ps))
  
  receiver_data <- receiver_data %>%
    mutate(spatial_region = list(sps))
  
  return(receiver_data)
}

container <- function(throw_frame_data, receiver_data){
  receiver_box <- receiver_data$spatial_region
  
  throw_frame_data <- throw_frame_data %>%
    mutate(spatial_region = receiver_box,
           spatial_point = map2(x, y, ~SpatialPoints(data.frame(x = .x, y =.y))),
           is_contained = map2_lgl(spatial_region, spatial_point, ~gContains(.x, .y)))
  
  return(throw_frame_data)
}

contained_counts <- function(container_results, receiver_id){
  relevant_players <- container_results %>%
    filter(is_contained)
  
  receiver_location <- container_results %>%
    filter(nfl_id == receiver_id) %>%
    select(x,y)
  
  if(nrow(receiver_location) == 0){
    return(list(NA))
  }
  
  n_off <- nrow(relevant_players %>%
                  filter(poss_team == team))
  n_def <- nrow(relevant_players %>%
                  filter(!(poss_team == team)))
  close_def <- data.frame(dist = NA_real_, dist_x = NA_real_)
  
  if(n_def >0){
    close_def <- relevant_players %>%
      filter(!(poss_team == team)) %>%
      mutate(dist = sqrt((x - receiver_location$x)^2 + (y - receiver_location$y)^2),
             dist_x = abs(x - receiver_location$x)) %>%
      arrange(dist) %>%
      slice(1) %>%
      select(dist, dist_x)
  }
  
  n_closer_off <- 0
  if(n_off > 0){
    n_closer_off <- relevant_players %>%
      filter(poss_team == team) %>%
      mutate(dist = sqrt((x - receiver_location$x)^2 + (y - receiver_location$y)^2),
             is_closer = case_when(!is.na(close_def$dist) ~ (dist < close_def$dist),
                                   TRUE ~ TRUE)) %>%# If no defenders, offensive player is indeed closest
      summarize(n = sum(is_closer)) %>%
      pull(n)
    
  }
  return(tibble(n_off = n_off, n_def = n_def, dist_def = close_def$dist, dist_def_x = close_def$dist_x,
                n_closer_off = n_closer_off))
}

yac_improvement_wrapper_general <- function(receiving_play, receiver_id){
  
  if(is.na(receiver_id)){
    # Not a receiver, no need to calculate
    return(list(NA))
  }
  
  # For some of the functions that rely on receiver being defined
  receiver <- receiver_id
  
  # Isolate the receiver information
  the_receiver <- receiving_play %>%
    filter(nfl_id == receiver_id) %>%
    mutate(sect_of_field = case_when(between(y, 0, 16.66) ~ 'left',
                                     between(y, 16.67, 37.66) ~ 'middle',
                                     between(y, 37.67, 53) ~ 'right',
                                     TRUE ~ 'out_of_bounds'))
  
  if(the_receiver$sect_of_field %in% 'out_of_bounds'){
    return(list(NA))
  }
  
  the_receiver_downfield <- downfield_x_calc(the_receiver)
  the_receiver_bounds <- end_of_base(the_receiver_downfield)
  the_receiver_speed <- sideline_vel(the_receiver_bounds)
  the_receiver_spatial <- spatial_region(the_receiver_speed)
  
  holder <- the_receiver_spatial$spatial_region[[1]]
  just_coords <- data.frame(holder@polygons[[1]]@Polygons[[1]]@coords)
  container_results <- container(receiving_play, the_receiver_spatial) # This works! Verify with other orientations
  measures <- contained_counts(container_results, receiver)
  
  return(measures)
}

get_elig_rec <- function(frame){
  # Called position_abbr in my updated version, called position in the raw data 
  return(frame %>%
           filter(position %in% c('WR', 'RB', 'TE', 'HB', 'FB')) %>%
           pull(nfl_id))
}

holder <- tibble()

flog.info("Read in receiver info for observed target on play.", name = "all_time")
#receiver <- read_csv("C:/Users/mreyers/Documents/GitHub/bdb_3/Data/additional_data/targetedReceiver.csv") %>%
#  janitor::clean_names()

plan(multisession, workers = 4)
epsilon <- 0
#plan(sequential)
nbrOfWorkers() # Issue on 40, I have 1:39 and 41:91 done. It is the problem game 2017092407 so must skip
for(i in 1:91){
  if(i == 40){
    print("Skipping game 40")
    next
  }
  # This info comes from parallel_observed_new, should probably move it to main
  # Note that i = 1:17 goes 1, 10, 11, .. due to character naming
  flog.info('Starting iteration %s at time %s.', i, format(Sys.time(), '%X'), name = 'all_time')
  file_name <- file_list[i]
  file_name_extract <- str_extract(file_name, "[0-9]+")
  
  #tictoc::tic()
  tracking <- read_csv(paste0(default_path, file_name)) %>%
    select_at(select_cols) %>%
    janitor::clean_names() %>%
    left_join(players %>% select(-display_name), by = "nfl_id") %>%
    rename(velocity = s)
  #tictoc::toc()
  # 4 sec
  
  # Need possession team for current format
  possession <- get_possession_team(tracking)
  
  # Control whether offensive routes are standardized: False for visualization, True for work
  reorient <- FALSE
  
  #tictoc::tic()
  tracking_clean <- tracking %>%
    left_join(possession) %>%
    nest(-game_id, -play_id) %>%
    inner_join(play_ids) %>%
    mutate(data = pmap(list(data, game_id, play_id), ~quick_nest_fix(..1, ..2, ..3)),
           data = map(data, ~standardize_play(., reorient)),
           cleaning = map_dbl(data, ~handle_no_ball(.))) %>%
    filter(!is.na(cleaning), pass_result %in% c('C', 'I', 'IN'))
  rm(tracking)
  #tictoc::toc()
  # 77 sec
  
  # Add additional filtering criteria
  #tictoc::tic()
  tracking_additional <- tracking_clean %>%
    mutate(data = map(data, ball_fix_2),
           target = map_dbl(data, intended_receiver, is_football = new_age_tracking_data),
           complete = map_lgl(data, play_success),
           fake_pt = map_dbl(data, fake_punt),
           qb_check = map_dbl(data, no_qb),
           pass_recorded = map_lgl(data, ~pass_start_event(.)),
           check_pass_catchers = map_dbl(data,
                                         function(x){sum(x$position %in% c("WR", "TE", "RB", "HB", "FB"))})) %>%
    left_join(players %>% dplyr::select(nfl_id, display_name), by = c("target" = "nfl_id")) %>%
    filter(!is.na(target), !is.na(fake_pt), !is.na(qb_check), pass_recorded, check_pass_catchers > 0) %>%
    mutate(receiver = display_name) %>%
    dplyr::select(-display_name) #%>%
  # Stuff below here is for bdb_3
    #dplyr::select(game_id, play_id, data, pass_result) %>%
    #left_join(receiver, by = c("game_id", "play_id")) %>%
    #rename(target = target_nfl_id)
  rm(tracking_clean)
  #tictoc::toc()
  
  # Looks like Brady's data might be messed up for 2017092407
  tracking_rel_frames <- tracking_additional %>%
    mutate(first_elig = map_int(data, ~first_elig_frame(.)),
           last_elig = map_int(data, ~last_elig_frame(.)) + epsilon) %>%
    select(-c(game_id, play_id)) %>%
    unnest(data) %>%
    filter(frame_id >= first_elig, frame_id <= last_elig)
  
  tictoc::tic()
  flog.info(paste0("Starting loop ", i, " parallel section"), name = "all_time")
  #options(future.globals.maxSize=1500*1024^2)
  tracking_yac <- tracking_rel_frames %>%
    ungroup() %>%
    nest(-c(game_id, play_id, frame_id)) %>%
    mutate(elig_receivers = map(data, ~ get_elig_rec(.))) %>%
    unnest(elig_receivers) %>%
    mutate(yac_estimate = future_map2(data, elig_receivers,
                                      ~yac_improvement_wrapper_general(.x, .y)))
  tictoc::toc()
  
  holder <- holder %>%
    bind_rows(tracking_yac %>% select(game_id, play_id, frame_id, elig_receivers, yac_estimate))
  
  holder %>%
    saveRDS('Data/release/yac_covariates.rds')
  
  rm(tracking, tracking_additional,
     tracking_rel_frames, tracking_yac) # seems like there has been some issues with new set being assigned
  toc()
}
