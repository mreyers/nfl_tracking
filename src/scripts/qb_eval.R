# QB Evaluation step
# Doing again with epsilon = 0 for comparison requested in July 15th email
# Necessary data for epsilon = 5 stored in "all_frames_epsilon_5.rds"

all_frames_data <- readRDS('Data/release/all_preds_and_covariates_complete.rds')

# Some names in the above are off so I need to replace them
correct_player_names <- readRDS('new_player_name_fix.rds') %>%
  select(target = nfl_id, receiver_fix = display_name_fix)

# Improve the targetted receiver estimation
receiver_fix <- readRDS('~/Github/thesis/receiver_name_fix.rds') %>%
  select(game_id, play_id, second_rec_fix = receiver)

# Now join in
all_frames_data <- all_frames_data %>%
  left_join(correct_player_names) %>%
  mutate(receiver = receiver_fix) %>%
  select(-receiver_fix) # %>%
# left_join(receiver_fix, by = c('game_id', 'play_id')) %>%
# mutate(receiver = second_rec_fix) %>%
# select(-second_rec_fix)

# What does this look like with smoothed probabilities
all_frames_data_temp <- all_frames_data %>%
  nest(-game_id, -play_id, -display_name) %>%
  mutate(smoothed_fit = map(data, ~loess(.$completion_prob_pred ~ .$frame_id_2)),
         smoothed_preds = map2(smoothed_fit, data, ~ predict(.x, newdata = .y)),
         smoothed_preds = map2(smoothed_preds, data, ~ as.data.frame(.x) %>% mutate(frame_id_2 = .y$frame_id_2))) %>%
  select(game_id, play_id, display_name, smoothed_preds) %>%
  unnest(smoothed_preds) %>%
  mutate(smoothed_preds = .x) %>%
  select(-.x)

# Make modifications now that QBs are involved in this: A QB throw is just a tuck
# Because of this, there is a 100% completion probability
# qb_list <- correct_player_names %>%
#   left_join(players %>% select(nfl_id, position_abbr), by = c('target' = 'nfl_id')) %>% 
#   filter(position_abbr %in% 'QB') %>%
#   pull(target)

qb_list <- readRDS('~/Github/thesis/qb_names.rds') %>%
  pull(full_name)

# Add back the smoothed values
all_frames_data <- all_frames_data %>%
  left_join(all_frames_data_temp) %>%
  mutate(completion_prob_pred = if_else(display_name %in% qb_list,
                                        100,
                                        round(completion_prob_pred, 2) * 100),
         completion_prob_pred_smooth = if_else(display_name %in% qb_list,
                                               100,
                                               round(smoothed_preds, 2)* 100),
         incompletion_prob_pred_smooth = 100 - completion_prob_pred_smooth,
         interception_prob_pred_smooth = if_else(display_name %in% qb_list,
                                                 0,
                                                 incompletion_prob_pred_smooth * interception_prob_pred),
         incompletion_prob_pred_smooth = 100 - completion_prob_pred_smooth - interception_prob_pred_smooth)



# Need to add in yards downfield value as it is necessary for EPA calculations
# Just waiting on updates for sacks and runs
poss_yards <- readRDS('~/Github/thesis/yards_to_be_gained.rds') %>%
  dplyr::select(game_id, play_id, frame_id, display_name, yards_downfield)

# Join
all_frames_data_calc <- all_frames_data %>%
  left_join(poss_yards, by = c('game_id', 'play_id', 'frame_id_2' = 'frame_id', 'display_name')) %>%
  mutate(exp_yards = (completion_prob_pred / 100) * yards_downfield)

# Now I need to get the baseline value, the observed quantity the QB chose on the given play
# This differs dependent on epsilon value
selected_rec <- all_frames_data_calc %>%
  group_by(game_id, play_id) %>%
  filter(display_name == receiver) %>% # Name differences, need to do differently
  arrange(desc(frame_id_2)) %>% # Last frame_id is the one when he chose to throw
  slice(1) %>% # Slice 5 because we have added 5 additional frames due to epsilon
  dplyr::select(game_id, play_id, obs_exp_yards = exp_yards)

# Modify the baseline now by adding this back in and measuring quantity of missed performance
all_frames_exp_yards <- all_frames_data_calc %>%
  left_join(selected_rec) %>%
  mutate(exp_yards_diff = exp_yards - obs_exp_yards) %>%
  group_by(game_id, play_id) %>%
  arrange(desc(exp_yards_diff)) %>%
  mutate(perc_of_opt = exp_yards / first(exp_yards)) %>%
  arrange(desc(frame_id_2)) %>%
  mutate(is_chosen = if_else(frame_id_2 == (first(frame_id_2) - 5) & display_name == receiver,
                             TRUE, FALSE)) %>%
  arrange(desc(perc_of_opt))


pbp <- read_csv('~/Github/thesis/reg_pbp_2017.csv', col_types = cols()) %>%
  select(game_id, play_id, play_type, air_yards, yards_after_catch,
         passer_player_name, rusher_player_name) %>%
  mutate(passer_player_name = if_else(is.na(passer_player_name),
                                      rusher_player_name,
                                      passer_player_name))

all_frames_exp_yards <- all_frames_exp_yards %>%
  left_join(pbp %>% select(game_id, play_id, passer_player_name), by = c('game_id', 'play_id'))

avg_qb_decision <- all_frames_exp_yards %>%
  filter(is_chosen) %>%
  group_by(passer_player_name) %>%
  summarize(comp_perc = mean(pass_result_f == 'C', na.rm=TRUE),
            avg_exp_yards = mean(exp_yards, na.rm = TRUE),
            dev_exp_yards = sd(exp_yards, na.rm = TRUE),
            avg_prop_opt = mean(perc_of_opt, na.rm=TRUE),
            dev_prop_opt = sd(perc_of_opt, na.rm = TRUE),
            n_passes = n()) %>%
  filter(n_passes > 20) %>%
  arrange(desc(avg_prop_opt))

print('The following represents QB decision making as a function of maximizing the proportion of 
      expected yards gained on a given play')
print(avg_qb_decision)


# Next point: Expected yards is insufficient as a metric
# It doesnt reward the first down decisions nor the touchdown attempts
# Instead lets try to incorporate the EPA associated with each decision
# This can be built off of the pbp data
# Data can be downloaded from nflscrapr git, I have this from nate work
nfl_2017 <- read_csv("~/Github/thesis/reg_pbp_2017.csv", col_types = cols()) %>%
  rename_all(to_snake_case)

# Generate next play EP, really just lead play
nfl_2017_small <- nfl_2017 %>%
  group_by(game_id, drive) %>%
  mutate(next_ep = ep + epa) %>%
  ungroup() %>%
  select(game_id, play_id, play_type, posteam_type, down, ydstogo, yardline_100, ep, epa, next_ep) %>%
  filter(!is.na(down), play_type %in% 'pass') # Only doing passing analysis currently


# Now to add these to each of the players during the play
pbp_params <- read_csv('~/Github/thesis/reg_pbp_2017.csv', col_types = cols()) %>%
  rename_all(to_snake_case) %>%
  mutate(pass_result = case_when(complete_pass == 1 ~ "C",
                                 incomplete_pass == 1 ~ "I",
                                 sack == 1 ~ "S",
                                 qb_scramble == 1 ~ "R",
                                 interception == 1 ~ "IN",
                                 TRUE ~ NA_character_)) %>%
  select(game_id, play_id, pass_result, yards_after_catch)

all_frames_exp_yards_pbp <- all_frames_exp_yards %>%
  left_join(pbp_params, by = c('game_id', 'play_id'))

# Add Yac
# Quick loess fit for yac
# I think this might be overfitting so use the h2o fit instead
# Main reasoning is that short throws are all being assumed to gain 5+ yards which is likely
# unreasonable

# Check here to fix, I didn't actually regenerate this data set so I expect this is the prob
ep_requirements <- nfl_2017 %>% 
  dplyr::select(game_id, play_id, half_seconds_remaining,
                yardline_100, down, ydstogo, goal_to_go)

non_na_plays <- ep_requirements %>% 
  drop_na() %>%
  dplyr::select(game_id, play_id) %>%
  mutate(valid = TRUE)

# Calculate ep for receivers
all_frames_test <- all_frames_exp_yards_pbp %>%
  ungroup() %>%
  rename(yac = yards_after_catch,
         yac_preds = pred_yac) %>%
  mutate(yac = if_else(is.na(yac), 0, yac),
         yac = if_else(is_chosen & pass_result %in% 'S', 0, yac)) %>% # Current adjust
  filter(!is.na(pass_result)) %>%
  nest(-game_id, -play_id, -yardline_100, -ydstogo, -down) %>%
  left_join(non_na_plays) %>% # Fix for NNET prediction (really multinomial)
  #filter(valid) %>% # Also added a filter for the Goff play that fails with no snap
  mutate(ep_rec = pmap(list(data, game_id, play_id, yardline_100, ydstogo, down), 
                       ~ep_for_receivers_exact(..1, ..2, ..3, ..4, ..5, ..6)))

# Expand back to workable format
all_frames_test <- all_frames_test %>%
  select(game_id, play_id, ydstogo_par = ydstogo, down_par = down, 
         yardline_100_par = yardline_100, ep_rec) %>%
  unnest(ep_rec)

# Some missing plays, seem to be the ones with no snap events
all_frames_exp_yards_test <- all_frames_exp_yards_pbp %>%
  left_join(all_frames_test %>% select(game_id, play_id, frame_id_2, display_name,
                                       los, new_yardline_100, new_down, new_ydstogo, ep,
                                       adj_comp_ep, adj_inc_ep, adj_int_ep, initial_ep,
                                       complete_epa, incomplete_epa, interception_epa),
            by = c('game_id', 'play_id', 'frame_id_2', 'display_name'))

# So now I can modify with the completion probabilities from earlier
all_frames_exp_values <- all_frames_exp_yards_test %>%
  mutate(ep_of_pass = completion_prob_pred_smooth / 100 * adj_comp_ep + 
           incompletion_prob_pred_smooth / 100 * adj_inc_ep +
           interception_prob_pred_smooth / 100 * adj_int_ep,
         epa_of_pass = completion_prob_pred_smooth / 100 * complete_epa + 
           incompletion_prob_pred_smooth / 100 * incomplete_epa +
           interception_prob_pred_smooth / 100 * interception_epa)

# Add in WP if desired
add_wp <- FALSE

if(add_wp){
  # Call the created function to generate the desired win probability values
  results <- wp_function(all_frames_exp_values)
  
  all_frames_exp_values <- all_frames_exp_values %>%
    left_join(results, by = c('game_id', 'play_id', 'frame_id_2', 'display_name'))
}

# Compare these values now the same way I did earlier
best_values_ep <- all_frames_exp_values %>%
  group_by(game_id, play_id) %>%
  arrange(desc(ep_of_pass)) %>%
  slice(1) %>%
  select(game_id, play_id, best_ep = ep_of_pass)

best_values_epa <-  all_frames_exp_values %>%
  group_by(game_id, play_id) %>%
  arrange(desc(epa_of_pass)) %>%
  slice(1) %>%
  select(game_id, play_id, best_epa = epa_of_pass)

# Actually just need to get min value per play
min_val_ep <- all_frames_exp_values %>%
  group_by(game_id, play_id) %>%
  arrange(ep_of_pass) %>%
  slice(1) %>%
  select(game_id, play_id, worst_ep = ep_of_pass)

min_val_epa <- all_frames_exp_values %>%
  group_by(game_id, play_id) %>%
  arrange(epa_of_pass) %>%
  slice(1) %>%
  select(game_id, play_id, worst_epa = epa_of_pass)

all_frames_exp_values <- all_frames_exp_values %>%
  left_join(best_values_ep) %>%
  left_join(best_values_epa) %>%
  left_join(min_val_ep) %>%
  left_join(min_val_epa) %>%
  mutate(ep_gap = best_ep - worst_ep,
         epa_gap = best_epa - worst_epa) %>%
  mutate(diff_best_ep = best_ep - ep_of_pass,
         diff_best_epa = best_epa - epa_of_pass,
         prop_best_ep = round(1 - diff_best_ep / ep_gap, 2) * 100,
         prop_best_epa = round(1 - diff_best_epa / epa_gap, 2) * 100) 


# Tidy up the builder objects to save on RAM
rm('all_frames_data', 'all_frames_data_calc', 'all_frames_data_temp',
   'all_frames_exp_yards_test', 'all_frames_exp_yards', 'all_frames_test')
invisible(gc(verbose=FALSE))

# Okay this is working much better now, these values seem reasonable
# Go in and touch up the ep calculation to be sure
# A few other touch up things before incorporating other important features (interceptions, 
# yards after catch, run options)
qb_eval_2 <- all_frames_exp_values %>%
  group_by(passer_player_name) %>%
  filter(is_chosen) %>%
  summarize(avg_ep = mean(ep_of_pass, na.rm = TRUE),
            avg_prop_ep = mean(prop_best_ep, na.rm = TRUE),
            avg_diff = mean(diff_best_ep, na.rm = TRUE),
            sd_diff = sd(diff_best_ep, na.rm = TRUE),
            n_passes = n(),
            avg_epa = mean(epa_of_pass, na.rm = TRUE),
            avg_prop_epa = mean(prop_best_epa, na.rm = TRUE),
            avg_diff_epa = mean(diff_best_epa, na.rm = TRUE),
            sd_diff_epa = sd(diff_best_epa, na.rm = TRUE)) %>%
  arrange(desc(avg_prop_epa)) %>% 
  filter(n_passes > 100) # This looks good after my rerun on april 21

print('The following represents QB decision making as a function of maximizing the proportion of 
      expected points added on a given play')
print(qb_eval_2)
print('The next plot helps to understand the relationship between average EPA and 
      the average proportion of max EPA. We find the expected connection between higher 
      average EPA and higher average proportion of EPA.')
qb_eval_2 %>% 
  ggplot(aes(x = avg_epa, y = avg_prop_epa)) +
  geom_point() + 
  ggtitle('Relationship between Proportion and Average EPA')

qb_eval_3 <- all_frames_exp_values %>%
  group_by(passer_player_name) %>%
  filter(is_chosen) %>%
  summarize(n_passes = n(),
            avg_ep = mean(ep_of_pass, na.rm = TRUE),
            avg_epa = mean(epa_of_pass, na.rm = TRUE)) %>%
  arrange(desc(avg_epa)) %>%
  filter(n_passes >  100)

# So far this makes a very reasonable list but I think I should be using prop instead of ep
qb_eval_4 <- all_frames_exp_values %>%
  group_by(passer_player_name) %>%
  filter(is_chosen) %>%
  mutate(weighted_ep = yards_downfield * ep_of_pass,
         weighted_epa = yards_downfield * epa_of_pass) %>%
  summarize(n_passes = n(),
            avg_ep = mean(ep_of_pass, na.rm = TRUE),
            avg_epa = mean(epa_of_pass, na.rm = TRUE),
            avg_exp_yards = mean(exp_yards, na.rm = TRUE),
            avg_yards_downfield = mean(yards_downfield, na.rm = TRUE),
            avg_weighted_ep = mean(weighted_ep, na.rm = TRUE),
            avg_weighted_epa = mean(weighted_epa, na.rm = TRUE)) %>%
  arrange(desc(avg_weighted_epa)) %>%
  filter(n_passes > 100)

qb_eval_5 <- all_frames_exp_values %>%
  group_by(passer_player_name) %>%
  filter(is_chosen) %>%
  mutate(weighted_ep = yards_downfield * (prop_best_ep / 100),
         weighted_epa = yards_downfield * (prop_best_epa / 100)) %>%
  summarize(n_passes = n(),
            avg_ep = mean(ep_of_pass, na.rm = TRUE),
            avg_epa = mean(epa_of_pass, na.rm = TRUE),
            avg_exp_yards = mean(exp_yards, na.rm = TRUE),
            avg_yards_downfield = mean(yards_downfield, na.rm = TRUE),
            avg_weighted_ep = mean(weighted_ep, na.rm = TRUE),
            avg_weighted_epa = mean(weighted_epa, na.rm = TRUE)) %>%
  arrange(desc(avg_weighted_epa)) %>%
  filter(n_passes > 100)


# Raw win probability is insufficient, do the same scaling setup as earlier with EP
# Done further below

# This gives some reasonable results but I think some of these EP / EPA values are confounded 
# by score differential and win probability
# Next challenge then is to integrate win probability

# Follow up thought by Dani: what are the average and max EPA typically observed at a given
# time since the snap

testing_dani <- all_frames_exp_values %>% 
  mutate(time_since_snap = (frame_id_2 - first_elig + 1) / 10) %>%
  group_by(game_id, play_id, time_since_snap, is_chosen) %>%
  summarize(avg_ep = mean(ep_of_pass, na.rm = TRUE),
            avg_epa = mean(epa_of_pass, na.rm = TRUE),
            max_ep = max(ep_of_pass),
            max_epa = max(epa_of_pass))

# This plot makes a lot of sense: The best option on a play tends to grow over time
testing_dani %>%
  filter(time_since_snap > 0, time_since_snap < 10) %>%
  group_by(time_since_snap) %>%
  summarize(avg_epa = mean(avg_epa, na.rm = TRUE),
            max_epa = mean(max_epa, na.rm = TRUE)) %>%
  ggplot(aes(x = time_since_snap, y = max_epa)) +
  geom_point() 

# I think this plot is suggesting a minor issue with EPA calculation in short plays
# Especially with respect to TDs or plays that are near the endzone
# Probably overestimating
testing_dani %>%
  filter(time_since_snap > 0, time_since_snap < 10) %>%
  group_by(time_since_snap) %>%
  summarize(avg_epa = mean(avg_epa, na.rm = TRUE),
            max_epa = mean(max_epa, na.rm = TRUE)) %>%
  ggplot(aes(x = time_since_snap, y = avg_epa)) +
  geom_point() 

# This makes sense: Average EPA for actual QB decisions is pretty consistent across times
testing_dani %>%
  filter(time_since_snap > 0, time_since_snap < 10, is_chosen) %>%
  group_by(time_since_snap) %>%
  summarize(avg_epa = mean(avg_epa, na.rm = TRUE),
            max_epa = mean(max_epa, na.rm = TRUE)) %>%
  ggplot(aes(x = time_since_snap, y = avg_epa)) +
  geom_point() 

testing_dani %>%
  filter(time_since_snap > 0, time_since_snap < 10, !is_chosen) %>%
  group_by(time_since_snap) %>%
  summarize(avg_epa = mean(avg_epa, na.rm = TRUE),
            max_epa = mean(max_epa, na.rm = TRUE)) %>%
  ggplot(aes(x = time_since_snap, y = avg_epa)) +
  geom_point() 


# Maybe QB evaluation should be considered with respect to time after snap, weighted
time_weighted <- all_frames_exp_values %>% 
  mutate(time_since_snap = (frame_id_2 - first_elig + 1) / 10) %>%
  group_by(passer_player_name) %>%
  filter(is_chosen) %>%
  mutate(weighted_ep = time_since_snap * ep_of_pass,
         weighted_epa = time_since_snap * epa_of_pass) %>%
  summarize(n_passes = n(),
            avg_ep = mean(ep_of_pass, na.rm = TRUE),
            avg_epa = mean(epa_of_pass, na.rm = TRUE),
            avg_time_to_throw = mean(time_since_snap, na.rm = TRUE),
            avg_exp_yards = mean(exp_yards, na.rm = TRUE),
            avg_yards_downfield = mean(yards_downfield, na.rm = TRUE),
            avg_weighted_ep = mean(weighted_ep, na.rm = TRUE),
            avg_weighted_epa = mean(weighted_epa, na.rm = TRUE)) %>%
  arrange(desc(avg_weighted_epa)) %>%
  filter(n_passes > 100)

# This could be done better by comparing to the average EPA at a given time point
avg_ep_framewise <- all_frames_exp_values %>% 
  mutate(time_since_snap = (frame_id_2 - first_elig + 1) / 10) %>%
  group_by(time_since_snap) %>%
  summarize(n = n(),
            avg_ep_frame = mean(ep_of_pass, na.rm = TRUE, trim = 0.1),
            avg_epa_frame = mean(epa_of_pass, na.rm = TRUE, trim = 0.1)) 

# Should probably loess the above eventually
# In the meantime, go with as is
weighted_by_avg <-  all_frames_exp_values %>% 
  mutate(time_since_snap = (frame_id_2 - first_elig + 1) / 10) %>%
  left_join(avg_ep_framewise, by = "time_since_snap") %>%
  mutate(ep_over_avg_frame = (ep_of_pass - avg_ep_frame) / avg_ep_frame,
         epa_over_avg_frame = (epa_of_pass - avg_epa_frame) / avg_epa_frame) %>%
  group_by(passer_player_name) %>%
  filter(is_chosen) %>%
  summarize(n_passes = n(),
            avg_ep = mean(ep_of_pass, na.rm = TRUE),
            avg_epa = mean(epa_of_pass, na.rm = TRUE),
            avg_time_to_throw = mean(time_since_snap, na.rm = TRUE),
            avg_exp_yards = mean(exp_yards, na.rm = TRUE),
            avg_yards_downfield = mean(yards_downfield, na.rm = TRUE),
            avg_time_ep = mean(avg_ep_frame, na.rm = TRUE),
            avg_time_epa = mean(avg_epa_frame, na.rm = TRUE)) %>%
  arrange(desc(avg_time_epa)) %>%
  filter(n_passes > 100)


# Continuing to explore WP and WPA
  # Can include the trivial version of Win Prob from nflscrapR
nfl_pbp_wp <- read_csv("~/Github/thesis/reg_pbp_2017.csv") %>%
  janitor::clean_names() %>%
  select(game_id, play_id, wp) %>%
  mutate(wp = if_else(is.na(wp), 0.5, wp))

all_frames_exp_values <- all_frames_exp_values %>%
  left_join(nfl_pbp_wp, by = c("game_id", "play_id")) %>% 
  mutate(off_wp = wp)

# Compare these values now the same way I did earlier
best_values_wp <- all_frames_exp_values %>%
  group_by(game_id, play_id) %>%
  arrange(desc(new_off_wp)) %>%
  slice(1) %>%
  select(game_id, play_id, best_wp = new_off_wp)

best_values_wpa <-  all_frames_exp_values %>%
  group_by(game_id, play_id) %>%
  arrange(desc(wpa)) %>%
  slice(1) %>%
  select(game_id, play_id, best_wpa = wpa)

# Need to do a min shift, find minimum value in data set <- actually probably not for wp
min_val_wp <- all_frames_exp_values %>%
  group_by(game_id, play_id) %>%
  arrange(new_off_wp) %>%
  slice(1) %>%
  select(game_id, play_id, worst_wp = new_off_wp)

min_val_wpa <- all_frames_exp_values %>%
  group_by(game_id, play_id) %>%
  arrange(wpa) %>%
  slice(1) %>%
  select(game_id, play_id, worst_wpa = wpa)

all_frames_exp_values <- all_frames_exp_values %>%
  left_join(best_values_wp) %>%
  left_join(best_values_wpa) %>%
  left_join(min_val_wp) %>%
  left_join(min_val_wpa) %>%
  mutate(wp_gap = best_wp - worst_wp,
         wpa_gap = best_wpa - worst_wpa) %>%
  mutate(diff_best_wp = best_wp - new_off_wp,
         diff_best_wpa = best_wpa - wpa,
         prop_best_wp = round(1 - diff_best_wp / wp_gap, 2) * 100,
         prop_best_wpa = round(1 - diff_best_wpa / wpa_gap, 2) * 100) 

# If I do any filtering by win probability, my threshold for passes should be lower
# Currently using 100 for weighted but not filtered and 75 for anything filtered

qb_eval_6 <- all_frames_exp_values %>%
  group_by(passer_player_name) %>%
  filter(is_chosen, off_wp > 0.1, off_wp < 0.9) %>%
  summarize(avg_wp = mean(off_wp, na.rm = TRUE),
            avg_prop_wp = mean(prop_best_wp, na.rm = TRUE),
            avg_diff = mean(diff_best_wp, na.rm = TRUE),
            sd_diff = sd(diff_best_wp, na.rm = TRUE),
            n_passes = n(),
            avg_wpa = mean(wpa, na.rm = TRUE),
            avg_prop_wpa = mean(prop_best_wpa, na.rm = TRUE),
            avg_diff_wpa = mean(diff_best_wpa, na.rm = TRUE),
            sd_diff_wpa = sd(diff_best_wpa, na.rm = TRUE)) %>%
  arrange(desc(avg_prop_wpa)) %>% 
  filter(n_passes > 75)

# Needs to be weighted by the maximum WP achievable on the play, 1 - initial
qb_eval_7 <- all_frames_exp_values %>%
  mutate(max_wp_possible = 1 - off_wp,
         is_likely_to_win = off_wp > 0.5,
         weighting = max_wp_possible * (1 - max_wp_possible),
         weighted_wpa = weighting * prop_best_wpa / 0.25) %>% # Max is 0.25, normalize 0-1
  group_by(passer_player_name) %>%
  filter(is_chosen) %>%
  summarize(n_passes = n(),
            total_wpa = sum(weighted_wpa, na.rm = TRUE),
            avg_wpa = mean(weighted_wpa, na.rm = TRUE)) %>%
  arrange(desc(avg_wpa)) %>%
  filter(n_passes > 100)

# Maybe WP is actually a filter process for EPA
qb_eval_8 <- all_frames_exp_values %>%
  filter(is_chosen, off_wp > 0.1, off_wp < 0.9) %>%
  group_by(passer_player_name) %>%
  summarize(n_passes = n(),
            tot_epa = sum(epa_of_pass, na.rm = TRUE),
            avg_epa = mean(epa_of_pass, na.rm = TRUE)) %>%
  arrange(desc(avg_epa)) %>%
  filter(n_passes > 75)

# Realized I hadnt tried a prop epa yet with filtering
qb_eval_8_5 <- all_frames_exp_values %>%
  filter(is_chosen, off_wp > 0.1, off_wp < 0.9) %>%
  group_by(passer_player_name) %>%
  summarize(n_passes = n(),
            tot_epa = sum(epa_of_pass, na.rm = TRUE),
            avg_epa = mean(epa_of_pass, na.rm = TRUE),
            avg_prop_epa = mean(prop_best_epa, na.rm = TRUE)) %>%
  arrange(desc(avg_prop_epa)) %>%
  filter(n_passes > 75)

qb_eval_for_paper_table <- all_frames_exp_values %>%
  filter(pass_result %in% c('C', 'I', 'IN', 'S')) %>%
  filter(is_chosen, off_wp > 0.1, off_wp < 0.9) %>%
  group_by(passer_player_name) %>%
  summarize(n_passes = n(),
            tot_epa = sum(epa_of_pass, na.rm = TRUE),
            avg_epa = mean(epa_of_pass, na.rm = TRUE),
            avg_prop_epa = mean(prop_best_epa, na.rm = TRUE)) %>%
  arrange(desc(avg_prop_epa)) %>%
  filter(n_passes > 75)

# Plot for Tim on Rushing QBs
all_frames_exp_values %>%
  filter(is_chosen, off_wp > 0.1, off_wp < 0.9) %>%
  filter(passer_player_name %in% c("R.Wilson", "C.Newton", "T.Taylor","D.Kizer")) %>%
  mutate(is_run = (pass_result == "R")) %>%
  ggplot(aes(x = prop_best_epa, col = is_run, fill = is_run)) +
  geom_histogram() +
  ggtitle("Distribution of chosen Runs with respect to optimal decision") +
  facet_wrap(~ passer_player_name)
# summarize(avg_wp = mean(new_off_wp, na.rm = TRUE),
#           avg_prop_wp = mean(prop_best_wp, na.rm = TRUE),
#           avg_diff = mean(diff_best_wp, na.rm = TRUE),
#           sd_diff = sd(diff_best_wp, na.rm = TRUE),
#           n_passes = n(),
#           avg_wpa = mean(wpa, na.rm = TRUE),
#           avg_prop_wpa = mean(prop_best_wpa, na.rm = TRUE),
#           avg_diff_wpa = mean(diff_best_wpa, na.rm = TRUE),
#           sd_diff_wpa = sd(diff_best_wpa, na.rm = TRUE)) 

# If I weight it using my other approach, same EPA style
qb_eval_9 <- all_frames_exp_values %>%
  filter(is_chosen) %>%
  mutate(max_wp_possible = 1 - off_wp,
         is_likely_to_win = off_wp > 0.5,
         weighting = max_wp_possible * (1 - max_wp_possible),
         weighted_epa = weighting * prop_best_epa / 0.25) %>%
  group_by(passer_player_name) %>%
  summarize(n_passes = n(),
            tot_epa = sum(weighted_epa, na.rm = TRUE),
            avg_epa = mean(weighted_epa, na.rm = TRUE)) %>%
  arrange(desc(avg_epa)) %>%
  filter(n_passes > 100)

# Since ESPN QBR is based on EPA, try just with EPA
qb_eval_10 <- all_frames_exp_values %>%
  filter(is_chosen) %>%
  mutate(max_wp_possible = 1 - off_wp,
         is_likely_to_win = off_wp > 0.5,
         weighting = max_wp_possible * (1 - max_wp_possible),
         weighted_epa = weighting * epa_of_pass / 0.25) %>%
  group_by(passer_player_name) %>%
  summarize(n_passes = n(),
            tot_epa = sum(weighted_epa, na.rm = TRUE),
            avg_epa = mean(weighted_epa, na.rm = TRUE)) %>%
  arrange(desc(avg_epa)) %>%
  filter(n_passes > 100)


# So which of these rankings is most closely correlated with the actual data for the whole season?
qbr <- read_csv('Data/season_qbr.csv') %>%
  select(Player, QBR) %>%
  mutate(first_init = str_extract(Player, "^[A-Z]{1}"),
         last_name = str_extract(Player, " [A-z]+"),
         passer_player_name = paste0(first_init, ".", str_extract(last_name, "[A-z]+"))) %>%
  select(passer_player_name, QBR) %>%
  arrange(desc(QBR)) %>%
  mutate(max_qbr = first(QBR),
         min_qbr = last(QBR),
         rel_qbr = (QBR - min_qbr) / (max_qbr - min_qbr)) %>%
  select(passer_player_name, rel_qbr)

# Compare with each of the QB_eval approaches above
comp_2 <- qb_eval_2 %>%
  select(passer_player_name, avg_prop_epa) %>%
  mutate(max_prop_epa = first(avg_prop_epa),
         min_prop_epa = last(avg_prop_epa),
         rel_prop_epa = (avg_prop_epa - min_prop_epa) / (max_prop_epa - min_prop_epa)) %>%
  select(passer_player_name, rel_prop_epa) %>%
  left_join(qbr) %>%
  drop_na() 

cor(comp_2$rel_prop_epa, comp_2$rel_qbr)
comp_2 %>%
  ggplot(aes(x = rel_qbr, y = rel_prop_epa)) +
  geom_point()
# correlation now ~ 27%

comp_3 <- qb_eval_3 %>%
  select(passer_player_name, avg_epa) %>%
  mutate(max_metric = first(avg_epa),
         min_metric = last(avg_epa),
         rel_metric = (avg_epa - min_metric) / (max_metric - min_metric)) %>%
  select(passer_player_name, rel_metric) %>%
  left_join(qbr) %>%
  drop_na() 

cor(comp_3$rel_metric, comp_3$rel_qbr)
comp_3 %>%
  ggplot(aes(x = rel_qbr, y = rel_metric)) +
  geom_point()
# qb_eval_3 a little better
# Now 0.40

comp_4 <- qb_eval_4 %>%
  select(passer_player_name, avg_weighted_epa) %>%
  mutate(max_metric = first(avg_weighted_epa),
         min_metric = last(avg_weighted_epa),
         rel_metric = (avg_weighted_epa - min_metric) / (max_metric - min_metric)) %>%
  select(passer_player_name, rel_metric) %>%
  left_join(qbr) %>%
  drop_na() 

cor(comp_4$rel_metric, comp_4$rel_qbr)
comp_4 %>%
  ggplot(aes(x = rel_qbr, y = rel_metric)) +
  geom_point()
# qb_eval_4 corr 0.29

comp_5 <- qb_eval_5 %>%
  select(passer_player_name, avg_weighted_epa) %>%
  mutate(max_metric = first(avg_weighted_epa),
         min_metric = last(avg_weighted_epa),
         rel_metric = (avg_weighted_epa - min_metric) / (max_metric - min_metric)) %>%
  select(passer_player_name, rel_metric) %>%
  left_join(qbr) %>%
  drop_na() 

cor(comp_5$rel_metric, comp_5$rel_qbr)
comp_5 %>%
  ggplot(aes(x = rel_qbr, y = rel_metric)) +
  geom_point()
# qb_eval_5 worst so far with respect to qbr, 0.16

comp_6 <- qb_eval_6 %>%
  select(passer_player_name, avg_prop_wpa) %>%
  mutate(max_metric = first(avg_prop_wpa),
         min_metric = last(avg_prop_wpa),
         rel_metric = (avg_prop_wpa - min_metric) / (max_metric - min_metric)) %>%
  select(passer_player_name, rel_metric) %>%
  left_join(qbr) %>%
  drop_na() 

cor(comp_6$rel_metric, comp_6$rel_qbr)
comp_6 %>%
  ggplot(aes(x = rel_qbr, y = rel_metric)) +
  geom_point()
# Corr of 0.42


comp_7 <- qb_eval_7 %>%
  select(passer_player_name, avg_wpa) %>%
  mutate(max_metric = first(avg_wpa),
         min_metric = last(avg_wpa),
         rel_metric = (avg_wpa - min_metric) / (max_metric - min_metric)) %>%
  select(passer_player_name, rel_metric) %>%
  left_join(qbr) %>%
  drop_na() 

cor(comp_7$rel_metric, comp_7$rel_qbr)
comp_7 %>%
  ggplot(aes(x = rel_qbr, y = rel_metric)) +
  geom_point()
# qb_eval_7 is about 0.36, right up there with qb_eval_3

comp_8 <- qb_eval_8 %>%
  select(passer_player_name, avg_epa) %>%
  mutate(max_metric = first(avg_epa),
         min_metric = last(avg_epa),
         rel_metric = (avg_epa - min_metric) / (max_metric - min_metric)) %>%
  select(passer_player_name, rel_metric) %>%
  left_join(qbr) %>%
  drop_na() 

cor(comp_8$rel_metric, comp_8$rel_qbr)
comp_8 %>%
  ggplot(aes(x = rel_qbr, y = rel_metric)) +
  geom_point()
# qb_eval_8 about 0.36, another decent option
# Is 0.56 when we cut off wp \in [0.2, 0.8] instead of [0.1, 0.9]

comp_8_5 <- qb_eval_8_5 %>%
  select(passer_player_name, avg_prop_epa) %>%
  mutate(max_metric = first(avg_prop_epa),
         min_metric = last(avg_prop_epa),
         rel_metric = (avg_prop_epa - min_metric) / (max_metric - min_metric)) %>%
  select(passer_player_name, rel_metric) %>%
  left_join(qbr) %>%
  drop_na() 

cor(comp_8_5$rel_metric, comp_8_5$rel_qbr)
comp_8_5 %>%
  ggplot(aes(x = rel_qbr, y = rel_metric)) +
  geom_point()
# qb_eval_8_5 about 0.384, decent option

comp_9 <- qb_eval_9 %>%
  select(passer_player_name, avg_epa) %>%
  mutate(max_metric = first(avg_epa),
         min_metric = last(avg_epa),
         rel_metric = (avg_epa - min_metric) / (max_metric - min_metric)) %>%
  select(passer_player_name, rel_metric) %>%
  left_join(qbr) %>%
  drop_na() 

cor(comp_9$rel_metric, comp_9$rel_qbr)
comp_9 %>%
  ggplot(aes(x = rel_qbr, y = rel_metric)) +
  geom_point()
# qb_eval_9 is 0.35ish as well, alright

comp_10 <- qb_eval_10 %>%
  select(passer_player_name, avg_epa) %>%
  mutate(max_metric = first(avg_epa),
         min_metric = last(avg_epa),
         rel_metric = (avg_epa - min_metric) / (max_metric - min_metric)) %>%
  select(passer_player_name, rel_metric) %>%
  left_join(qbr) %>%
  drop_na() 

cor(comp_10$rel_metric, comp_10$rel_qbr)
comp_10 %>%
  ggplot(aes(x = rel_qbr, y = rel_metric)) +
  geom_point()
# 0.44 correlation, pretty good as well
