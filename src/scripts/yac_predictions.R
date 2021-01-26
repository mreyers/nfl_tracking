# Do the actual yards after catch modeling to generate yac predictions
yac_covariates <- readRDS(
  glue('{default_path}{time_of_arrival_explicit}/yac_covariates.rds')
  ) %>%
  unnest(yac_estimate) %>%
  select(-yac_estimate)

# Just need the final frame for targeted receiver
  # This will be last frame but for all receivers, filter on join
yac_covariates_observed <- yac_covariates %>%
  group_by(game_id, play_id, elig_receivers) %>%
  arrange(desc(frame_id)) %>%
  slice(1)

# Load additional data, use nflfastR
flog.info(glue("Loading the data from the {seasons} season"))
nflfastr_stuff <- readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{seasons}.rds")
    )
  ) %>%
  # Need to grab observed YAC and the side of field (left/middle/right) of pass
  dplyr::select(game_id = old_game_id, play_id, yards_after_catch, pass_location)

# Now to join it with the data set
observed_target <- readRDS(glue(
  '{default_path}{time_of_arrival_explicit}/observed_covariates.rds')
  )

observed_target <- observed_target %>%
  left_join(yac_covariates_observed, by = c('game_id', 'play_id'))

# Add additional context variables
observed_target <- observed_target %>%
  left_join(nfl_pbp,
            by = c('game_id', 'play_id')) %>%
  left_join(players %>% select(nfl_id, position), by = c("target" = "nfl_id")) %>%
  mutate(position_f = factor(case_when(
    position == "WR" ~ "WR",
    position == "TE" ~ "TE",
    TRUE ~ "RB"
  ), levels = c("WR", "TE", "RB")),
  down = factor(down, levels = c(1, 2, 3, 4))) %>%
  select(-position)

# Setup for stacking model
set.seed(1312020)
observed_target <- observed_target %>%
  mutate(game_id = as.character(game_id)) %>%
  filter(pass_result %in% 'C') %>% # YAC only on completions
  left_join(nflfastr_stuff, by = c('game_id', 'play_id')) %>%
  mutate(set = sample(c('train', 'test'),
                      size = n(), replace = TRUE, prob = c(0.75, 0.25)))

# Remove the ones with NA pass_locations, necessary for model
observed_target <- observed_target %>%
  filter(!is.na(pass_location))

# Need to grab only the receiver with the ball
observed_target_prep <- observed_target %>%
  filter(elig_receivers == target) %>%
  select(air_dist, rec_separation, sideline_sep,
          qb_vel, time_to_throw,
          # Ownership related covariates
          n_cells_at_throw, own_intensity_at_throw, own_avg_intensity_at_throw,
          air_yards_x, air_time_ball,
          # Specific features for yards after catch
          dist_def, dist_def_x,
          # Contextual features
          yardline_100, down, ydstogo, score_differential, is_redzone,
          position_f,
         # Response
          yards_after_catch,
         # Partition
         set) %>%
  mutate(yards_after_catch = if_else(is.na(yards_after_catch), 0, yards_after_catch))

ngs_train <- observed_target_prep %>%
  filter(set %in% 'train')

ngs_test <- observed_target_prep %>%
  filter(set %in% 'test') 

# Tidymodels stacking hype
  # This section will follow tidy_completion_prob pretty much exactly, just instead
  # making use of a continuous response variable.

# Change the recipe such that ownership only is PCA'd, too heavily correlated but useful
thesis_recipe <- recipe(yards_after_catch ~ ., data = ngs_train) %>%
  step_rm(set) %>%
  step_knnimpute(own_avg_intensity_at_throw,
                 dist_def, dist_def_x) %>%
  step_zv(all_predictors()) %>%
  step_pca(n_cells_at_throw, own_intensity_at_throw, own_avg_intensity_at_throw) %>%
  step_dummy(down, position_f, is_redzone)

yac_wflow <- workflow() %>%
  add_recipe(thesis_recipe)

check <- thesis_recipe %>%
  prep() %>%
  bake(ngs_train)
glimpse(check)

# Need to define basic model architectures with which I will be tuning
# Since goal is ensemble, need same CV splits & control grid
# stacks should be loaded from main.R
set.seed(1312020)
ctrl_grid <- stacks::control_stack_grid()
folds <- rsample::vfold_cv(ngs_train, v = 10)

# From paper that is
# nnet
# random forest
# gbm
# glm
# naivebayes - dropped from tidymodels implementation

# First nnet setup
nnet_mod <- mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine("nnet") %>%
  set_mode("regression")

# Need to also normalize input for nnet
nnet_rec <- thesis_recipe %>%
  step_normalize(all_predictors())

nnet_wflow <- yac_wflow %>%
  update_recipe(nnet_rec) %>%
  add_model(nnet_mod)

# If nnet all fail its likely because of NA in train set
nnet_res <- tune_grid(object = nnet_wflow,
                      resamples = folds,
                      grid = 10,
                      control = ctrl_grid)

# The other models follow similar processing but without need to modify recipe
# Random Forest
rf_mod <- rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>%
  set_mode("regression") %>%
  set_engine("ranger")

rf_wflow <- yac_wflow %>%
  add_model(rf_mod)

rf_res <- tune_grid(object = rf_wflow,
                    resamples = folds,
                    # Specify a grid as random forest needs to finalize a param
                    grid = grid_max_entropy(
                      finalize(mtry(), ngs_train %>% select(-yards_after_catch, -set)),
                      min_n(),
                      trees(),
                      size = 10
                    ),
                    control = ctrl_grid)

# GBM: Replaced by xgboost here due to compatability
gbm_mod <- parsnip::boost_tree(min_n = tune(), tree_depth = tune(), trees = tune()) %>%
  set_mode("regression") %>%
  set_engine("xgboost")

gbm_wflow <- yac_wflow %>%
  add_model(gbm_mod)

gbm_res <- tune_grid(object = gbm_wflow,
                     resamples = folds,
                     grid = 10,
                     control = ctrl_grid)

# Regression (can't use logistic_reg in continuous setting via parsnip)
glm_mod <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

glm_wflow <- yac_wflow %>%
  add_model(glm_mod)

glm_res <- tune_grid(object = glm_wflow,
                     resamples = folds,
                     grid = 10,
                     control = ctrl_grid)


# Build the stacks the same way I would build a workflow
yac_stack <- stacks() %>%
  # Add tuned model output
  add_candidates(nnet_res) %>%
  add_candidates(rf_res) %>%
  add_candidates(gbm_res) %>%
  add_candidates(glm_res) %>%
  # Ensemble
  blend_predictions() %>%
  fit_members()

# Show prediction weight distribution
yac_stack

# Save the yac_stack to file
saveRDS(yac_stack, glue("{default_path}{time_of_arrival_explicit}/yac_stack_model.rds"))
saveRDS(test_ngs, glue("{default_path}{time_of_arrival_explicit}/yac_test_set.rds"))

# Try h2o and normal stacking approach, curious if similar performance
  # h2o is pretty cool though
  # Will need to be tested more thoroughly in full run

# h2o section
# h2o::h2o.init(max_mem_size = '4G')
# 
# h2o_train <- ngs_train %>%
#   h2o::as.h2o()
# 
# h2o_test <- ngs_test %>%
#   h2o::as.h2o()
# 
# x <- c(# NGS Features
#   'air_dist', 'rec_separation', 'sideline_sep', 'no_frame_rush_sep',
#   'qb_vel', 'time_to_throw', #'dist_from_pocket',  distance from pocket least useful var, model without
#   'n_cells_at_throw', 'own_intensity_at_throw', 'own_avg_intensity_at_throw',
#   'air_yards_x', 'air_time_ball',
#   'dist_def', 'dist_def_x')
# 
# y <- 'yards_after_catch'
# 
# yac_model <- h2o::h2o.automl(x = x, # For memory issues
#                              y = y,
#                              training_frame = h2o_train,
#                              nfolds = 10,
#                              max_runtime_secs = 300,
#                              seed = 1312020)
# 
# 
# #3.43, fit is better without these additional factors. Need to beat 3.41
# rmse_fewer_vars <- mean(sqrt((as.data.frame(h2o::h2o.predict(yac_model, newdata = h2o_test))$predict -
#                                 ngs_test$yards_after_catch)^2))
# # Looks like it is now 3.64 after adjusting covariates
# # Need also to check RMSE for QB runs
# # Training set rmse for Tim
# mean(sqrt((as.data.frame(h2o::h2o.predict(yac_model, newdata = h2o_train))$predict -
#              ngs_train$yards_after_catch)^2), na.rm=TRUE)
# 
# # correlation of about 0.5, not horrible

# Can I get a diagnostic plot here?
# test_set_preds <- as.data.frame(h2o::h2o.predict(yac_model, newdata = h2o_test))$predict

# Get the predictions for all completions trivially from this
# Then calculate the error associated with this

# Now I will just run this again on all the observed catches
# Then I will use it to predict for all frames, potentially offering better estimates of YAC

h2o_train <- observed_target %>%
  h2o::as.h2o()

yac_model <- h2o::h2o.automl(x = x,
                             y = y,
                             training_frame = h2o_train,
                             validation_frame = h2o_test,
                             nfolds = 10,
                             max_runtime_secs = 120,
                             seed = 1312020)

preds_all_catches <- as.data.frame(h2o::h2o.predict(yac_model, newdata = h2o_train))$predict

observed_target <- observed_target %>%
  mutate(yac = preds_all_catches)



yac_estimates <- readRDS('Data/yac_covariates_apr_20_5add_frames.rds') %>%
  unnest(yac_estimate) %>% 
  mutate(display_name = elig_receivers) %>%
  dplyr::select(-elig_receivers, -yac_estimate)

yac_preds <- tibble()
for(i in 1:9){
  lower <- (i - 1) * 10 + 1
  if(i == 9){
    upper <- 91
  } else{
    upper <- i * 10
  }
  base_data <- readRDS(paste0('all_track_res_5add_frames', lower, '_', upper, '.rds')) %>%
    left_join(yac_estimates, by = c('game_id', 'play_id', 'frame_id_2' = 'frame_id', 'display_name'))
  # Model predictions
  
  # Fix pocket_dist
  # Quick fix for a column that is still nested
  temp <- base_data %>% 
    group_by(game_id, play_id) %>% 
    slice(1) %>%
    select(game_id, play_id, first_elig, last_elig, pocket_dist) %>%
    unnest(pocket_dist) %>% 
    mutate(frame_id_2 = first_elig + row_number() - 1) %>%
    select(game_id, play_id, frame_id_2, dist_from_pocket)
  
  base_data <- base_data %>%
    dplyr::select(-pocket_dist) %>%
    left_join(temp, by = c('game_id', 'play_id', 'frame_id_2'))
  
  base_data <- base_data %>%
    mutate(yac_pred = 
             as.data.frame(h2o::h2o.predict(yac_model, newdata = h2o::as.h2o(base_data)))$predict) %>%
    dplyr::select(game_id, play_id, frame_id_2, display_name, yac_pred)
  
  yac_preds <- yac_preds %>% 
    bind_rows(base_data)
}

# Additional stuff for the sack and scramble data that was not used in above

base_data_sack <- readRDS('sack_and_rush_plays_5add_frames.rds') %>%
  left_join(yac_estimates,
            by = c('game_id', 'play_id', 'frame_id_2' = 'frame_id', 'display_name')) %>%
  group_by(game_id, play_id, frame_id_2, display_name) %>% slice(1) %>% ungroup()
# Model predictions

# Fix pocket_dist
# Quick fix for a column that is still nested
temp_sack <- base_data_sack %>% 
  group_by(game_id, play_id) %>% 
  slice(1) %>%
  select(game_id, play_id, first_elig, last_elig, pocket_dist) %>%
  unnest(pocket_dist) %>% 
  mutate(frame_id_2 = first_elig + row_number() - 1) %>%
  select(game_id, play_id, frame_id_2, dist_from_pocket)

base_data_sack <- base_data_sack %>%
  dplyr::select(-pocket_dist) %>%
  left_join(temp_sack, by = c('game_id', 'play_id', 'frame_id_2'))

base_data_sack <- base_data_sack %>%
  ungroup() %>%
  mutate(yac_pred = 
           as.data.frame(h2o::h2o.predict(yac_model, newdata = h2o::as.h2o(base_data_sack)))$predict) %>%
  dplyr::select(game_id, play_id, frame_id_2, display_name, pass_result, is_chosen, yac_pred) 

yac_preds <- yac_preds %>% 
  bind_rows(base_data_sack %>% dplyr::select(-pass_result, -is_chosen))



yac_preds %>%
  saveRDS('yac_preds_apr_20_5add_frames.rds')

h2o::h2o.shutdown()

rm(yac_preds)
rm(base_data_sack)
gc(verbose=FALSE)

# My question about rmse for QB runs
qb_runs <- base_data_sack %>%
  filter(pass_result %in% "R", is_chosen)
# Now join the actual results
qb_obs_runs <- read_csv('reg_pbp_2017.csv', col_types = cols()) %>%
  dplyr::select(game_id, play_id, yards_gained,desc)

qb_runs <- qb_runs %>%
  left_join(qb_obs_runs, by = c("game_id", "play_id")) %>%
  filter(!is.na(yards_gained)) # Removes 1 Trevor Seimian scramble

# Fumble counter, 1
qb_runs %>% filter(str_detect(desc, "fumble|FUMBLE"))

# RMSE estimate
mean(sqrt((qb_runs$yac_pred - qb_runs$yards_gained)^2))
