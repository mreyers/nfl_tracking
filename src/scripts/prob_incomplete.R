flog.appender(appender.file('logs/inc_int_model.log'), 'inc')
flog.info('Start of incompletion and interception modeling', name = 'inc')

# This is currently just interceptions, also need the incomplete plays which are stored elsewhere
all_inc_and_int <- readRDS(paste0(default_path, "observed_covariates.rds")) %>%
  filter(pass_result %in% c('I', 'IN'))


# Now to model the probability of an interception
# First need just the targeted receiver at time of release
flog.info('Preprocess the data for training and model building.', name = 'inc')

set.seed(1312020)
all_passes <-
  all_inc_and_int %>%
  left_join(nfl_pbp, by = c('game_id', 'play_id')) %>%
  group_by(pass_result) %>% 
  mutate(pass_result = if_else(pass_result %in% 'I', 'I', 'IN'),
         pass_result_f = factor(pass_result, levels = c("I", "IN"))) %>%
  left_join(players %>% select(nfl_id, position), by = c("target" = "nfl_id")) %>%
  mutate(position_f = factor(case_when(
    position == "WR" ~ "WR",
    position == "TE" ~ "TE",
    TRUE ~ "RB"
  ), levels = c("WR", "TE", "RB"))) %>%
  select(-position) %>%
  # Most of these are flagged plays, a few random data errors
  filter(!is.na(score_differential)) %>%
  ungroup()

specific_vars_at_release <- all_passes %>%
  ungroup() %>%
  select(pass_result_f, air_dist, rec_separation,
         sideline_sep,
         qb_vel, time_to_throw, dist_from_pocket, 
         # Ownership Features, only used 1 due to colinearity
         n_cells = n_cells_at_throw,
         own_intensity = own_intensity_at_throw,
         own_avg_intensity = own_avg_intensity_at_throw,
         # Additional features that differ slightly
         air_yards_x,
         # Context Features
         yardline_100, down, ydstogo,
         # Additional features created for calibration testing
         score_differential, is_redzone, number_of_pass_rushers, position_f)

set.seed(1312020)
if(time_of_arrival_explicit == "arrival"){
  splits <- initial_split(specific_vars_at_arrival, 0.75)
} else{
  splits <- initial_split(specific_vars_at_release, 0.75)
}

train_ngs <- training(splits)
test_ngs <- testing(splits)

# Change the recipe such that ownership only is PCA'd, too heavily correlated but useful
thesis_recipe <- recipe(pass_result_f ~ ., data = train_ngs) %>%
  # Just this column has <10 NAs, breaking workflow
  step_knnimpute(own_avg_intensity, number_of_pass_rushers) %>%
  step_zv(all_predictors()) %>%
  step_num2factor(down, levels = c("1", "2", "3", "4")) %>%
  step_pca(n_cells, own_intensity, own_avg_intensity) %>%
  step_dummy(down, position_f, is_redzone)

int_prob_wflow <- workflow() %>%
  add_recipe(thesis_recipe)

set.seed(1312020)
ctrl_grid <- stacks::control_stack_grid()
folds <- rsample::vfold_cv(train_ngs, v = 10)

nnet_mod <- mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine("nnet") %>%
  set_mode("classification")

# Need to also normalize input for nnet
nnet_rec <- thesis_recipe %>%
  step_normalize(all_predictors())

nnet_wflow <- int_prob_wflow %>%
  update_recipe(nnet_rec) %>%
  add_model(nnet_mod)

# If nnet all fail its likely because of NA in train set
nnet_res <- tune_grid(object = nnet_wflow,
                      resamples = folds,
                      grid = 10,
                      control = ctrl_grid)

# The other models follow similar processing but without need to modify recipe
# Random Forest
rf_mod <- rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("ranger")

rf_wflow <- int_prob_wflow %>%
  add_model(rf_mod)

rf_res <- tune_grid(object = rf_wflow,
                    resamples = folds,
                    grid = 10,
                    control = ctrl_grid)

# GBM: Replaced by xgboost here due to compatability
gbm_mod <- parsnip::boost_tree(min_n = tune(), tree_depth = tune(), trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("xgboost")

gbm_wflow <- int_prob_wflow %>%
  add_model(gbm_mod)

gbm_res <- tune_grid(object = gbm_wflow,
                     resamples = folds,
                     grid = 10,
                     control = ctrl_grid)

# GLM
glm_mod <- logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

glm_wflow <- int_prob_wflow %>%
  add_model(glm_mod)

glm_res <- tune_grid(object = glm_wflow,
                     resamples = folds,
                     grid = 10,
                     control = ctrl_grid)


# Stacks are not working here, lets just choose the best model from the fits
nnet_res %>% show_best()
rf_res %>% show_best()
gbm_res %>% show_best()
glm_res %>% show_best()

# Choose first nnet
best_nnet <- nnet_res %>% select_best()

nnet_mod <- mlp(hidden_units = best_nnet$hidden_units,
                penalty = best_nnet$penalty, epochs = best_nnet$epochs) %>%
  set_engine("nnet") %>%
  set_mode("classification")

# Need to also normalize input for nnet
nnet_rec <- thesis_recipe %>%
  step_normalize(all_predictors())

nnet_final <- int_prob_wflow %>%
  update_recipe(nnet_rec) %>%
  add_model(nnet_mod) %>%
  fit(specific_vars_at_release)

# Save the model, run it in predict_all_frames_incomplete
saveRDS(nnet_final, glue::glue("{default_path}{time_of_arrival_explicit}/int_prob.rds"))
