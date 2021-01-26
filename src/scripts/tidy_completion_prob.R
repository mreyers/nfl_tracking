# Can I do completion_probability via tidymodels?
# Load in parallel_observed.R results
# Be sure to run all the functions from the QB_evaluation file first

ngs_features <- readRDS(glue("{default_path}{time_of_arrival_explicit}/observed_covariates.rds"))


flog.info('Data loaded', name = 'comp_prob')


flog.info('Set up really simple data splitting. Can obviously
          be improved, may be later. Who knows.', name = 'comp_prob')
set.seed(1312020)
new_features <-
  ngs_features %>%
  filter(pass_result %in% c("C", "I")) %>%
  left_join(nfl_pbp, by = c('game_id', 'play_id')) %>%
  group_by(pass_result) %>% 
  mutate(pass_result = if_else(pass_result %in% 'C', 'C', 'I'),
         pass_result_f = factor(pass_result, levels = c("C", "I"))) %>%
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
#rm(ngs_features)

# For the new data, I should definitely remove rush separation as new data
# has no lineman information

# There should be additional preprocessing steps that are useful via recipes
# General approach:
  # Create a recipe
  # Include within this recipe a series of preprocessing steps
  # Bake this recipe into a model and workflow
  # Split data into train and test
  # Train the stacked ensemble through grid tuning on base models
specific_vars_at_release <- new_features %>%
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

# Probably could improve further by adding separations at arrival
# I believe these separations are currently at release

specific_vars_at_arrival <- new_features %>%
  ungroup() %>%
  select(pass_result_f, air_dist, rec_separation,
         sideline_sep,
         qb_vel, time_to_throw, dist_from_pocket,
         # Ownership Features, only used 1 due to colinearity
         n_cells = n_cells_at_arrival,
         own_intensity = own_intensity_at_arrival,
         own_avg_intensity = own_avg_intensity_at_arrival,
         # Additional features that differ slightly
         air_yards_x,
         # Context Features
         yardline_100, down, ydstogo)

# Using 75/25 split instead of 85/15 as in paper
set.seed(1312020)
if(time_of_arrival_explicit == "arrival"){
  splits <- initial_split(specific_vars_at_arrival, 0.75, strata = pass_result_f)
} else{
  splits <- initial_split(specific_vars_at_release, 0.75, strata = pass_result_f)
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

comp_prob_wflow <- workflow() %>%
  add_recipe(thesis_recipe)

check <- thesis_recipe %>%
  prep() %>%
  bake(train_ngs)
glimpse(check)

#saveRDS(thesis_recipe, paste0(default_path, type, "/comp_prob_recipe.rds"))
# Need to define basic model architectures with which I will be tuning
# Since goal is ensemble, need same CV splits & control grid
# stacks should be loaded from main.R
set.seed(1312020)
ctrl_grid <- stacks::control_stack_grid()
folds <- rsample::vfold_cv(train_ngs, v = 10)

# From paper that is
  # nnet
  # random forest
  # gbm
  # glm
  # naive bayes

# First nnet setup
nnet_mod <- mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine("nnet") %>%
  set_mode("classification")

  # Need to also normalize input for nnet
nnet_rec <- thesis_recipe %>%
  step_normalize(all_predictors())

nnet_wflow <- comp_prob_wflow %>%
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

rf_wflow <- comp_prob_wflow %>%
  add_model(rf_mod)

rf_res <- tune_grid(object = rf_wflow,
                    resamples = folds,
                    grid = 10,
                    control = ctrl_grid)

# GBM: Replaced by xgboost here due to compatability
gbm_mod <- parsnip::boost_tree(min_n = tune(), tree_depth = tune(), trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("xgboost")

gbm_wflow <- comp_prob_wflow %>%
  add_model(gbm_mod)

gbm_res <- tune_grid(object = gbm_wflow,
                     resamples = folds,
                     grid = 10,
                     control = ctrl_grid)

# GLM
glm_mod <- logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

glm_wflow <- comp_prob_wflow %>%
  add_model(glm_mod)

glm_res <- tune_grid(object = glm_wflow,
                     resamples = folds,
                     grid = 10,
                     control = ctrl_grid)

# Naive Bayes: Requires download of discrim extension: Still not functioning as intended
# nb_mod <- discrim::naive_Bayes(smoothness = tune()) %>%
#   set_engine("klaR")
# 
# nb_wflow <- comp_prob_wflow %>%
#   add_model(nb_mod)
# 
# nb_tune <- tune_grid(object = nb_wflow,
#                      resamples = folds,
#                      grid = 10,
#                      control = ctrl_grid)

# Build the stacks the same way I would build a workflow
comp_prob_stack <- stacks() %>%
  # Add tuned model output
  add_candidates(nnet_res) %>%
  add_candidates(rf_res) %>%
  add_candidates(gbm_res) %>%
  add_candidates(glm_res) %>%
  #add_candidates(nb_res) %>%
  # Ensemble
  blend_predictions() %>%
  fit_members()

# Show prediction weight distribution
comp_prob_stack

# Save the model
saveRDS(comp_prob_stack, glue("{default_path}{type}/comp_prob_with_ownership.rds"))
saveRDS(test_ngs, glue("{default_path}{type}/comp_prob_test_set.rds"))
