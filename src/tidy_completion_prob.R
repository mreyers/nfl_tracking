# Can I do completion_probability via tidymodels?
# Load in parallel_observed.R results
# Be sure to run all the functions from the QB_evaluation file first
n_games <- dim(games)[1]
iter <- ceiling(n_games / 10)

ngs_features <- tibble()
for(i in 1:iter){
  lower <- (i-1) * 10 + 1
  upper <- i * 10
  upper <- min(upper, n_games)
  
  temp <- readRDS(paste0('observed_covariates', lower, '_', upper, '.rds'))
  
  ngs_features <- ngs_features %>%
    bind_rows(temp)
}

flog.info('Data loaded', name = 'comp_prob')

flog.info('Loading in standard nflscrapR data. Have updated to nflfastR
          and require just minimal changes to adjust. This assumes
          that a user only wants 2017 data. Come change the range
          of season to get more data.', name = 'comp_prob')

seasons <- 2017:2017
nfl_pbp <- purrr::map_df(seasons, function(x) {
  readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.csv.gz")
  )
}) %>%
  dplyr::select(game_id = old_game_id,
                play_id, yardline_100, down, ydstogo)

flog.info('Set up really simple data splitting. Can obviously
          be improved, may be later. Who knows.', name = 'comp_prob')
set.seed(1312020)
new_features <-
  ngs_features %>%
  left_join(nfl_pbp, by = c('game_id', 'play_id')) %>%
  group_by(pass_result) %>% 
  mutate(pass_result = if_else(pass_result %in% 'C', 'C', 'I'),
         pass_result_f = factor(pass_result, levels = c("C", "I")))
rm(ngs_features)

library(tidymodels)

# There should be additional preprocessing steps that are useful via recipes
# General approach:
  # Create a recipe
  # Include within this recipe a series of preprocessing steps
  # Bake this recipe into a model and workflow
  # Split data into train and test
  # Train the stacked ensemble through grid tuning on base models
specific_vars <- new_features %>%
  ungroup() %>%
  select(pass_result_f, air_dist, rec_separation,
         sideline_sep, no_frame_rush_sep, 
         qb_vel, time_to_throw, dist_from_pocket, 
         # Ownership Features, only used 1 due to colinearity
         n_cells_at_throw, own_intensity_at_throw, own_avg_intensity_at_throw,
         # Additional features that differ slightly
         air_yards_x,
         # Context Features
         yardline_100, down, ydstogo)

splits <- initial_split(specific_vars, 0.85, strata = pass_result_f)

train_ngs <- training(splits)
test_ngs <- testing(splits)

thesis_recipe <- recipe(pass_result_f ~ ., data = train_ngs) %>%
  # Just this column has <10 NAs, breaking workflow
  step_knnimpute(own_avg_intensity_at_throw) %>%
  step_zv(all_predictors()) %>%
  step_num2factor(down, levels = c("1", "2", "3", "4")) %>%
  step_pca(all_numeric()) %>%
  step_dummy(down)

comp_prob_wflow <- workflow() %>%
  add_recipe(thesis_recipe)

# Need to define basic model architectures with which I will be tuning
# Since goal is ensemble, need same CV splits & control grid
set.seed(1312020)
ctrl_grid <- control_stack_grid()
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

# Naive Bayes: Requires download of discrim extension
# Return to this after reboot
remotes::install_github("tidymodels/discrim")
nb_mod <- discrim::naive_Bayes(smoothness = tune()) %>%
  set_engine("klaR")

nb_wflow <- comp_prob_wflow %>%
  add_model(nb_mod)

nb_tune <- tune_grid(object = nb_wflow,
                     resamples = folds,
                     grid = 10,
                     control = ctrl_grid)

# To run stacking, need stacks
# remotes::install_github("tidymodels/stacks", ref = "main")
library(stacks)

# Build the stacks the same way I would build a workflow
comp_prob_stack <- stacks() %>%
  # Add tuned model output
  add_candidates(nnet_res) %>%
  add_candidates(rf_res) %>%
  add_candidates(gbm_res) %>%
  add_candidates(glm_res) %>%
  # add_candidates(nb_res) %>%
  # Ensemble
  blend_predictions() %>%
  fit_members()

# Show prediction weight distribution
comp_prob_stack

# Gut check
theme_set(theme_bw())
autoplot(comp_prob_stack)

# Actual test set predictions
comp_prob_test <- ngs_test %>%
  bind_cols(predict(comp_prob_stack, ., type = "prob"))

# roc_auc = 0.763
roc_auc(comp_prob_test,
        truth = pass_result_f,
        contains(".pred_C"))

# Comparison of ensemble to the individual learners
comp_prob_all_test <- ngs_test %>%
  select(pass_result_f) %>%
  bind_cols(predict(comp_prob_stack, ngs_test, 
                    type = "class", members = TRUE))

comp_prob_all_test
