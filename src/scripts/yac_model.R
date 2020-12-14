# Do the actual yards after catch modeling to generate yac predictions
yac_covariates <- readRDS('Data/release/yac_covariates.rds') %>%
  unnest(yac_estimate) %>%
  select(-yac_estimate)

nfl_scrapr_stuff <- read_csv('~/Github/thesis/reg_pbp_2017.csv', col_types = cols()) %>%
  dplyr::select(game_id, play_id, yards_after_catch)

# Now to join it with the data set
observed_target <- readRDS('Data/observed_covariates.rds') %>%
  mutate(throw_frame = first_elig + 10 * time_to_throw)

observed_target <- observed_target %>%
  left_join(yac_covariates, by = c('game_id', 'play_id', 'throw_frame' = 'frame_id',
                                   'target' = 'elig_receivers')) %>%
  group_by(game_id, play_id) %>%
  slice(1) %>%
  ungroup()

# Try a simple h2o build model
set.seed(1312020)
observed_target <- observed_target %>%
  filter(pass_result %in% 'C') %>% # YAC only on completions
  left_join(nfl_scrapr_stuff, by = c('game_id', 'play_id')) 

side_of_field_stuff <- read_rds('~/Github/thesis/pbp_17.rds')

observed_target <- observed_target %>%
  left_join(side_of_field_stuff) %>%
  filter(!is.na(pass_location))

# # # Model prep
new_features <-
  observed_target %>%
  left_join(nfl_pbp, by = c('game_id', 'play_id')) %>%
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
  select(yards_after_catch, air_dist, rec_separation,
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
         score_differential, is_redzone, number_of_pass_rushers, position_f) %>%
  filter(!is.na(yards_after_catch))

# This is with 0.85 split
set.seed(1312020)
type <- "release" # "release" / "arrival"
if(type == "arrival"){
  splits <- initial_split(specific_vars_at_arrival, 0.85)
} else{
  splits <- initial_split(specific_vars_at_release, 0.85)
}


train_ngs <- training(splits)
test_ngs <- testing(splits)

yac_recipe <- recipe(yards_after_catch ~ ., data = train_ngs) %>%
  # Just this column has <10 NAs, breaking workflow
  step_knnimpute(own_avg_intensity, number_of_pass_rushers) %>%
  step_zv(all_predictors()) %>%
  step_num2factor(down, levels = c("1", "2", "3", "4")) %>%
  step_pca(n_cells, own_intensity, own_avg_intensity) %>%
  step_dummy(down, position_f, is_redzone)

yac_wflow <- workflow() %>%
  add_recipe(yac_recipe)

check <- yac_recipe %>%
  prep() %>%
  bake(train_ngs)
glimpse(check)


# Actual model fitting
set.seed(1312020)
ctrl_grid <- stacks::control_stack_grid()
folds <- rsample::vfold_cv(train_ngs, v = 10)

nnet_mod <- mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine("nnet") %>%
  set_mode("regression")

# Need to also normalize input for nnet
nnet_rec <- yac_recipe %>%
  step_normalize(all_predictors())

nnet_wflow <- yac_wflow %>%
  update_recipe(nnet_rec) %>%
  add_model(nnet_mod)

# If nnet all fail its likely because of NA in train set
nnet_res <- tune_grid(object = nnet_wflow,
                      resamples = folds,
                      grid = 10,
                      control = ctrl_grid)


# These YAC results are bad, review tomorrow
best_nnet <- nnet_res %>% select_best()

# Train the best model and check preds
updated_nnet <- nnet_wflow %>%
  finalize_workflow(best_nnet) %>%
  fit(train_ngs)

# I'll need to do better than this but for now its what Ive got
pred_check <- test_ngs %>%
  bind_cols(predict(updated_nnet, .))
pred_check %>%
  rmse(yards_after_catch, .pred)

pred_check %>%
  arrange(.pred) %>%
  mutate(bin = floor((row_number() - 1) / n() * 10)) %>%
  group_by(bin) %>%
  summarize(avg_yac = mean(yards_after_catch),
            avg_pred_yac = mean(.pred),
            n = n()) %>%
  ggplot(aes(x = avg_pred_yac, y = avg_yac)) +
  geom_point(size = 4) +
  geom_abline(intercept = 0, slope = 1, col = "red", lty = 2) +
  ylim(c(0, 10)) +
  xlab("Average Predicted YAC") + ylab("Average Observed YAC") +
  ggtitle("YAC Calibration Plot") +
  theme_bw() +
  theme(legend.position = "none")

# Now to build the model over all observed
full_nnet_model <- nnet_wflow %>%
  finalize_workflow(best_nnet) %>%
  fit(specific_vars_at_release)


# Load the all frames covariates
yac_estimates <- readRDS('Data/release/yac_covariates.rds') %>%
  unnest(yac_estimate) %>% 
  mutate(nfl_id = elig_receivers) %>%
  dplyr::select(-elig_receivers, -yac_estimate)

# I think the loop below is just handled by joining on all_covariates_and_all_preds and predict
all_covariates_preds_and_yac <- readRDS("Data/release/all_covariates_and_all_preds.rds") %>%
  left_join(yac_estimates, by = c("game_id", "play_id",
                                  "frame_id_2" = "frame_id", "nfl_id"))

# Make predictions now, adjust any NAs to 0 if they come up
all_covariates_preds_and_yac <- all_covariates_preds_and_yac %>%
  bind_cols(predict(full_nnet_model, .))

all_covariates_preds_and_yac %>%
  rename(pred_yac = .pred) %>%
  saveRDS("Data/release/all_preds_and_covariates_complete.rds")

