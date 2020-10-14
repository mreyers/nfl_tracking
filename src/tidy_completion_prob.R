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
  step_num2factor(down, levels = c("1", "2", "3", "4")) %>%
  step_normalize(all_numeric()) %>%
  step_pca(all_numeric()) %>%
  step_dummy(down)

# To run stacking, need stacks
# remotes::install_github("tidymodels/stacks", ref = "main")
library(stacks)
