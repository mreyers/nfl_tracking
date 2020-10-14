# This is the script to build a completion probability model based on
# the observed data. Ideally this would be done over a period of
# tracking data that is not of interest to observe,
# e.g. over 2017 season for training and then used to infer things
# about the 2018 season. Alas I do not have this data.

# Anyways, consider that although this generates decent out of sample
# performance that there is a small tendency to overfit towards the
# observed outcomes in the training sample.

# Load libraries not already loaded in main script
flog.appender(appender.file('logs/build_completion_probability.log'), 'comp_prob')
flog.info('Building h2o based model for the observed covariates', name = 'comp_prob')
library(h2o)

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
  mutate(set =sample(c('train', 'test'),
                     size = n(), replace = TRUE, prob = c(0.85, 0.15))) %>%
  ungroup() %>%
  mutate(pass_result = if_else(pass_result %in% 'C', 'C', 'I'),
         pass_result_f = factor(pass_result, levels = c("C", "I")))
rm(ngs_features)

# Partition data for training/testing
ngs_train <- new_features %>%
  filter(set %in% 'train')

ngs_test <- new_features %>%
  filter(set %in% 'test')


flog.info('Data parsed into train/test/validate for model building', name = 'comp_prob')

# Initialize h2o environment
h2o::h2o.init(max_mem_size = '6G')

h2o_train <- ngs_train %>%
  h2o::as.h2o()

h2o_test <- ngs_test %>%
  h2o::as.h2o()

x_2 <- c(# NGS Features
  'air_dist', 'rec_separation', 'sideline_sep', 'no_frame_rush_sep', 
  'qb_vel', 'time_to_throw', 'dist_from_pocket', 
  # Ownership Features, only used 1 due to colinearity
  'n_cells_at_throw',
  'own_intensity_at_throw', 
  'own_avg_intensity_at_throw',
  # Additional features that differ slightly
  'air_yards_x',
  # Context Features
  'yardline_100', 'down', 'ydstogo')

y <- 'pass_result_f'


flog.info('Random Forest section. Training 4 fixed models for ensemble',
          name = 'comp_prob')

rf_1 <- h2o.randomForest(x = x_2,
                         y = y, 
                         training_frame = h2o_train,
                         nfolds = 10,
                         keep_cross_validation_predictions = TRUE,
                         fold_assignment = 'modulo',
                         balance_classes = FALSE,
                         ntrees = 500,
                         max_depth = 5,
                         min_rows = 10,
                         seed = 1312020)

rf_2 <- h2o.randomForest(x = x_2,
                         y = y, 
                         training_frame = h2o_train,
                         nfolds = 10,
                         keep_cross_validation_predictions = TRUE,
                         fold_assignment = 'Modulo',
                         balance_classes = TRUE,
                         ntrees = 100,
                         max_depth = 10,
                         min_rows = 10,
                         seed = 1312020)

# Can I define more complex, better ensembles?
rf_3 <- h2o.randomForest(x = x_2,
                         y = y, 
                         training_frame = h2o_train,
                         nfolds = 10,
                         keep_cross_validation_predictions = TRUE,
                         fold_assignment = 'Modulo',
                         balance_classes = TRUE,
                         ntrees = 1000,
                         max_depth = 10,
                         min_rows = 15,
                         seed = 1312020)

rf_4 <- h2o.randomForest(x = x_2,
                         y = y, 
                         training_frame = h2o_train,
                         nfolds = 10,
                         keep_cross_validation_predictions = TRUE,
                         fold_assignment = 'Modulo',
                         balance_classes = FALSE,
                         ntrees = 5000,
                         max_depth = 3,
                         min_rows = 25,
                         seed = 1312020)

flog.info('GLM section. Training the two fixed GLMs used for this which
          are really just ridge regressions',
          name = 'comp_prob')

glm_2 <- h2o.glm(x = x_2,
                 y = y, 
                 training_frame = h2o_train,
                 nfolds = 10,
                 keep_cross_validation_predictions = TRUE,
                 fold_assignment = 'Modulo',
                 balance_classes = TRUE,
                 alpha = 0,
                 lambda_search = TRUE,
                 remove_collinear_columns = TRUE,
                 family = 'binomial',
                 seed = 1312020)

# With Ridge I can manage collinearity, dont need to remove ahead of time
glm_3 <- h2o.glm(x = x_2,
                 y = y, 
                 training_frame = h2o_train,
                 nfolds = 10,
                 keep_cross_validation_predictions = TRUE,
                 fold_assignment = 'Modulo',
                 balance_classes = TRUE,
                 alpha = 0,
                 lambda_search = TRUE,
                 family = 'binomial',
                 seed = 1312020)

flog.info('GBM section. Training the 3 GBMs used in ensemble.',
          name = 'comp_prob')
gbm_2 <- h2o.gbm(x = x_2,
                 y = y, 
                 training_frame = h2o_train,
                 nfolds = 10,
                 keep_cross_validation_predictions = TRUE,
                 fold_assignment = 'Modulo',
                 balance_classes = FALSE,
                 max_after_balance_size = 5,
                 max_hit_ratio_k = 0,
                 ntrees = 50,
                 max_depth = 5,
                 min_rows = 10,
                 nbins = 20,
                 distribution = 'bernoulli',
                 seed = 1312020)

gbm_3 <- h2o.gbm(x = x_2,
                 y = y,
                 training_frame = h2o_train,
                 nfolds = 10,
                 keep_cross_validation_predictions = TRUE,
                 fold_assignment = 'Modulo',
                 balance_classes = FALSE,
                 max_after_balance_size = 10,
                 max_hit_ratio_k = 0,
                 ntrees = 500,
                 max_depth = 10,
                 min_rows = 5,
                 nbins = 30,
                 distribution = 'bernoulli',
                 seed = 1312020)

gbm_4 <- h2o.gbm(x = x_2,
                 y = y,
                 training_frame = h2o_train,
                 nfolds = 10,
                 keep_cross_validation_predictions = TRUE,
                 fold_assignment = 'Modulo',
                 balance_classes = FALSE,
                 max_after_balance_size = 5,
                 max_hit_ratio_k = 0,
                 ntrees = 100,
                 max_depth = 5,
                 min_rows = 30,
                 nbins = 30,
                 distribution = 'bernoulli',
                 seed = 1312020)

gbm_5 <- h2o.gbm(x = x_2,
                 y = y,
                 training_frame = h2o_train,
                 nfolds = 10,
                 keep_cross_validation_predictions = TRUE,
                 fold_assignment = 'Modulo',
                 balance_classes = FALSE,
                 max_after_balance_size = 5,
                 max_hit_ratio_k = 0,
                 ntrees = 1000,
                 max_depth = 10,
                 min_rows = 30,
                 nbins = 30,
                 distribution = 'bernoulli',
                 seed = 1312020)

# Neural net
flog.info('Neural Net section. Included to make sure this qualifies for Sloan.',
          name = 'comp_prob')
nn_1 <- h2o.deeplearning(x = x_2,
                         y = y, 
                         training_frame = h2o_train,
                         nfolds = 10,
                         keep_cross_validation_predictions = TRUE,
                         fold_assignment = 'Modulo',
                         hidden = c(100, 100, 100),
                         epochs = 100,
                         distribution = 'bernoulli',
                         input_dropout_ratio = 0.1,
                         hidden_dropout_ratios = c(0.3, 0.3, 0.3),
                         activation = 'RectifierWithDropout',
                         seed = 1312020)

nn_2 <- h2o.deeplearning(x = x_2,
                         y = y, 
                         training_frame = h2o_train,
                         nfolds = 10,
                         keep_cross_validation_predictions = TRUE,
                         fold_assignment = 'Modulo',
                         hidden = c(100, 100),
                         epochs = 100,
                         distribution = 'bernoulli',
                         input_dropout_ratio = 0.2,
                         hidden_dropout_ratios = c(0.5, 0.5),
                         activation = 'RectifierWithDropout',
                         seed = 1312020)

nn_3 <- h2o.deeplearning(x = x_2,
                         y = y, 
                         training_frame = h2o_train,
                         nfolds = 10,
                         keep_cross_validation_predictions = TRUE,
                         fold_assignment = 'Modulo',
                         hidden = c(100, 100, 100, 100),
                         epochs = 100,
                         distribution = 'bernoulli',
                         input_dropout_ratio = 0.2,
                         hidden_dropout_ratios = c(0.5, 0.5, 0.5, 0.5),
                         activation = 'RectifierWithDropout',
                         seed = 1312020)

flog.info('Naive Bayes model.', name = 'comp_prob')
nb_1 <- h2o.naiveBayes(x = x_2,
                       y = y, 
                       training_frame = h2o_train,
                       nfolds = 10,
                       keep_cross_validation_predictions = TRUE,
                       fold_assignment = 'Modulo',
                       balance_classes = FALSE,
                       seed = 1312020)

# Current complete ensemble
general_ensemble <- h2o.stackedEnsemble(x = x_2,
                                          y = y,
                                          training_frame = h2o_train,
                                          base_models = list(rf_1, rf_2, rf_3, rf_4,
                                                             glm_2, glm_3,
                                                             gbm_2, gbm_3, gbm_4, gbm_5,
                                                             nn_1, nn_2, nn_3,
                                                             nb_1),
                                          metalearner_algorithm = 'gbm',
                                          seed = 1312020)

models_list <- list(rf_1, rf_2, rf_3, rf_4,
                    glm_2, glm_3,
                    gbm_2, gbm_3, gbm_4,
                    nn_1, nn_2, nn_3,
                    nb_1, general_ensemble)

# Expectation is ~75% accuracy on BDB1 data
accuracy_calc <- function(model){
  return(mean(as.data.frame(h2o::h2o.predict(model, newdata = h2o_test))$predict ==
                ngs_test$pass_result))
}

flog.info('Model accuracy checks. Ideally ensemble is most accurate
          though it may be possible that a neural net is more accurate.
          Some minor randomness things make this feasible. This step
          will just select the most accurate completion probability model
          as the working model for the given script run.', name = 'comp_prob')

#holder <- lapply(models_list, accuracy_calc)
lapply(models_list, accuracy_calc)


# Consider doing in tidymodels instead
best_model %>% saveRDS('completion_probability.rds')