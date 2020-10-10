# Full Refactor of QB evaluation
# Set all code up in a fully reproducible manner
# This means removing hardcoded game filtering and replacing with problem based filtering
# Also fix up comments, improve parallelization, and adjust utils files
# Some of this was previously held together by duct tape and super glue
# Time to remedy that
# Perhaps a dockerfile build?
# Likely unnecessary, at least for now


# # # # # # # # # # # # # # # # # #
setwd("src")

# Start the log
flog.appender(appender.file('logs/main_log.log'), 'main')
flog.info('Welcome to QB Decision Making with Tracking Data!
          This script controls all of the values generated in
          the thesis defended in August of 2020. Changes will
          be made as this continues to be updated.', name = 'main')

flog.info('Loading packages. A few gotchas might exist for 4.0.x users
          with future, furrr, dtwclust, and glmnet. Be sure to reinstall
          codetools, survival, class, and cluster. If problem persists,
          probably more missing dependencies.', name = 'main')

# Task 0: Package management
# Load Packages
pacman::p_load(gganimate, cowplot, gifski, bezier, magrittr, 
               funHDDC,  broom, ggridges, mvtnorm, scales,
               data.table, snakecase, tictoc, futile.logger, glue,
               tidyverse, nflscrapR, future, furrr, dtwclust, glmnet)

flog.info('Loading helper functions', name = 'main')
flog.info('Some of these functions are still hardcoded to original dataset.
           General updates are in progress for robustness.', name = 'main')
source('utils/tracking_helpers.R')

flog.info('Beginning of the condensed QB evaluation. Running on the first 6 weeks of the 2017-2018 NFL
          season.', name = 'main')

# # # # # # # # # # # # # # # # # #
# Task 1: Generate the covariates necessary to calculate probability of catching the ball
# Load the games, players, and list of plays in the data set
# This can all be done via the parallel_observed.R script. Data will be written, as desired
source('parallel_observed.R') # Need to replace pocket distance calculation with pocket_fixed()
flog.info('Completed parallel_observed.R. Onto building completion probability.', name = 'main')

# # # # # # # # # # # # # # # # # #
# Task 2: Use the covariates to calculate P(Catch) 
source('build_completion_probability.R') # Still roughly 70% with corrections to covariate
flog.info('Completed build_completion_probability.R. Onto building frame by frame covariates.', name = 'main')

# # # # # # # # # # # # # # # # # #
# Task 3: Generate frame by frame covariates necessary to apply the model
source('All_time_points_decisions.R') 
source('additional_all_time_points_fns.R') 
source('sacks_and_runs.R') # Deal with the covariates for plays that are sacks or runs
source('prob_incomplete.R') 
source('yac_covariates.R')
flog.info('Completed building frame by frame covariates. Onto prediction.', name = 'main')

# # # # # # # # # # # # # # # # # #
# Task 4: Predict frame by frame catch probability
source('predict_all_frames.R')
source('predict_all_frames_interceptions.R')
source('yac_predictions.R')
flog.info('Completed predict_all_frames.R. Onto QB Evaluation step. Next step is not yet documented.', name = 'main')

# # # # # # # # # # # # # # # # # #
# Task 5: Summarize QB Performance
source('redo_ep.R')
source('qb_eval_step.R') # Figure out why this is breaking, clearly relevant to nflscrapR
# and the changes to the NFL API. Problem is in wp_function(), seems fixable
flog.info('Analysis complete. Relevant objects are in this R environment or in %s.', getwd(), name = 'main')

# # # # # # # # # # # # # # # # # #
# Task 6: Additional Comparison 
source('extended_comparison.R')