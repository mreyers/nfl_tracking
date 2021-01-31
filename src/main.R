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
library(futile.logger)
flog.appender(appender.file('logs/main_log.log'), 'main')
flog.info('Welcome to QB Decision Making with Tracking Data!
          This script controls all of the values generated in
          the thesis defended in August of 2020. Changes will
          be made as this continues to be updated.', name = 'main')

flog.info('Loading packages. A few gotchas might exist for 4.0.x users
          with future, furrr, dtwclust, and glmnet. Be sure to reinstall
          codetools, survival, class, stringi, and cluster.
          If problem persists,
          probably more missing dependencies.', name = 'main')

# Task 0: Package management
# Load Packages
pacman::p_load(gganimate, cowplot, gifski, bezier, magrittr, 
               funHDDC,  broom, ggridges, mvtnorm, scales,
               data.table, snakecase, tictoc, glue,
               tidyverse, future, furrr, dtwclust, glmnet)

#remotes::install_github("tidymodels/discrim")
#remotes::install_github("tidymodels/stacks", ref = "main")
library(tidymodels)
library(stacks)
library(discrim)

flog.info('Loading helper functions', name = 'main')
flog.info('Some of these functions are still hardcoded to original dataset.
           General updates are in progress for robustness.', name = 'main')
source('utils/tracking_helpers.R')

flog.info('Beginning of the condensed QB evaluation. Running on the first 6 weeks of the 2017-2018 NFL
          season.', name = 'main')
flog.info('Note that some of the inefficiencies in data loading, such as looping
          and iterating, are a relic of 8GB of RAM. Whenever you build your PC
          next, do me a favour and get more RAM than you think you need.',
          name = 'main')

flog.info('Since BDB3 is a thing allow for an option to manipulate incoming data
           to work with old tracking framework or new tracking framework. Also
           allow for user to switch between at time of throw and at time of arrival.',
           name = 'main')

# Specify the task of interest. Thesis settings are specified beside as default
new_age_tracking_data <- FALSE # Default FALSE
time_of_arrival <- FALSE # Default FALSE
time_of_arrival_explicit <- if_else(time_of_arrival, "arrival", "release")
seasons <- 2017 # Default 2017
epsilon <- 5 # Default 5 for paper, 0 for standard play up to pass release

# # # # # # # #
# Task 0: Set up global parameters to be called from additional scripts
source("scripts/setup.R")

# Test with 1 file
file_list <- "tracking_gameId_2017090700.csv"
# # # # # # # # # # # # # # # # # #
# Task 1: Generate the covariates necessary to calculate probability of catching the ball
# Load the games, players, and list of plays in the data set
# This can all be done via the parallel_observed.R script. Data will be written, as desired
source('scripts/parallel_observed_new.R') # Need to replace pocket distance calculation with pocket_fixed()
flog.info('Completed parallel_observed.R. Onto building completion probability.', name = 'main')
# Still need to check for 2017 data but not urgent currently

# # # # # # # # # # # # # # # # # #
# Task 2: Use the covariates to calculate P(Catch) 
source('scripts/tidy_completion_prob.R') 
flog.info('Completed tidy_build_completion_probability.R
           Onto building frame by frame covariates.', name = 'main')

  # Task 2b): check diagnostics for the trained model
  source("scripts/basic_comp_prob_diagnostics.R")

# # # # # # # # # # # # # # # # # #
# Task 3: Generate frame by frame covariates necessary to apply the model
all_frames <- TRUE # Default TRUE
if(all_frames){
  source('utils/all_time_points_decisions.R') 
} else{
  source("utils/important_time_points.R")
}

flog.info("These additional functions handle air yards, dealt with elsewhere.",
          name = "main")
source('scripts/additional_all_time_points_fns.R') 

# Check this output
flog.info("There are no sacks and runs to deal with in this data.", name = "main")
source('scripts/sacks_and_runs.R') # Deal with the covariates for plays that are sacks or runs

flog.info("There will be a greater need for interception probability shortly.
          Update to tidymodels infrastructure when integrated.",
          name = "main")

source('scripts/prob_incomplete.R')
source('scripts/yac_covariates.R')
flog.info('Completed building frame by frame covariates. Onto prediction.', name = 'main')

# Should be up to here at this point. Going to save everything and test with just
# 1 input file from 2017, then 1 from 2018.

# # # # # # # # # # # # # # # # # #
# Task 4: Predict frame by frame catch probability
source('scripts/predict_all_frames.R')
source('scripts/predict_all_frames_interceptions.R')
  # Need to now check blend_predictions()
  # Current issue is in_id must be a positive integer vector, probably messed up bc sample size
source('scripts/yac_predictions.R') # Not done yet, needs to predict all frames
  # Check YAC diagnostics, validate model
  source('scripts/yac_diagnostics.R') # Need to test 
flog.info('Completed predict_all_frames.R. Onto QB Evaluation step. Next step is not yet documented.', name = 'main')

# # # # # # # # # # # # # # # # # #
# Task 5: Summarize QB Performance
source('scripts/redo_ep.R')
  # Refactor of nflfastR EP seems good!

# Honestly should probably just completely overhaul this one
# Perhaps address after refactor of some other code, not urgent
source('qb_eval_step.R') # Figure out why this is breaking, clearly relevant to nflscrapR
# and the changes to the NFL API. Problem is in wp_function(), seems fixable
flog.info('Analysis complete. Relevant objects are in this R environment or in %s.', getwd(), name = 'main')

# # # # # # # # # # # # # # # # # #
# Task 6: Additional Comparison 
source('extended_comparison.R')