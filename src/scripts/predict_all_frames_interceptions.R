# Now to add the predictions for interceptions to the frames
flog.info('Add interception probabilities to the original frame and write results.', name = 'inc')
all_frames <- readRDS('Data/release/all_covariates_preds_and_ids.rds')
inc_model <- readRDS(glue::glue("{default_path}{time_of_arrival_explicit}/int_prob.rds"))

# Predict on all_frames with the model
all_frames_preds <- predict(inc_model, all_frames, type = "prob") %>%
  select(.pred_IN)

# All together
all_frames_both_preds <- all_frames %>%
  bind_cols(all_frames_preds)

all_frames_with_all_preds <- all_frames_both_preds %>%
  mutate(completion_prob_pred = .pred_C,
         interception_prob_pred = (1 - completion_prob_pred) * .pred_IN,
         incompletion_prob_pred = 1 - (interception_prob_pred + completion_prob_pred))

flog.info('Writing results', name = 'inc')

all_frames_with_all_preds %>%
  saveRDS(glue::glue("{default_path}{type}/all_covariates_and_all_preds.rds"))
