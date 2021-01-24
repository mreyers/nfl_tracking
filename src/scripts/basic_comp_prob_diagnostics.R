# Some model diagnostics to ensure the model trained appropriately
comp_prob_stack <- readRDS(glue("{default_path}{time_of_arrival_explicit}/comp_prob_with_ownership.rds"))

# Gut check
theme_set(theme_bw())
autoplot(comp_prob_stack)

# Actual test set predictions
comp_prob_test <- test_ngs %>%
  bind_cols(predict(comp_prob_stack, ., type = "prob")) %>%
  # Null model (64.6 percent of training observations are completions)
  # mutate(.pred_C = 0.646) %>%
  mutate(pred_pass_c = factor(if_else(.pred_C > 0.5, "C", "I"),
                              levels = c("C", "I")
  ))


# Accuracy check
print(accuracy(comp_prob_test,
         truth = pass_result_f,
         estimate = pred_pass_c))

# roc_auc = 0.763
# Now its 0.799 with recent modifications adding game context and target type
# 81.3 % on 2017 data with ownership
# Only 75% without ownership on 2017
# Only 74.4% with solely NGS covariates
print(roc_auc(comp_prob_test,
        truth = pass_result_f,
        contains(".pred_C")))

# Can probably do a bit better with touchups
# 0.49 log loss with 2017 data and ownership
# 0.564 log loss without ownership
print(mn_log_loss(comp_prob_test,
            truth = pass_result_f,
            contains(".pred_C")))

# Comparison of ensemble to the individual learners
comp_prob_all_test <- test_ngs %>%
  select(pass_result_f) %>%
  bind_cols(predict(comp_prob_stack, test_ngs, 
                    type = "prob", members = TRUE))

comp_prob_all_test

# Use the following to see accuracy of the stacked ensemble as well as components
  # Ideally the stacked ensemble (.pred_C) will have best performance
  # Occasionally possible this doesnt hold, minor discrepancy if that is the case
comp_prob_all_test %>%
  select(pass_result_f, contains(".pred_C")) %>%
  pivot_longer(cols = -pass_result_f, names_to = "model", values_to = "preds") %>%
  group_by(model) %>%
  roc_auc(truth = pass_result_f,
          preds) %>%
  arrange(desc(.estimate))

comp_prob_all_test %>%
  select(pass_result_f, contains(".pred_C")) %>%
  pivot_longer(cols = -pass_result_f, names_to = "model", values_to = "preds") %>%
  group_by(model) %>%
  mn_log_loss(truth = pass_result_f,
              preds) %>%
  arrange(.estimate)

# Calibration plot
# Acceptable calibration plot with 2017 ownership
# Awful calibration plot without accounting for ownership, way over predicting unlikely catches and under
# predicting likely ones
comp_prob_test %>%
  arrange(.pred_C) %>%
  mutate(bins = floor((row_number() - 1) / n() * 10)) %>%
  group_by(bins) %>%
  summarize(exp_cp = mean(.pred_C),
            obs_cp = mean(pass_result_f == "C"),
            n = n()) %>%
  ggplot(aes(x = exp_cp, y = obs_cp, size =3)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red", lty = 2) +
  ggtitle("Calibration of Completion Probability with Ownership") +
  xlab("Expected CP") + ylab("Observed CP") +
  theme_bw() +
  theme(legend.position = "none")

