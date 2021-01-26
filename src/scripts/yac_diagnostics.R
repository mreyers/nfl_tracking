# YAC diagnostics
yac_stack <- readRDS(glue("{default_path}{time_of_arrival_explicit}/yac_stack_model.rds"))
test_ngs <- readRDS(glue("{default_path}{time_of_arrival_explicit}/yac_test_set.rds"))

test_set_preds <- predict(yac_stack, test_ngs)$.pred



test_ngs %>%
  mutate(preds = test_set_preds) %>%
  filter(yards_after_catch >= -1) %>%
  ggplot(aes(x = yards_after_catch, y = preds)) +
  geom_point() +
  geom_smooth() + 
  #geom_abline(slope = 1, intercept = 0, col = "red", lty = 2) +
  ggtitle("Scatter of Predicted vs Observed YAC") +
  ylab("Predicted YAC") + xlab("Observed YAC") +
  theme_bw()

ngs_test %>%
  mutate(preds = test_set_preds,
         absolute_gap = abs(yards_after_catch - preds),
         within_1 = (absolute_gap < 1),
         within_3 = (absolute_gap < 3),
         within_5 = (absolute_gap < 5)) %>%
  summarize(prop_within_1 = mean(within_1),
            prop_within_3 = mean(within_3),
            prop_within_5 = mean(within_5))