# 06_grid_search.R: choose the window (L) and threshold (k) on the training period
library(tidyverse)
source("R/05_signal_rule.R")   # reruns steps 3-5: panel, run_rule(), rule_perf()

bbb5 <- panel %>% filter(series == "bbb_5y")

# Run the rule for every combination of L and k, scored on training only
train_grid <- expand_grid(L = c(12, 24, 36), k = c(0.5, 1, 1.5)) %>%
  mutate(results = map2(L, k, function(L, k) {
    bbb5 %>%
      run_rule(L = L, k = k) %>%
      filter(date >= as.Date("2008-01-31"), date <= as.Date("2015-12-31")) %>%
      rule_perf()
  })) %>%
  unnest(results)

train_grid %>% arrange(desc(sharpe))

# Pick the setting with the highest training Sharpe ratio
best <- train_grid %>% slice_max(sharpe, n = 1)
best

# Heatmap of training Sharpe ratios (blue = positive, red = negative)
train_grid %>%
  ggplot(aes(factor(k), factor(L), fill = sharpe)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(sharpe, 2))) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  labs(x = "Threshold k (sd)", y = "Lookback L (months)", fill = "Sharpe",
       title = "Training Sharpe ratio (2008-15), net of costs")