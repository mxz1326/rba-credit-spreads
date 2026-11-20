# 09_all_series.R: does the result hold beyond BBB 5-year? All 8 series, and higher costs
library(tidyverse)
source("R/08_statistics.R")   # reruns steps 3-8: panel, bbb5, run_rule(), best, newey_west_t()

in_test <- function(d) d %>% filter(date >= as.Date("2016-01-31"), date <= as.Date("2026-09-30"))

# Test-period summary for one series, using the settings chosen on BBB 5-year training
test_summary <- function(d, cost = 4) {
  bt <- d %>% run_rule(L = best$L, k = best$k, cost = cost) %>% in_test()
  active <- bt$net_bp - mean(bt$held) * bt$xr_bp
  tibble(rule_return_bp = mean(bt$net_bp) * 12,
         rule_sharpe    = rule_return_bp / (sd(bt$net_bp) * sqrt(12)),
         in_credit      = mean(bt$held),
         bh_return_bp   = mean(bt$xr_bp) * 12,
         bh_sharpe      = bh_return_bp / (sd(bt$xr_bp) * sqrt(12)),
         timing_bp      = mean(active) * 12,
         timing_nw_t    = newey_west_t(active))
}

series_order <- c("a_3y", "a_5y", "a_7y", "a_10y", "bbb_3y", "bbb_5y", "bbb_7y", "bbb_10y")

# 1. All 8 series
all_series <- panel %>%
  group_by(series) %>%
  group_modify(~ test_summary(.x)) %>%
  ungroup() %>%
  arrange(factor(series, levels = series_order))
all_series

# 2. Cost sensitivity for BBB 5-year: 0, 4 (base case) and 8bp per trade
tibble(cost = c(0, 4, 8)) %>%
  mutate(results = map(cost, ~ test_summary(bbb5, cost = .x))) %>%
  unnest(results) %>%
  select(cost, rule_return_bp, rule_sharpe, timing_bp, timing_nw_t)

# 3. Timing value by series, labelled with its Newey-West t-statistic
all_series %>%
  mutate(series = factor(series, levels = series_order)) %>%
  ggplot(aes(series, timing_bp)) +
  geom_col() +
  geom_text(aes(label = paste0("t = ", round(timing_nw_t, 2))), vjust = -0.5) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  labs(x = NULL, y = "Timing value (bp a year)",
       title = "Test-period timing value by series (L = 36, k = 0.5)")