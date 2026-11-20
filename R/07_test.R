# 07_test.R: apply the chosen setting to the test period (2016-26), once
library(tidyverse)
source("R/06_grid_search.R")   # reruns steps 3-6: run_rule(), rule_perf(), perf(), best

# Largest fall in cumulative return from its previous peak (the start counts as a peak)
max_dd <- function(r) {
  cum <- cumsum(r)
  min(cum - pmax(cummax(cum), 0))
}

# Settings come from `best`, chosen on training only
test <- bbb5 %>%
  run_rule(L = best$L, k = best$k) %>%
  filter(date >= as.Date("2016-01-31"), date <= as.Date("2026-09-30"))

# 1. The rule vs always holding credit
rule_perf(test) %>% mutate(max_dd_bp = max_dd(test$net_bp))   # rule
perf(test) %>% mutate(max_dd_bp = max_dd(test$xr_bp))         # buy-and-hold

# 2. Timing value: does the timing add anything beyond simply holding less credit?
#    Benchmark: a constant position equal to the rule's average position
avg_pos <- mean(test$held)
test <- test %>%
  mutate(constant_bp = avg_pos * xr_bp,
         active_bp   = net_bp - constant_bp)

test %>% summarise(avg_position = avg_pos,
                   timing_bp    = mean(active_bp) * 12,
                   timing_t     = mean(active_bp) / (sd(active_bp) / sqrt(n())))

# 3. Cumulative returns over the test period
test %>%
  mutate(rule = cumsum(net_bp),
         buy_and_hold = cumsum(xr_bp),
         constant_position = cumsum(constant_bp)) %>%
  select(date, rule, buy_and_hold, constant_position) %>%
  pivot_longer(-date, names_to = "strategy", values_to = "cum_bp") %>%
  ggplot(aes(date, cum_bp, colour = strategy)) +
  geom_line() +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  labs(x = NULL, y = "Cumulative excess return (bp)", colour = NULL,
       title = "Test period (2016-26): rule vs benchmarks")