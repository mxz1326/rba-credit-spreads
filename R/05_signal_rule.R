# 05_signal_rule.R: z-score signal and long-only trading rule for BBB 5-year
library(tidyverse)
source("R/04_returns.R")   # reruns steps 3-4 to build `panel` with xr_bp

# Rolling z-score: how many standard deviations this month's spread is
# above its average over the last L months (this month included).
# Only uses data known at the end of each month.
roll_z <- function(x, L) {
  z <- rep(NA_real_, length(x))
  for (i in seq_along(x)) {
    if (i >= L) {
      w <- x[(i - L + 1):i]
      z[i] <- (x[i] - mean(w)) / sd(w)
    }
  }
  z
}

# Trading rule for one series:
#   signal: at each month end, 1 (hold credit) if z > k, otherwise 0
#   held:   the position is held over the NEXT month, to avoid look-ahead bias
#   trade:  1 in the months the position changes
#   cost:   4bp of spread x duration per trade
run_rule <- function(d, L, k, cost = 4) {
  d %>%
    arrange(date) %>%
    mutate(z        = roll_z(spread_bp, L),
           signal   = if_else(!is.na(z) & z > k, 1, 0),
           held     = lag(signal, default = 0),
           trade    = abs(held - lag(held, default = 0)),
           cost_bp  = cost * coalesce(lag(duration), 0) * trade,
           gross_bp = held * xr_bp,
           net_bp   = gross_bp - cost_bp)
}

# Summary for the rule, net of costs
rule_perf <- function(bt) {
  bt %>% summarise(n               = n(),
                   ann_return_bp   = mean(net_bp) * 12,
                   ann_vol_bp      = sd(net_bp) * sqrt(12),
                   sharpe          = ann_return_bp / ann_vol_bp,
                   pct_in_credit   = mean(held),
                   trades_per_year = mean(trade) * 12)
}

# Starting settings: 3-year window, enter when the spread is 0.5 sd above average
bbb5_rule <- panel %>%
  filter(series == "bbb_5y") %>%
  run_rule(L = 36, k = 0.5)

# Check the first trade: z starts in Dec 2007 (month 36), position starts Jan 2008
bbb5_rule %>%
  filter(date >= as.Date("2007-10-31"), date <= as.Date("2008-02-29")) %>%
  select(date, spread_bp, z, signal, held, trade, net_bp)

# Training period only
train <- bbb5_rule %>% filter(date >= as.Date("2008-01-31"), date <= as.Date("2015-12-31"))
rule_perf(train)

# Rule vs always holding credit, training period
train %>%
  mutate(rule = cumsum(net_bp), buy_and_hold = cumsum(xr_bp)) %>%
  select(date, rule, buy_and_hold) %>%
  pivot_longer(-date, names_to = "strategy", values_to = "cum_bp") %>%
  ggplot(aes(date, cum_bp, colour = strategy)) +
  geom_line() +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  labs(x = NULL, y = "Cumulative excess return (bp)", colour = NULL)