# 04_returns.R: duration and monthly excess returns for all 8 series
library(tidyverse)
source("R/03_panel.R")   # reruns step 3 to build `panel`

# Modified duration of a bond priced at par with semi-annual coupons:
# how much its price moves (in %) for a 1% change in yield
par_duration <- function(yield_pct, tenor) {
  y <- yield_pct / 100
  (1 - (1 + y / 2)^(-2 * tenor)) / y
}

# Monthly excess return over government bonds, in bp:
#   one month of spread income (last month's spread / 12)
#   minus duration x change in spread (a price loss if spreads widen)
panel <- panel %>%
  mutate(duration = par_duration(yield, tenor)) %>%
  group_by(series) %>%
  mutate(xr_bp = lag(spread_bp) / 12 -
           lag(duration) * (spread_bp - lag(spread_bp))) %>%
  ungroup()

# Summary: annualised return, volatility and Sharpe ratio
perf <- function(d) {
  d %>% summarise(n = n(),
                  ann_return_bp = mean(xr_bp) * 12,
                  ann_vol_bp    = sd(xr_bp) * sqrt(12),
                  sharpe        = ann_return_bp / ann_vol_bp)
}

bbb5 <- panel %>% filter(series == "bbb_5y")

bbb5 %>% filter(date >= as.Date("2008-01-31"), date <= as.Date("2015-12-31")) %>% perf()  # training
bbb5 %>% filter(date >= as.Date("2016-01-31"), date <= as.Date("2026-09-30")) %>% perf()  # testing

# Cumulative excess return from always holding BBB 5-year credit
bbb5 %>%
  filter(!is.na(xr_bp)) %>%
  mutate(cum_bp = cumsum(xr_bp)) %>%
  ggplot(aes(date, cum_bp)) +
  geom_line() +
  labs(x = NULL, y = "Cumulative excess return (bp)")