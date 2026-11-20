# 08_statistics.R: how strong is the evidence? Half-life and Newey-West t-statistics
library(tidyverse)
source("R/07_test.R")   # reruns steps 3-7: bbb5, test (with net_bp, xr_bp, active_bp)

# --- 1. Mean reversion: AR(1) half-life of the spread ---
# Regress this month's spread on last month's: spread_t = a + phi * spread_(t-1).
# phi close to 1 means slow reversion. Half-life = months for a gap from the
# average to halve.
ar1_half_life <- function(spread) {
  x <- head(spread, -1)
  y <- tail(spread, -1)
  phi <- cov(x, y) / var(x)
  tibble(ar1 = phi, half_life_months = log(0.5) / log(phi))
}

bind_rows(
  full     = ar1_half_life(bbb5$spread_bp),
  training = bbb5 %>%
    filter(date >= as.Date("2008-01-31"), date <= as.Date("2015-12-31")) %>%
    pull(spread_bp) %>% ar1_half_life(),
  test     = bbb5 %>%
    filter(date >= as.Date("2016-01-31"), date <= as.Date("2026-09-30")) %>%
    pull(spread_bp) %>% ar1_half_life(),
  .id = "period"
)

# --- 2. Newey-West t-statistics ---
# A plain t-statistic assumes each month is independent. If returns are
# correlated from month to month, it overstates the evidence. Newey-West
# widens the standard error to allow for correlation over the last `lags` months.
newey_west_t <- function(x, lags = 3) {
  n <- length(x)
  e <- x - mean(x)
  s <- sum(e^2) / n
  for (l in 1:lags) {
    s <- s + 2 * (1 - l / (lags + 1)) * sum(e[(l + 1):n] * e[1:(n - l)]) / n
  }
  mean(x) / sqrt(s / n)
}

simple_t <- function(x) mean(x) / (sd(x) / sqrt(length(x)))

# Test period (2016-26)
tibble(
  strategy = c("Rule (net)", "Buy-and-hold", "Timing value"),
  simple_t = c(simple_t(test$net_bp), simple_t(test$xr_bp), simple_t(test$active_bp)),
  nw_t     = c(newey_west_t(test$net_bp), newey_west_t(test$xr_bp), newey_west_t(test$active_bp))
)