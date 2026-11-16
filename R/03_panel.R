library(tidyverse)

# Same helper functions as 02_spreads.R
fill_gap <- function(short, long) {
  slope <- long - short
  ok <- !is.na(slope)
  slope_filled <- approx(which(ok), slope[ok], xout = seq_along(slope))$y
  if_else(is.na(short), long - slope_filled, short)
}

cgs_at_tenor <- function(tenor, g2, g3, g5, g10) {
  approx(c(2, 3, 5, 10), c(g2, g3, g5, g10), xout = tenor, rule = 2)$y
}

rba <- read_csv("data/rba_snapshot.csv",
                col_types = cols(date = col_date(), .default = col_double())) %>%
  mutate(cgs_2y = fill_gap(cgs_2y, cgs_10y),
         cgs_3y = fill_gap(cgs_3y, cgs_10y),
         cgs_5y = fill_gap(cgs_5y, cgs_10y))

# One row per month per series (A/BBB at 3/5/7/10 years)
panel <- rba %>%
  pivot_longer(
    cols = matches("^(a|bbb)_\\d+y_(tenor|yield)$"),
    names_to = c("rating", "target", ".value"),
    names_pattern = "^(a|bbb)_(\\d+)y_(tenor|yield)$"
  ) %>%
  mutate(
    series = paste0(rating, "_", target, "y"),
    cgs_matched = pmap_dbl(list(tenor, cgs_2y, cgs_3y, cgs_5y, cgs_10y), cgs_at_tenor),
    spread_bp = (yield - cgs_matched) * 100
  ) %>%
  arrange(series, date)

nrow(panel)   # should be 2088 (8 series x 261 months)

panel %>%
  group_by(series) %>%
  summarise(mean_bp = mean(spread_bp), min_bp = min(spread_bp), max_bp = max(spread_bp))

panel %>%
  mutate(maturity = factor(paste0(target, "y"), levels = c("3y", "5y", "7y", "10y")),
         rating   = toupper(rating)) %>%
  ggplot(aes(date, spread_bp, colour = maturity)) +
  geom_line() +
  facet_wrap(~ rating, ncol = 1) +
  labs(x = NULL, y = "Spread (bp)", colour = "Maturity")