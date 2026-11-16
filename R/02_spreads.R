library(tidyverse)

rba <- read_csv("data/rba_snapshot.csv",
                col_types = cols(date = col_date(), .default = col_double()))

## 1. Fill the May-Aug 2013 gap in the 2/3/5-year government yields.
# Interpolate the gap between the 10-year yield and each shorter yield
# in a straight line, then subtract it from the 10-year yield.
fill_gap <- function(short, long) {
  slope <- long - short
  ok <- !is.na(slope)
  slope_filled <- approx(which(ok), slope[ok], xout = seq_along(slope))$y
  if_else(is.na(short), long - slope_filled, short)
}

rba <- rba %>%
  mutate(cgs_2y = fill_gap(cgs_2y, cgs_10y),
         cgs_3y = fill_gap(cgs_3y, cgs_10y),
         cgs_5y = fill_gap(cgs_5y, cgs_10y))

## 2. Government yield at any maturity: straight line between the
# 2, 3, 5 and 10-year points, held flat outside that range.
cgs_at_tenor <- function(tenor, g2, g3, g5, g10) {
  approx(c(2, 3, 5, 10), c(g2, g3, g5, g10), xout = tenor, rule = 2)$y
}

## 3. BBB 5-year spread in basis points
rba <- rba %>%
  mutate(cgs_matched = pmap_dbl(list(bbb_5y_tenor, cgs_2y, cgs_3y, cgs_5y, cgs_10y), cgs_at_tenor),
  bbb5_spread_bp = (bbb_5y_yield - cgs_matched) * 100
  )

summary(rba$bbb5_spread_bp)

ggplot(rba, aes(date, bbb5_spread_bp)) +
  geom_line() +
  labs(x = NULL, y = "BBB 5-year spread (bp)")