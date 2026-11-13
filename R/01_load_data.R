library(tidyverse)

rba <- read_csv("data/rba_snapshot.csv")
glimpse(rba)   # should be 261 rows, one per month

# First look: BBB 5-year corporate yield vs 5-year government yield
rba %>%
  select(date, bbb_5y_yield, cgs_5y) %>%
  pivot_longer(-date, names_to = "series", values_to = "yield") %>%
  ggplot(aes(date, yield, colour = series)) +
  geom_line()