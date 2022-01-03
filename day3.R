library(tidyverse)

# day 3
day3 <- read_table("~/Downloads/input3.txt", 
                   col_names = c("value"))

# p1
counts <- day3 %>%
  mutate(digits = str_split(value, "")) %>%
  unnest_longer(digits) %>% 
  group_by(value) %>% 
  mutate(spot = 1:str_length(value)) %>% 
  ungroup() %>% 
  count(digits, spot)

gamm <- counts %>%
  group_by(spot) %>%
  slice_max(n) %>%
  mutate(digits = as.numeric(digits)) %>%
  ungroup() %>%
  summarize(gamm = sum(digits * 2 ^ (spot - 1))) %>% 
  pull(gamm)

eps <- counts %>%
  group_by(spot) %>%
  slice_min(n) %>%
  mutate(digits = as.numeric(digits)) %>%
  ungroup() %>%
  summarize(eps = sum(digits * 2 ^ (spot - 1))) %>% 
  pull(eps)

gamm * eps

# p2
