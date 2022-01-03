library(tidyverse)

# day 1
day1 <- read_table("Downloads/input1.txt", 
                   col_names = c("value"))

# p1
day1 %>% 
  mutate(lag_value = lag(value)) %>% 
  drop_na() %>% 
  summarize(answ = sum(value > lag_value))

# p2
day1 %>% 
  mutate(next1 = c(NA, day1$value[-nrow(day1)]),
         next2 = c(NA, NA, day1$value[-(nrow(day1):(nrow(day1) - 1))])) %>%
  mutate(total = value + next1 + next2) %>% 
  mutate(lag_total = lag(total)) %>% 
  drop_na() %>% 
  summarize(answ = sum(total > lag_total))
