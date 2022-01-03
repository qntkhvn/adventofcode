library(tidyverse)

# day 2
day2 <- read_table("Downloads/input2.txt", 
                   col_names = c("direction", "value"))

# p1
day2 %>% 
  group_by(direction) %>% 
  summarize(total = sum(value)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = direction, 
              values_from = total) %>% 
  summarize(answ = forward * (down - up))

# p2
day2 <- day2 %>% 
  mutate(id = row_number())

updown <- day2 %>% 
  filter(direction != "forward") %>% 
  mutate(value = ifelse(direction == "up", -value, value),
         aim = cumsum(value))

aim <- tibble(id = 1:nrow(day2)) %>% 
  left_join(updown) %>% 
  fill(aim) %>% 
  mutate(aim = ifelse(is.na(aim), 0, aim)) %>% 
  pull(aim)

day2 %>% 
  mutate(aim = aim) %>% 
  filter(direction == "forward") %>% 
  mutate(depth = value * aim) %>% 
  summarize(answ = sum(value) * sum(depth))
