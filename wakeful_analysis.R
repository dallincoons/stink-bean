pacman::p_load(tidyverse, lubridate, ggplot2)

source('./sleeping_analysis.R')

sleeping <- createSleepingDataset() %>% 
  mutate(
    mil_start_time = format(strptime(start_time_am_pm, "%I:%M:%S %p"), "%H:%M:%S"),
    mil_end_time = format(strptime(end_time_am_pm, "%I:%M:%S %p"), "%H:%M:%S")
  ) %>% 
  arrange(date, mil_start_time) %>% 
  mutate(hours_since_last_sleep = 
           (as.POSIXct(strptime(start_time_am_pm, "%I:%M:%S %p"), tz="") -
            as.POSIXct(strptime(lag(end_time_am_pm), "%I:%M:%S %p"), tz="")) / 60) %>% 
  group_by(date) %>% 
  mutate(hours_since_last_sleep = case_when(
    hours_since_last_sleep < 0 ~ hours_since_last_sleep + 24,
    TRUE ~ hours_since_last_sleep
  )) %>% 
  mutate(hours_since_last_sleep_rounded = round(hours_since_last_sleep))

sleeping[is.na(sleeping)] <- 0

sleeping %>% 
  filter(date > '2019-12-20') %>% 
  group_by(date) %>% 
  ggplot() +
    geom_bar(aes(x = hours_since_last_sleep_rounded)) +
    facet_grid(date ~ .)

